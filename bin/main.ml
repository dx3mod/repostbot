open Lib
module Log = Dolog.Log

let get_env_vars () =
  try Project_env.capture ()
  with Not_found ->
    Log.error "Не найдены необходимые переменные окружения";
    exit 1

let envs = get_env_vars ()

module Vk_api =
  Vkashka.Api
    (Cohttp_lwt_unix.Client)
    ((val Vkashka.access_token envs.vk_token))

module Tg_bot = Telegram.Bot ((val Telegram.token envs.tg_token))

let filter_new_vk_records ~last_record_id records =
  List.filter (fun (r : Vkashka.Wall.Record.t) -> r.id > last_record_id) records
  |> List.rev

let filter_edited_records_and_posts ~posts records =
  List.filter_map
    (fun (post : Posts_cache.post) ->
      List.find_opt
        (fun (record : Vkashka.Wall.Record.t) ->
          record.id = post.vk_record_id
          && post.last_modify < Utils.last_date_vk_record record)
        records
      |> Option.map (fun r -> (r, post)))
    posts

let crop_text input =
  let input_length = String.length input - 1 in

  if input_length < Telegram.limit_message_chapters then input
  else
    let last_whitespace_index = ref 0 in

    for i = 0 to input_length do
      if i < Telegram.limit_message_chapters then
        if String.unsafe_get input i = ' ' then last_whitespace_index := i
    done;

    String.sub input 0 !last_whitespace_index

let repost (post : Vkashka.Wall.Record.t) =
  let attachments_to_string (attachments : Vkashka.Media.Attachment.t list) =
    String.concat ","
    @@ List.map
         (function
           | Vkashka.Media.Attachment.Photo _ -> "photo"
           | Vkashka.Media.Attachment.Video _ -> "video"
           | Vkashka.Media.Attachment.Other other -> other)
         attachments
  in

  let last_size xs = List.fold_left (fun _ x -> Some x) None xs in

  let%lwt message =
    Tg_bot.send_message ~chat_id:envs.targets.tg_chat_id
      (crop_text post.text ^ "\n\n" ^ attachments_to_string post.attachments)
  in

  let%lwt _ =
    let photo_urls =
      List.filter_map
        (function
          | Vkashka.Media.Attachment.Photo photo ->
              last_size photo.sizes
              |> Option.map (fun (size : Vkashka.Media.Photo.size) -> size.url)
          | Vkashka.Media.Attachment.Video video ->
              last_size video.preview
              |> Option.map (fun (img : Vkashka.Media.Video.image) -> img.url)
          | _ -> None)
        post.attachments
    in

    Tg_bot.send_photos ~chat_id:envs.targets.tg_chat_id photo_urls
  in

  Lwt.return message

let format_unix_time time =
  time |> float_of_int |> Unix.localtime |> fun t ->
  Printf.sprintf "%04d-%02d-%02d %02d:%02d" (t.tm_year + 1900) (t.tm_mon + 1)
    t.tm_mday t.tm_hour t.tm_min

let edit_post ~(post : Posts_cache.post) ~(record : Vkashka.Wall.Record.t) =
  let%lwt _ =
    Tg_bot.edit_message_text ~chat_id:envs.targets.tg_chat_id
      ~message_id:(string_of_int post.tg_message_id)
    @@ Printf.sprintf "%s\n\nedited at %s" (crop_text record.text)
         (format_unix_time @@ Utils.last_date_vk_record record)
  in
  Lwt.return_unit

let main () =
  let cache = Posts_cache.load_from_file envs.cache_file in

  Log.info "Получение постов со стены (пользователя %s)" envs.targets.vk_user;

  let%lwt vk_records =
    Vk_api.Wall.get (`Domain envs.targets.vk_user)
    |> Lwt.map (fun (resp : Vkashka.Wall.records) -> resp.items)
  in

  (* Repost new posts. *)
  let new_records =
    filter_new_vk_records ~last_record_id:cache.last_record_id vk_records
  in

  Log.debug "Начат процесс репостинга";

  let%lwt cache =
    Lwt_list.fold_left_s
      (fun (cache : Posts_cache.t) (record : Vkashka.Wall.Record.t) ->
        Log.info "Найдена новая запись (id: %d)" record.id;

        try%lwt
          Log.debug "Репост поста %d" record.id;
          let%lwt message = repost record in

          Lwt.return
          @@ Posts_cache.add_post cache
               Posts_cache.
                 {
                   vk_record_id = record.id;
                   tg_message_id = message.result.message_id;
                   last_modify = Option.value record.edited ~default:record.date;
                 }
        with Telegram.Parser_response_error { message; body } ->
          Log.error "Не удалось зарепостить пост: %s; body: %s" message body;
          Lwt.return cache)
      cache new_records
  in

  Log.debug "Закончен процесс репостинга";

  (* Edit updated posts. *)
  let edited_records =
    filter_edited_records_and_posts ~posts:cache.posts vk_records
  in

  Log.debug "Начат процесс обновления";

  let%lwt cache =
    Lwt_list.fold_left_s
      (fun cache ((record : Vkashka.Wall.Record.t), (post : Posts_cache.post)) ->
        try%lwt
          Log.debug "Попытка обновить пост (id: %d)" record.id;
          edit_post ~post ~record;%lwt

          Log.info "Обновлён пост (id: %d)" record.id;

          Lwt.return
          @@ Posts_cache.edit_post cache
               { post with last_modify = Utils.last_date_vk_record record }
        with Telegram.Parser_response_error { message; body } ->
          Log.error "Не удалось зарепостить пост: %s; body: %s" message body;
          Lwt.return cache)
      cache edited_records
  in

  (* let%lwt  *)
  Log.debug "Закончен процесс обновления";

  (* Save cache. *)
  Log.debug "Начато сохранение кеша";
  Posts_cache.save_to_file ~path:envs.cache_file cache;
  Log.info "Кеш был сохранён (по пути %s)" envs.cache_file;

  Log.info "Последний ID записи: %d" cache.last_record_id;

  Lwt.return_unit

let () =
  Log.set_log_level @@ if envs.debug then Log.DEBUG else Log.INFO;
  Log.set_output stdout;

  if Unix.isatty Unix.stdout then Log.color_on ()
  else
    Log.set_prefix_builder (fun l ->
        Printf.sprintf "%s: " @@ Log.string_of_level l);

  Lwt_main.run @@ main ()
