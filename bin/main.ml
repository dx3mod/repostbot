open Lib
module Log = Dolog.Log

let get_env_vars () =
  try Project_env.capture ()
  with Not_found ->
    Log.error "не найдены необходимые переменные окружения";
    exit 1

let envs = get_env_vars ()

module Vk_api =
  Vkashka.Api
    (Cohttp_lwt_unix.Client)
    ((val Vkashka.access_token envs.vk_token))

module Tg_bot = Telegram.Bot ((val Telegram.token envs.tg_token))

let get_new_vk_records ~last_record_id ~user =
  let%lwt wall = Vk_api.Wall.get (`Domain user) in

  let new_records =
    List.filter
      (fun (r : Vkashka.Wall.Record.t) -> r.id > last_record_id)
      wall.items
  in

  Lwt.return @@ List.rev new_records

let repost (post : Vkashka.Wall.Record.t) =
  let attachments_to_string (attachments : Vkashka.Media.Attachment.t list) =
    String.concat ", "
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
      (post.text ^ "\n\n" ^ attachments_to_string post.attachments)
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

let main () =
  let cache = Posts_cache.load_from_file envs.cache_file in

  Log.info "получение постов со стены (пользователя %s)" envs.targets.vk_user;

  let%lwt new_records =
    get_new_vk_records ~last_record_id:cache.last_record_id
      ~user:envs.targets.vk_user
  in

  let%lwt cache =
    Lwt_list.fold_left_s
      (fun (cache : Posts_cache.t) (record : Vkashka.Wall.Record.t) ->
        Log.info "найдена новая запись (id: %d)" record.id;

        if String.length record.text >= Telegram.limit_message_chapters then (
          Log.warn "текст записи превышает %d символов"
            Telegram.limit_message_chapters;
          Log.error "не удалось зарепостить";
          Lwt.return cache)
        else
          try%lwt
            Log.debug "репост поста %d" record.id;
            let%lwt message = repost record in

            Lwt.return
            @@ Posts_cache.add_post cache
                 Posts_cache.
                   {
                     vk_record_id = record.id;
                     tg_message_id = message.result.message_id;
                     last_modify =
                       Option.value record.edited ~default:record.date;
                   }
          with Telegram.Parser_response_error { message; body } ->
            Log.error
              "не удалось зарепостить пост из-ша ошибки парсинга ответа: %s; \
               body: %s"
              message body;
            Lwt.return cache)
      cache new_records
  in

  if not @@ List.is_empty new_records then (
    Log.debug "начато сохранение кеша";
    Posts_cache.save_to_file ~path:envs.cache_file cache;
    Log.info "кеш был сохранён (по пути %s)" envs.cache_file)
  else Log.info "новых постов не обнаружено";

  Log.info "последний ID записи: %d" cache.last_record_id;

  Lwt.return_unit

let () =
  Log.set_log_level @@ if envs.debug then Log.DEBUG else Log.INFO;
  Log.set_output stdout;

  if Unix.isatty Unix.stdout then Log.color_on ()
  else
    Log.set_prefix_builder (fun l ->
        Printf.sprintf "%s: " @@ Log.string_of_level l);

  Lwt_main.run @@ main ()
