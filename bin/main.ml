open Lib

module type Repost_bot_depends = sig
  module Vk_api : Vkashka.S
  module Tg_bot_api : Tgbot.Bot.S

  val cache : Posts_cache.t
  val project_env : Project_env.t
end 

module Repost_bot (Depends : Repost_bot_depends) = struct
  open! Depends

  let pull_new_and_edited_vk_posts () =
    let%lwt Vkashka.Wall.{ items = posts; _ } =
      Vk_api.Wall.get ~count:"30" (`Domain project_env.targets.vk_user)
    in

    let new_posts =
      List.filter
        (fun (post : Vkashka.Wall.Record.t) -> post.id > cache.last_record_id)
        posts
    in

    let edited_posts =
      List.filter_map
        (fun (cached_post : Posts_cache.post) ->
          List.find_opt
            (fun (vk_post : Vkashka.Wall.Record.t) ->
              vk_post.id = cached_post.vk_record_id
              && cached_post.last_modify < Utils.last_date_vk_record vk_post)
            posts
          |> Option.map (fun vk_post -> (vk_post, cached_post)))
        cache.posts
    in

    Lwt.return (List.rev new_posts, edited_posts)

  let crop_text input =
    let input_length = String.length input - 1 in

    if input_length < 4055 then input
    else
      let last_whitespace_index = ref 0 in

      for i = 0 to input_length do
        if i < 4055 then
          if String.unsafe_get input i = ' ' then last_whitespace_index := i
      done;

      String.sub input 0 !last_whitespace_index

  let repost_post (vk_post : Vkashka.Wall.Record.t) =
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
      Tg_bot_api.send_message ~chat_id:project_env.targets.tg_chat_id
        (crop_text vk_post.text ^ "\n\n"
        ^ attachments_to_string vk_post.attachments)
    in

    let%lwt _ =
      let photo_urls =
        List.filter_map
          (function
            | Vkashka.Media.Attachment.Photo photo ->
                last_size photo.sizes
                |> Option.map (fun (size : Vkashka.Media.Photo.size) ->
                       size.url)
            | Vkashka.Media.Attachment.Video video ->
                last_size video.preview
                |> Option.map (fun (img : Vkashka.Media.Video.image) -> img.url)
            | _ -> None)
          vk_post.attachments
      in

      Tg_bot_api.send_media_group ~chat_id:project_env.targets.tg_chat_id
        (List.map
           (fun url -> Tgbot_api.Methods.Media.Photo (`Url url))
           photo_urls)
    in

    Lwt.return message

  let edit_post ~(cached_post : Posts_cache.post)
      ~(vk_post : Vkashka.Wall.Record.t) =
    let format_unix_time time =
      time |> float_of_int |> Unix.localtime |> fun t ->
      Printf.sprintf "%04d-%02d-%02d %02d:%02d" (t.tm_year + 1900)
        (t.tm_mon + 1) t.tm_mday t.tm_hour t.tm_min
    in

    let%lwt _ =
      Tg_bot_api.edit_message_text ~chat_id:project_env.targets.tg_chat_id
        ~message_id:cached_post.tg_message_id
      @@ Printf.sprintf "%s\n\nedited at %s" (crop_text vk_post.text)
           (format_unix_time @@ Utils.last_date_vk_record vk_post)
    in
    Lwt.return_unit

  let start () =
    Lwt_io.printlf "Start reposting";%lwt

    let%lwt new_posts, edited_posts = pull_new_and_edited_vk_posts () in

    Lwt_list.iter_p
      (fun (vk_post, cached_post) ->
        try%lwt
          edit_post ~vk_post ~cached_post;%lwt

          Posts_cache.edit_post cache
            { cached_post with last_modify = Utils.last_date_vk_record vk_post };

          Lwt.return_unit
        with _ ->
          Lwt_io.printlf "Failed to update post vk_id:%d!"
            cached_post.vk_record_id)
      edited_posts;%lwt

    Lwt_list.iter_s
      (fun vk_post ->
        try%lwt
          let%lwt posted_tg_message = repost_post vk_post in

          Posts_cache.add_post cache
            Posts_cache.
              {
                vk_record_id = vk_post.id;
                tg_message_id = posted_tg_message.message_id;
                last_modify = Option.value vk_post.edited ~default:vk_post.date;
              }
          |> ignore;

          Lwt.return_unit
        with _ ->
          Lwt_io.printlf "Failed to repost VK post with ID %d!" vk_post.id)
      new_posts;%lwt

    Posts_cache.save_to_file cache ~path:project_env.cache_file;

    Lwt_io.printlf "Finish reposting"
end

(* Run *)

let make_repost_bot_depends (project_env : Project_env.t) =
  let vk_access_token = Vkashka.access_token project_env.vk_token in
  let module Vk_api =
    Vkashka.Make (Cohttp_lwt_unix.Client) ((val vk_access_token))
  in
  let (module Tg_bot_api) = Tgbot.Bot.make ~token:project_env.tg_token in

  (module struct
    module Vk_api = Vk_api
    module Tg_bot_api = Tg_bot_api

    let cache = Posts_cache.load_from_file project_env.cache_file
    let project_env = project_env
  end : Repost_bot_depends)

let () =
  let project_env = Project_env.capture () in

  let (module Repost_bot_depends) = make_repost_bot_depends project_env in
  let module Repost_bot = Repost_bot (Repost_bot_depends) in
  let rec interval duration f =
    Lwt_unix.sleep duration;%lwt
    f ();%lwt
    interval duration f
  in

  Sys.catch_break true;
  try
    Lwt_main.run
    @@ interval (float_of_int project_env.interval) Repost_bot.start
  with Sys.Break ->
    Posts_cache.save_to_file Repost_bot_depends.cache
      ~path:project_env.cache_file
