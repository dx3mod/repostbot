open Lib

let watch_new_posts () =
  let%lwt wall = Global.Vk.Wall.get (`Domain Global.target_user) in
  let new_posts =
    List.filter
      (fun (r : Vkashka.Wall.Record.t) ->
        r.id > Cache.last_post_id Global.cache)
      wall.items
  in
  let updated_posts =
    let cached_posts = Cache.posts Global.cache in

    List.filter_map
      (fun (r : Vkashka.Wall.Record.t) ->
        let post =
          List.find_opt
            (fun (p : Cache.post) ->
              p.id = r.id && r.date > Cache.(p.last_modification))
            cached_posts
        in

        Option.map (fun p -> (r, p)) post)
      wall.items
  in

  Lwt.return (List.rev new_posts, updated_posts)

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
    Global.TgBot.send_message ~chat_id:Global.target_chat
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

    Global.TgBot.send_photos ~chat_id:Global.target_chat photo_urls
  in

  Lwt.return message

let main =
  Lwt_io.printlf "Get new posts and updates %s" Global.target_user;%lwt

  let%lwt new_posts, updated_posts = watch_new_posts () in

  Lwt_io.printlf "Start repost process";%lwt

  Lwt_list.iter_s
    (fun (r : Vkashka.Wall.Record.t) ->
      Lwt_io.printlf " + new post id:%d" r.id;%lwt

      let%lwt message = repost r in

      Cache.add_post Global.cache
        {
          id = r.id;
          last_modification = Option.value r.edited ~default:r.date;
          message_id = message.result.message_id;
        };

      Lwt.return_unit)
    new_posts;%lwt

  Cache.save Global.cache;

  print_endline "Update posts";

  Lwt_list.iter_s
    (fun ((r : Vkashka.Wall.Record.t), (p : Cache.post)) ->
      let last_modification_date =
        Option.value r.edited ~default:p.last_modification
        |> float_of_int |> Unix.localtime
        |> fun t ->
        Printf.sprintf "%04d-%02d-%02d" (t.tm_year + 1900) (t.tm_mon + 1)
          t.tm_mday
      in

      let new_text =
        Printf.sprintf "%s\nEdited at %s." r.text last_modification_date
      in

      try%lwt
        let%lwt _ =
          Global.TgBot.edit_message_text ~chat_id:Global.target_chat
            ~message_id:(string_of_int p.message_id)
            new_text
        in

        Lwt_io.printlf " ~ edit post id:%d" r.id
      with Failure msg ->
        Lwt_io.eprintlf "Failed to edit message. Error: %s" msg)
    updated_posts;%lwt

  Lwt_io.printlf "Last post id: %d" Global.cache.cache.last_post_id;%lwt

  Lwt.return_unit

let () = Lwt_main.run main
