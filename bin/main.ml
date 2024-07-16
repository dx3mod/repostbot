open Lib

let watch_new_posts () =
  let%lwt wall = Api.Vk.Wall.get (`Domain Api.target_user) in
  let new_posts =
    List.filter
      (fun (r : Vkashka.Wall.Record.t) -> r.id > Cache.last_post_id Api.cache)
      wall.items
  in
  let updated_posts =
    let cached_posts = Cache.posts Api.cache in

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
  Printf.printf "new post: %d\n" post.id;

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
    Api.TgBot.send_message ~chat_id:Api.target_chat
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

    Api.TgBot.send_photos ~chat_id:Api.target_chat photo_urls
  in

  Lwt.return message

let main =
  let%lwt new_posts, updated_posts = watch_new_posts () in

  print_endline "start repost posts";

  let%lwt _ =
    Lwt_list.iter_s
      (fun (r : Vkashka.Wall.Record.t) ->
        let%lwt message = repost r in

        Cache.add_post Api.cache
          {
            id = r.id;
            last_modification = Option.value r.edited ~default:r.date;
            message_id = message.result.message_id;
          };

        Lwt.return_unit)
      new_posts
  in

  Cache.save Api.cache;

  print_endline "start update";

  let%lwt _ =
    Lwt_list.iter_s
      (fun ((r : Vkashka.Wall.Record.t), (p : Cache.post)) ->
        let%lwt _ =
          Printf.printf "edit post %d\n" r.id;

          try
            let%lwt _ =
              Api.TgBot.edit_message_text ~chat_id:Api.target_chat
                ~message_id:(string_of_int p.message_id)
                r.text
            in

            Lwt.return_unit
          with Failure msg -> Lwt_io.eprintlf "failure %s" msg
        in
        Lwt.return_unit)
      updated_posts
  in

  Lwt.return_unit

let () = Lwt_main.run main
