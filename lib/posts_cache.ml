open Sexplib.Std

type t = { last_record_id : int; posts : post list [@default []] }
[@@deriving sexp]

and post = { vk_record_id : int; tg_message_id : int; last_modify : int }

let load_from_file path = Sexplib.Sexp.load_sexp_conv_exn path t_of_sexp
let save cache oc = Sexplib.Sexp.output_hum oc (sexp_of_t cache)
let save_to_file ~path cache = Out_channel.with_open_text path (save cache)
let add_post cache post = { cache with posts = post :: cache.posts }

let edit_post cache post =
  {
    last_record_id = post.vk_record_id;
    posts =
      List.map
        (fun p -> if p.vk_record_id = post.vk_record_id then post else p)
        cache.posts;
  }

let iter f cache = List.iter f cache.posts
let posts cache = cache.posts
let last_post cache = List.hd cache.posts
