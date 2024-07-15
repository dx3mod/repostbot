open Sexplib.Std

type cache = { mutable last_post_id : int; mutable posts : post list }
[@@deriving sexp]

and post = { id : int; last_modification : int; message_id : int }

type t = { path : string; cache : cache }

let of_file path =
  let cache = Sexplib.Sexp.load_sexp path |> cache_of_sexp in
  { path; cache }

let save c = Sexplib.Sexp.save c.path (sexp_of_cache c.cache)

let add_post c post =
  let posts = post :: c.cache.posts in
  c.cache.last_post_id <- post.id;
  c.cache.posts <- posts

let posts c = c.cache.posts
let iter f c = List.iter f (posts c)
let last_post_id c = c.cache.last_post_id
