(lang dune 3.16)

(name repostbot)

(generate_opam_files true)

(source
 (github dx3mod/repostbot))

(authors "Mikhail Lopatin <dx3mod@bk.ru>")

(maintainers "Mikhail Lopatin <dx3mod@bk.ru>")

(license MIT)

(package
 (name repostbot)
 (synopsis "Bot for forwarding user's posts from VK to Telegram chat")
 (depends
  ocaml
  dune
  lwt
  cohttp-lwt-unix
  vkashka
  sexplib
  lwt_ppx
  ppx_deriving_yojson))
