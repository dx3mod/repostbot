(lang dune 3.16)

(name repostbot)

(generate_opam_files true)

(source
 (github dx3mod/repostbot))

(authors "Mihail Lopatin <dx3mod@bk.ru>")

(maintainers "Mihail Lopatin <dx3mod@bk.ru>")

(license MIT)

(package
 (name repostbot)
 (synopsis "Bot for forwarding user's posts from VK to Telegram chat")
 (depends ocaml dune))
