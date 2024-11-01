type t = {
  tg_token : string;
  vk_token : string;
  targets : targets;
  cache_file : string;
  debug : bool;
}

and targets = { vk_user : string; tg_chat_id : int }

let capture () =
  let tg_token = Sys.getenv "TG_TOKEN" in
  let vk_token = Sys.getenv "VK_TOKEN" in
  let targets =
    let vk_user = Sys.getenv "TARGET_USER" in
    let tg_chat_id = Sys.getenv "TARGET_CHAT" in
    { vk_user; tg_chat_id = int_of_string tg_chat_id }
  in
  let cache_file = Sys.getenv "CACHE_FILE" in
  let debug =
    match Sys.getenv_opt "DEBUG" with Some ("1" | "true") -> true | _ -> false
  in

  { tg_token; vk_token; targets; cache_file; debug }
