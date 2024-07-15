open Lib

let vk_token = Vkashka.access_token (Sys.getenv "VK_TOKEN")

module Vk = Vkashka.Api (Cohttp_lwt_unix.Client) ((val vk_token))

let tg_token = Telegram.token (Sys.getenv "TG_TOKEN")

module TgBot = Telegram.Bot ((val tg_token))

let cache_path = Sys.getenv "CACHE_FILE"
let cache = Cache.of_file cache_path
let target_user = Sys.getenv "TARGET_USER"
let target_chat = Sys.getenv "TARGET_CHAT"
