module type Token = sig
  val token : string
end

let token value =
  (module struct
    let token = value
  end : Token)

module Bot (T : Token) = struct
  open Cohttp_lwt_unix

  let base_uri meth =
    Printf.sprintf "https://api.telegram.org/bot%s/%s" T.token meth
    |> Uri.of_string

  module Endpoint = struct
    let get_me = base_uri "getMe"

    let send_dice ~chat_id =
      let uri = base_uri "sendDice" in
      Uri.add_query_param' uri ("chat_id", chat_id)

    let send_message ~chat_id ~message =
      let uri = base_uri "sendMessage" in
      Uri.add_query_params' uri [ ("chat_id", chat_id); ("text", message) ]

    let send_photo ~chat_id ~url =
      let uri = base_uri "sendPhoto" in
      Uri.add_query_params' uri [ ("chat_id", chat_id); ("photo", url) ]

    let send_photos ~chat_id ~urls =
      let photo_objects =
        String.concat ","
        @@ List.map (Printf.sprintf {|{"type":"photo","media":"%s"}|}) urls
      in

      let uri = base_uri "sendMediaGroup" in
      Uri.add_query_params' uri
        [ ("chat_id", chat_id); ("media", "[" ^ photo_objects ^ "]") ]

    let edit_message_text ~chat_id ~message_id ~text =
      let uri = base_uri "editMessageText" in
      Uri.add_query_params' uri
        [ ("chat_id", chat_id); ("message_id", message_id); ("text", text) ]
  end

  module Response = struct
    type 'a t = { ok : bool; result : 'a }
    [@@deriving of_yojson { strict = false }]

    type message = { message_id : int }
    [@@deriving of_yojson { strict = false }]
  end

  let send_request ~p uri =
    let open Lwt.Syntax in
    let* _, body = Client.get uri in
    let* body = Cohttp_lwt.Body.to_string body in
    let json = Yojson.Safe.from_string body in

    Lwt.return
    @@
    match p json with
    | Ok x -> x
    | Error e ->
        failwith
        (* don't print uri (it's have token) *)
        @@ Printf.sprintf "(send_request) uri: %s  error: %s; body: %s"
             (Uri.to_string uri) e body

  let send_dice ~chat_id =
    send_request ~p:Response.(of_yojson message_of_yojson)
    @@ Endpoint.send_dice ~chat_id

  let send_message ~chat_id message =
    send_request ~p:Response.(of_yojson message_of_yojson)
    @@ Endpoint.send_message ~chat_id ~message

  let send_photo ~chat_id url =
    send_request ~p:Response.(of_yojson message_of_yojson)
    @@ Endpoint.send_photo ~chat_id ~url

  let send_photos ~chat_id urls =
    send_request ~p:(fun _ -> Ok ()) @@ Endpoint.send_photos ~chat_id ~urls

  let edit_message_text ~chat_id ~message_id text =
    send_request ~p:Response.(of_yojson message_of_yojson)
    @@ Endpoint.edit_message_text ~chat_id ~message_id ~text
end
