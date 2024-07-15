(* let exports ?(path = ".env") () =
   In_channel.with_open_text path @@ fun ch ->
   In_channel.fold_lines
     (fun _ line ->
       match String.split_on_char '=' line with
       | [ key; value ] -> Unix.putenv key value
       | _ -> ())
     () ch *)
