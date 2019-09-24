module Bytes_helper = struct
  let consume n bytes =
    Bytes.sub bytes n (Bytes.length bytes - n)

  let consume_crlf =
    consume 2
end

module String_helper = struct
  let of_char char =
    String.make 1 char

  let append_char char string =
    string ^ of_char char
end

type t =
  | List of t list
  | String of string

module Encoder = struct
  let encode_list_length list =
    Printf.sprintf "@%d\r\n" (List.length list)

  let rec encode_list_inner f list acc =
    match list with
    | []       -> acc
    | hd :: tl -> encode_list_inner f tl (String.concat "" [acc; f hd])

  let encode_list f list =
    String.concat "" [encode_list_inner f list (encode_list_length list); "\r\n"]

  let encode_string_length string =
    Printf.sprintf "$%d\r\n" (String.length string)

  let encode_string string =
    String.concat "" [encode_string_length string; string; "\r\n"]

  let rec encode message =
    match message with
    | List list     -> encode_list encode list
    | String string -> encode_string string
end

module Decoder = struct
  let rec decode_length_inner bytes acc =
    match (Bytes.get bytes 0) with
    | '\r' ->
      let bytes' = Bytes_helper.consume_crlf bytes in
      let length = int_of_string acc in
      (bytes', length)
    | char ->
      let bytes' = Bytes_helper.consume 1 bytes in
      let acc'   = String_helper.append_char char acc in
      decode_length_inner bytes' acc'

  let decode_length bytes =
    decode_length_inner bytes ""

  let rec decode_list_inner f bytes length acc =
    match length with
    | 0 ->
      let bytes'  = Bytes_helper.consume_crlf bytes in
      let message = List (List.rev acc) in
      (bytes', message)
    | _ ->
      let (bytes', message) = f bytes in
      decode_list_inner f bytes' (length - 1) (message :: acc)

  let decode_list f (bytes, length) =
    decode_list_inner f bytes length []

  let decode_string (bytes, length) =
    (* Consume the string length and trailing crlf in single call. *)
    let bytes'  = Bytes_helper.consume (length + 2) bytes in
    let message = String (Bytes.sub_string bytes 0 length) in
    (bytes', message)

  let rec decode bytes =
    let bind f =
      let bytes' = Bytes_helper.consume 1 bytes in
      f (decode_length bytes')
    in

    match (Bytes.get bytes 0) with
    | '@'  -> bind (decode_list decode)
    | '$'  -> bind decode_string
    | char -> invalid_arg (String_helper.append_char char "invalid character ")
end

let get_list message =
  match message with
  | List list -> list
  | String _  -> invalid_arg "response is String _"

let get_string message =
  match message with
  | List _        -> invalid_arg "response is List _"
  | String string -> string

let to_bytes message =
  Bytes.of_string (Encoder.encode message)

let of_bytes bytes =
  let (_, message) = Decoder.decode bytes in message
