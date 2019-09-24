let check predicate =
  assert predicate;

  Printf.printf "."

let check_equal a b =
  check (a = b)

let check_of_bytes message =
  let bytes = Message.to_bytes message in
  check_equal (Message.of_bytes bytes) message

let test_get_list () =
  let list = [Message.String "foo"] in
  check_equal (Message.get_list (Message.List list)) list

let test_get_string () =
  let string = "foo" in
  check_equal (Message.get_string (Message.String string)) string

let test_to_bytes_when_list () =
  let message = Message.List [Message.String "foo"] in
  let bytes   = Bytes.of_string "@1\r\n$3\r\nfoo\r\n\r\n" in
  check_equal (Message.to_bytes message) bytes

let test_to_bytes_when_list_is_empty () =
  let message = Message.List [] in
  let bytes   = Bytes.of_string "@0\r\n\r\n" in
  check_equal (Message.to_bytes message) bytes

let test_to_bytes_when_list_is_nested () =
  let message = Message.List [Message.List [Message.String "foo"]] in
  let bytes   = Bytes.of_string "@1\r\n@1\r\n$3\r\nfoo\r\n\r\n\r\n" in
  check_equal (Message.to_bytes message) bytes

let test_to_bytes_when_string () =
  let message = Message.String "foo" in
  let bytes   = Bytes.of_string "$3\r\nfoo\r\n" in
  check_equal (Message.to_bytes message) bytes

let test_to_bytes_when_string_is_empty () =
  let message = Message.String "" in
  let bytes   = Bytes.of_string "$0\r\n\r\n" in
  check_equal (Message.to_bytes message) bytes

let test_of_bytes_when_list () =
  check_of_bytes (Message.List [Message.String "foo"])

let test_of_bytes_when_list_is_empty () =
  check_of_bytes (Message.List [])

let test_of_bytes_when_string () =
  check_of_bytes (Message.String "foo")

let test_of_bytes_when_string_is_empty () =
  check_of_bytes (Message.String "")

let () =
  test_get_list ();
  test_get_string ();
  test_to_bytes_when_list ();
  test_to_bytes_when_list_is_empty ();
  test_to_bytes_when_list_is_nested ();
  test_to_bytes_when_string ();
  test_to_bytes_when_string_is_empty ();
  test_of_bytes_when_list ();
  test_of_bytes_when_list_is_empty ();
  test_of_bytes_when_string ();
  test_of_bytes_when_string_is_empty ();
  ()
