open Core

let read_file filename = In_channel.with_file filename ~f:In_channel.input_all

type scan_state =
  | Enabled
  | Disabled

type scanner =
  { mutable state : scan_state
  ; mutable acc : int
  }

let scanner = { state = Enabled; acc = 0 }
let pattern = "mul\\((\\d{1,3}),(\\d{1,3})\\)"
let re = Re.compile (Re.Perl.re pattern)

let find_mul ~pos ~content =
  let content_len = String.length content in
  let len = if pos + 12 >= content_len then content_len - pos else 12 in
  let string_to_search = String.sub ~pos ~len content in
  match Re.exec_opt re string_to_search with
  | Some result ->
    let m1 = Re.Group.get result 1 in
    let m2 = Re.Group.get result 2 in
    Some (Int.of_string m1 * Int.of_string m2)
  | None -> None
;;

let find_dont ~pos ~content =
  if String.is_substring_at content ~substring:"don't" ~pos then Some () else None
;;

let find_do ~pos ~content =
  match find_dont ~pos ~content with
  | Some _ -> None
  | None -> if String.is_substring_at content ~substring:"do" ~pos then Some () else None
;;

let () =
  let filename = "input/day3.txt" in
  let content = read_file filename in
  String.iteri
    ~f:(fun i c ->
      match scanner.state, c with
      | Enabled, 'm' ->
        (match find_mul ~pos:i ~content with
         | Some value -> scanner.acc <- scanner.acc + value
         | None -> ())
      | Enabled, 'd' ->
        (match find_dont ~pos:i ~content with
         | Some _ -> scanner.state <- Disabled
         | None -> ())
      | Disabled, 'd' ->
        (match find_do ~pos:i ~content with
         | Some _ -> scanner.state <- Enabled
         | None -> ())
      | _ -> ())
    content;
  print_endline (Int.to_string scanner.acc)
;;
