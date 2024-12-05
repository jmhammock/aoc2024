open Core

let read_file filename = In_channel.with_file filename ~f:In_channel.input_all

type scan_state =
  | Enabled
  | Disabled

type scanner =
  { state : scan_state
  ; pos : int
  ; acc : int
  }

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
    let len = String.length (Re.Group.get result 0) in
    Some (len, Int.of_string m1 * Int.of_string m2)
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

let rec scan ~scanner ~content =
  match Option.try_with (fun () -> String.get content scanner.pos) with
  | None -> scanner
  | Some c ->
    let new_scanner =
      match scanner.state, c with
      | Enabled, 'm' ->
        (match find_mul ~pos:scanner.pos ~content with
         | Some (len, value) ->
           { scanner with acc = scanner.acc + value; pos = scanner.pos + len }
         | None -> { scanner with pos = scanner.pos + 1 })
      | Enabled, 'd' ->
        (match find_dont ~pos:scanner.pos ~content with
         | Some _ -> { scanner with state = Disabled; pos = scanner.pos + 5 }
         | None -> { scanner with pos = scanner.pos + 1 })
      | Disabled, 'd' ->
        (match find_do ~pos:scanner.pos ~content with
         | Some _ -> { scanner with state = Enabled; pos = scanner.pos + 2 }
         | None -> { scanner with pos = scanner.pos + 1 })
      | _ -> { scanner with pos = scanner.pos + 1 }
    in
    scan ~scanner:new_scanner ~content
;;

let () =
  let filename = "input/day3.txt" in
  let content = read_file filename in
  let initial_state = { state = Enabled; pos = 0; acc = 0 } in
  let final_state = scan ~scanner:initial_state ~content in
  print_endline (Int.to_string final_state.acc)
;;
