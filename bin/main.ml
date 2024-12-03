open Core

let read_file filename =
  In_channel.with_file filename ~f:(fun ic -> In_channel.input_lines ic)
;;

let split_line line =
  String.split_on_chars ~on:[ ' ' ] line
  |> List.filter ~f:(fun s -> not (String.is_empty s))
;;

let update_map ~map ~key =
  Map.update map key ~f:(function
    | None -> 1
    | Some value -> value + 1)
;;

let process_lines ~lines =
  let c2_map = Map.empty (module Int) in
  List.fold
    ~init:([], c2_map)
    ~f:(fun (col1, map) s ->
      match split_line s with
      | [ c1; c2 ] -> int_of_string c1 :: col1, update_map ~map ~key:(int_of_string c2)
      | _ -> failwith "invalid string")
    lines
;;

let similarity_score ~col1 ~map =
  List.fold
    ~init:0
    ~f:(fun acc x ->
      match Map.find map x with
      | None -> acc
      | Some y -> acc + (x * y))
    col1
;;

let print_answer ~lines =
  let col1, col2_map = process_lines ~lines in
  print_endline (string_of_int (similarity_score ~col1 ~map:col2_map))
;;

let () =
  let filename = "input/day1_2.txt" in
  match read_file filename with
  | lines -> print_answer ~lines
  | exception exn -> printf "Error: %s\n" (Exn.to_string exn)
;;
