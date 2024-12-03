open Core

let read_file filename =
  In_channel.with_file filename ~f:(fun ic -> In_channel.input_lines ic)
;;

let split_line line =
  String.split_on_chars ~on:[ ' ' ] line
  |> List.filter ~f:(fun s -> not (String.is_empty s))
  |> List.map ~f:int_of_string
;;

type direction =
  | Increasing
  | Decreasing
  | NotDetermined

type trend_opt =
  | Some of
      { direction : direction
      ; value : int
      }
  | None

let determine_trend ~prev ~curr =
  let diff = Int.abs (prev - curr) in
  let valid = diff <= 3 && diff > 0 in
  if prev > curr && valid
  then Some { direction = Decreasing; value = curr }
  else if prev < curr && valid
  then Some { direction = Increasing; value = curr }
  else None
;;

let update_trend ~prev ~curr =
  match prev, curr with
  | Some { direction = NotDetermined; _ }, _ -> curr
  | Some { direction = Increasing; _ }, Some { direction = Increasing; _ } -> curr
  | Some { direction = Decreasing; _ }, Some { direction = Decreasing; _ } -> curr
  | _ -> None
;;

let process_line line =
  split_line line
  |> List.fold
       ~init:(Some { direction = NotDetermined; value = 0 })
       ~f:(fun acc curr ->
         match acc with
         | None -> None
         | Some { direction = NotDetermined; value = 0 } ->
           Some { direction = NotDetermined; value = curr }
         | Some prev ->
           let current_trend = determine_trend ~prev:prev.value ~curr in
           update_trend ~prev:acc ~curr:current_trend)
;;

let process_lines lines =
  List.fold
    ~init:0
    ~f:(fun acc line ->
      match process_line line with
      | None -> acc
      | Some _ -> acc + 1)
    lines
;;

let print_answer lines = print_endline (string_of_int (process_lines lines))

let () =
  let filename = "input/day2.txt" in
  match read_file filename with
  | lines -> print_answer lines
  | exception exn -> printf "Error: %s\n" (Exn.to_string exn)
;;
