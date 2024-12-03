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

let rec process_line ~orig_line ~line ~tries =
  let remove_at_index idx = List.filteri ~f:(fun i _ -> i <> idx) orig_line in
  let processed =
    List.fold
      ~init:(Some { direction = NotDetermined; value = 0 })
      ~f:(fun acc curr ->
        match acc with
        | None -> None
        | Some { direction = NotDetermined; value = 0 } ->
          Some { direction = NotDetermined; value = curr }
        | Some prev ->
          let current_trend = determine_trend ~prev:prev.value ~curr in
          update_trend ~prev:acc ~curr:current_trend)
      line
  in
  match processed with
  | None ->
    if tries = List.length orig_line
    then None
    else process_line ~orig_line ~line:(remove_at_index tries) ~tries:(tries + 1)
  | Some _ -> processed
;;

let process_lines lines =
  List.fold
    ~init:0
    ~f:(fun acc line ->
      let split = split_line line in
      match process_line ~orig_line:split ~line:split ~tries:0 with
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
