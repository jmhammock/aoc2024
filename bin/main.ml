open Core

type vect =
  { col : int
  ; row : int
  }

let get_x_vectors matrix =
  matrix
  |> Array.concat_mapi ~f:(fun r row ->
    row
    |> Array.mapi ~f:(fun c col ->
      if Char.equal col 'X' then Some { col = c; row = r } else None)
    |> Array.filter_map ~f:Fn.id)
  |> Array.to_list
;;

let search_vects ~vect ~scalars =
  [ List.map scalars ~f:(fun x -> { vect with col = vect.col + x })
  ; List.map scalars ~f:(fun x -> { vect with col = vect.col - x })
  ; List.map scalars ~f:(fun x -> { vect with row = vect.row + x })
  ; List.map scalars ~f:(fun x -> { vect with row = vect.row - x })
  ; List.map scalars ~f:(fun x -> { col = vect.col + x; row = vect.row + x })
  ; List.map scalars ~f:(fun x -> { col = vect.col - x; row = vect.row - x })
  ; List.map scalars ~f:(fun x -> { col = vect.col - x; row = vect.row + x })
  ; List.map scalars ~f:(fun x -> { col = vect.col + x; row = vect.row - x })
  ]
;;

let transform_vects ~search_vects ~matrix =
  let get_char vect = Option.try_with (fun () -> matrix.(vect.row).(vect.col)) in
  search_vects
  |> List.map ~f:(fun vl ->
    vl |> List.filter_map ~f:(fun v -> get_char v) |> String.of_char_list)
;;

let search ~matrix vect =
  let search_vects = search_vects ~vect ~scalars:[ 1; 2; 3 ] in
  let search_strings = transform_vects ~search_vects ~matrix in
  List.filter_map
    ~f:(fun s -> if String.( = ) s "MAS" then Some vect else None)
    search_strings
;;

let read_file filename =
  In_channel.with_file filename ~f:(fun ic -> In_channel.input_lines ic)
;;

let array_of_list list =
  let rows = List.map ~f:(fun s -> String.to_list s |> Array.of_list) list in
  Array.of_list rows
;;

let () =
  let filename = "input/day4.txt" in
  let content = read_file filename in
  let matrix = array_of_list content in
  let seeker = search ~matrix in
  matrix
  |> get_x_vectors
  |> List.map ~f:seeker
  |> List.concat
  |> List.length
  |> Int.to_string
  |> print_endline
;;
