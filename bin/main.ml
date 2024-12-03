open Core

let read_file filename = 
    In_channel.with_file filename ~f:(fun ic -> In_channel.input_lines ic)

let split_line line =
    String.split_on_chars ~on:[' '] line
    |> List.filter ~f:(fun s -> not (String.is_empty s))

let lines_to_lists ~lines =
    List.fold ~init:([], []) ~f:(fun (col1, col2) s -> 
        match split_line s with
        | [c1; c2] -> (int_of_string c1 :: col1, int_of_string c2 :: col2)
        | _ -> failwith "invalid string") lines 
    
let sort_fold_diff ~col1 ~col2 = 
    let col1 = List.sort ~compare:Int.compare col1 in
    let col2 = List.sort ~compare:Int.compare col2 in
    List.fold2_exn ~f:(fun acc x y -> acc + Int.abs(x - y)) ~init:0 col1 col2

let print_answer ~lines =
    let (col1, col2) = lines_to_lists ~lines in
    print_endline (string_of_int (sort_fold_diff ~col1 ~col2))

let () =
    let filename = "input/day1.txt" in
    match read_file filename with
        | lines -> print_answer ~lines
        | exception exn -> printf "Error: %s\n" (Exn.to_string exn)
