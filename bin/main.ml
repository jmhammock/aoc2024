open Core

let read_file filename = In_channel.with_file filename ~f:In_channel.input_all

let regex_match pattern input =
  let re = Re.compile (Re.Perl.re pattern) in
  Re.all re input
;;

let () =
  let filename = "input/day3.txt" in
  let contents = read_file filename in
  let pattern = "mul\\((\\d{1,3}),(\\d{1,3})\\)" in
  let sum, _ =
    regex_match pattern contents
    |> List.fold_map ~init:0 ~f:(fun acc g ->
      match Re.Group.all g with
      | [| _; m1; m2 |] ->
        let t = Int.of_string m1 * Int.of_string m2 in
        acc + t, t
      | _ -> failwith "invalid group")
  in
  print_endline (Int.to_string sum)
;;
