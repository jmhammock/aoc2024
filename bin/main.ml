open Core

let read_file filename =
  In_channel.with_file filename ~f:(fun ic -> In_channel.input_lines ic)
;;

let rules_map lines =
  let update_rule ~map ~line =
    match String.split ~on:'|' line with
    | [ hd; tl ] ->
      Map.update map hd ~f:(function
        | Some v -> tl :: v
        | None -> [ tl ])
    | _ -> failwith "bad line"
  in
  List.fold
    lines
    ~init:(Map.empty (module String))
    ~f:(fun map line -> update_rule ~map ~line)
;;

let lst_middle lst =
  let rec find_middle slow fast =
    match fast with
    | [] | [ _ ] -> slow
    | _ :: _ :: fast_tl -> find_middle (List.tl_exn slow) fast_tl
  in
  match lst with
  | [] -> failwith "bad list"
  | _ -> List.hd_exn (find_middle lst lst)
;;

let update_list lines = List.map lines ~f:(fun line -> String.split ~on:',' line)

let intersect_updates l1 l2 =
  List.filter l1 ~f:(fun x -> List.mem l2 x ~equal:String.( = ))
;;

let eval_pages ~page ~rules =
  let rec aux result updates =
    match result with
    | None -> None
    | Some _ ->
      (match updates with
       | [] | [ _ ] -> result
       | hd :: tl ->
         (match Map.find rules hd with
          | None -> None
          | Some order ->
            let inter = intersect_updates order tl in
            if List.length inter = List.length tl then aux (Some ()) tl else None))
  in
  match aux (Some ()) page with
  | Some _ -> Some (lst_middle page)
  | None -> None
;;

let () =
  let rules_file = "input/day5rules.txt" in
  let rules_content = read_file rules_file in
  let updates_file = "input/day5updates.txt" in
  let updates_content = read_file updates_file in
  let rules = rules_map rules_content in
  let updates = update_list updates_content in
  List.filter_map updates ~f:(fun update -> eval_pages ~page:update ~rules)
  |> List.map ~f:Int.of_string
  |> List.fold ~init:0 ~f:( + )
  |> printf "%d\n"
;;
