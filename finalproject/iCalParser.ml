open Schedule

(** Needs Comment == is the list of lines of an ical file. *)
let ical_to_list file =
  let inch = open_in file in
  let rec all_lines ch acc = 
    try
      all_lines ch ((input_line ch) :: acc)
    with
      End_of_file -> acc
  in
  let result = all_lines inch [] in
  close_in inch; result

(** Gets courses from list of data *)
let parse_courses data_list =
  let regexp = Str.regexp "[A-Z][A-Z]+ [0-9][0-9][0-9][0-9]" in
  let important_lines = 
    List.filter (fun s -> (Str.first_chars s 7) = "SUMMARY") data_list in
  List.fold_left 
    (fun acc elem -> 
       if Str.string_match regexp elem 23 then Str.matched_string elem :: acc
       else acc) 
    []
    important_lines 
  |> List.sort_uniq compare
  |> List.rev_map (let reg = Str.regexp " " in Str.replace_first reg "")

(** gets semester info from ical data *)
let parse_semid data_list =
  let line = 
    List.filter (fun s -> (Str.first_chars s 7) = "DTSTART") data_list
    |> List.hd
  in
  let timestamp = Str.string_before (Str.string_after line 10) 4 in
  match Str.last_chars timestamp 2 |> int_of_string with
  | 1 | 2 -> "SP" ^ (Str.first_chars timestamp 4)
  | 8 | 9 -> "FA" ^ (Str.first_chars timestamp 4)
  | _ -> failwith "Don't support summmer/winter sessions"

let parse_file file = 
  let data = ical_to_list file in
  (parse_courses data, parse_semid data)