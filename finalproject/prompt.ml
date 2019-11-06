open Schedule
open ClassRoster

type command =
  | Add of string list
  | Edit of string list
  | Remove of string list
  | Open of string 
  | Close of string

exception Empty

exception Malformed

let gradify str =
  let str_upper = String.uppercase_ascii str in
  if Str.string_match (Str.regexp "^[A-DF]{1}[\\+-]?$") str_upper 0 then
    Letter str_upper
  else
    match str_upper with
    | "INCOMPLETE" -> Incomplete
    | "W" | "WITHDRAWN" -> Withdrawn
    | "SAT" | "S" -> Sat
    | "UNSAT" | "U" -> Unsat
    | _ -> raise (Failure "Invalid grade entry")

(** [sem_id_parse sem_id] parses [sem_id] if is a valid semester type.
    Raises: Malformed when [sem_id] is not valid. *)
let sem_id_parse sem_id =
  if Str.string_match (Str.regexp "^SP[0-9]{2}$") sem_id 0 
  then Spring (int_of_string (String.sub sem_id 2 2))
  else if Str.string_match (Str.regexp "^FA[0-9]{2}$") sem_id 0 
  then Fall (int_of_string (String.sub sem_id 2 2)) else raise Malformed

let add_others sch str_lst =
  match str_lst with
  | [] -> raise Empty
  | course_name::grade::degree::sem_id::[] -> 
    add_course sch 
      (create_course course_name (get_course_creds course_name (sem_id_parse sem_id)) (gradify grade) degree) (sem_id_parse sem_id)
  | course_name::credits::grade::degree::sem_id::[] -> 
    add_course sch (create_course course_name (int_of_string credits) (gradify grade) degree) (sem_id_parse sem_id)
  | _ -> raise Malformed

let edit_others str_lst sch =
  match str_lst with
  | [] -> raise Empty
  | course_name::field::[] -> edit_course sch course_name field
  | _ -> raise Malformed 

let parse_command cmd_str sch = 

  let first_match_helper first others =
    match first with
    | "add" -> Add add_others (others sch)
    | "edit" -> Edit edit_others (others sch)
    | "remove" -> Remove others
    | _ -> raise Malformed
  in

  let split_cmd = String.split_on_char ' ' cmd_str in
  match (split_cmd) with 
  | [] -> raise Empty
  | fst::[] -> raise Malformed
  | fst::others -> match_helper fst others



