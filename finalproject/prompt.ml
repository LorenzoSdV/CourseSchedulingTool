open Schedule

type command =
  | Add of string list
  | Edit of string list
  | Remove of string list
  | Open of string (* only course/schedule not semester. right after they create a course,
                      prompt to add it to a schedule*)
  | Close of string

(* Add course with just the name will do online parsing from class roster *)

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

let parse_command cmd_str = 

  let check_match_helper str =
    if str = 
  in

  (* To add a class, check if it is a real class on class roster *)
  let others_match_helper others =
    match others with
    | fst::next -> check_match_helper fst
  in

  let first_match_helper first others =
    match first with
    | "add" -> Add others
    | "edit" -> Edit others_match_helper (others)
    | "remove" -> Remove others
    | _ -> raise Malformed
  in

  let split_cmd = String.split_on_char ' ' cmd_str in
  match (split_cmd) with 
  | [] -> raise Empty
  | fst::second::_ -> if second = 
  | fst::others -> match_helper fst others

let get_course_lst sem = 

