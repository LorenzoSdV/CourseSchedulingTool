open Schedule

type command =
  | Add of string list
  | Edit of string list
  | Remove of string list
  | Open of string (* only course/schedule not semester. right after they create a course,
                      prompt to add it to a schedule*)
  | Close of string

(* Add course with just the name will do online parsing from class roster *)


exception Empty

exception Malformed

let edit_others str_lst =
  match str_lst with
  | class_name::field::[] when  

      let parse_command cmd_str = 

        let first_match_helper first others =
          match first with
          | "add" -> Add others
          | "edit" -> Edit edit_others (others)
          | "remove" -> Remove others
          | _ -> raise Malformed
        in

        let split_cmd = String.split_on_char ' ' cmd_str in
        match (split_cmd) with 
        | [] -> raise Empty
        | fst::[] -> raise Malformed
        | fst::others -> match_helper fst others



