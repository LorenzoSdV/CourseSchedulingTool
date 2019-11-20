open Schedule
open ClassRoster

exception Empty

exception Malformed
exception MalformedSemId
exception MalformedAdd
exception MalformedEdit
exception MalformedRemove
exception MalformedSave
exception MalformedLoad
exception MalformedExport

exception InvalidFile


let is_valid_coursename str =
  if (Str.string_match (Str.regexp "^[A-Z][A-Z]+[0-9][0-9][0-9][0-9]$") str 0) 
  then true else false 

(** [sem_id_parse sem_id] parses [sem_id] if it is a valid semester type.
    Raises: Malformed when [sem_id] is not valid. *)
let sem_id_parse sem_id =
  let uppercase_id = String.uppercase_ascii sem_id in
  if Str.string_match (Str.regexp "^SP[0-9][0-9]$") uppercase_id 0 then
    Spring (int_of_string (String.sub sem_id 2 2))
  else if Str.string_match (Str.regexp "^FA[0-9][0-9]$") uppercase_id 0 then 
    Fall (int_of_string (String.sub sem_id 2 2)) 
  else 
    raise MalformedSemId

(** [add_others sch str_lst] parses [str_lst] in [sch] for the Add command. *)
let add_others sch str_lst =
  match str_lst with
  | [] -> raise MalformedAdd
  | sem_id::[] -> add_sem sch (create_sem (sem_id_parse sem_id))
  | course_name::grade::degree::sem_id::[] -> 
    let name = String.uppercase_ascii course_name in
    add_course sch 
      (create_course name 
         (get_course_creds name
            (sem_id_parse sem_id)) 
         (Schedule.gradify grade) degree) 
      (sem_id_parse sem_id)
  | course_name::credits::grade::degree::sem_id::[] -> 
    let name = String.uppercase_ascii course_name in
    add_course sch (create_course name 
                      (int_of_string credits) 
                      (Schedule.gradify grade) degree) 
      (sem_id_parse sem_id)
  | _ -> raise MalformedAdd

(** [edit_others sch str_lst] parses [str_lst] in [sch] for the Edit command. *)
let edit_others sch str_lst =
  match str_lst with
  | [] -> raise MalformedEdit
  | "name"::new_val::[] -> edit_name sch new_val
  | course_name::field::new_val::[] -> 
    edit_course sch (String.uppercase_ascii course_name) field new_val
  | _ -> raise MalformedEdit

(** [remove_others sch str_lst] parses [str_lst] in [sch] for the Remove 
    command. *)
let remove_others sch str_lst =
  match str_lst with
  | [] -> raise MalformedRemove
  | sem_id::[] when is_valid_coursename sem_id = false -> 
    remove_sem sch (sem_id_parse sem_id)
  | course_name::[] -> remove_course sch (String.uppercase_ascii course_name)
  | _ -> raise MalformedRemove

let export_handler sch str_lst = 
  match str_lst with
  | file :: [] -> HTML.export_schedule sch file; sch
  | _ -> raise MalformedExport

let save_handler sch str_lst = 
  match str_lst with
  | file :: [] -> SaveJSON.save_schedule sch file; sch
  | _ -> raise MalformedSave

let load_handler sch str_lst = 
  match str_lst with
  | file :: [] -> sch;
  | _ -> raise MalformedLoad

let parse_command sch cmd_str = 

  let match_helper first others =
    match first with
    | "add" -> add_others sch others
    | "edit" -> edit_others sch others
    | "remove" -> remove_others sch others
    | "save" -> save_handler sch others
    | "load" -> load_handler sch others
    | "export" -> export_handler sch others
    | _ -> raise Malformed
  in

  let split_cmd = String.split_on_char ' ' cmd_str in
  match split_cmd with 
  | [] -> raise Empty
  | "print"::[] -> (print_schedule sch); sch
  | fst::others -> match_helper fst others

