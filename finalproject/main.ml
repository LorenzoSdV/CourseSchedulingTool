open Schedule
open Command
open ClassRoster

(** [prompt sch] is the prompt where the user can enter a command to interact
    with current schedule [sch]. *)
let rec prompt sch =
  ANSITerminal.(print_string [green] ("\n" ^ (get_name sch) ^ ": "));
  match read_line () with
  | exception End_of_file -> ()
  | "quit" -> Stdlib.exit 0
  | "" -> print_endline "Valid Commands: add | edit | remove | print | quit";
    prompt sch
  | string_cmd -> 
    try
      prompt (parse_command sch string_cmd)
    with
    | UnknownCourse msg -> 
      exceptions sch ("Invalid or Unkown Course: " ^ msg)
    | UnknownSemester msg -> 
      exceptions sch ("Invalid or Unkown Semester: " ^ msg)
    | UnkownGrade msg -> 
      exceptions sch ("Invalid or Unkown Grade Value: " ^ msg)
    | DuplicateCourse msg -> 
      exceptions sch ("Duplicate Course Already Exists: " ^ msg)
    | DuplicateSemester msg -> 
      exceptions sch ("Duplicate Semester Already Exists: " ^ msg)
    | InvalidURL -> 
      exceptions sch "Error Retreiving Course Info from Online"
    | MalformedSemId -> 
      exceptions sch "Improperly Formatted (Unrecognized) Semester Entry"
    | MalformedAdd ->
      exceptions sch ("Usage: add [<course_name>|sem <sem_id>] [<credits> " ^ 
                      "<grade> <category> <semester>]")
    | MalformedEdit ->
      exceptions sch "Usage: edit [course|sem|schedule] <attribute> <new_value)"
    | MalformedRemove ->
      exceptions sch "Usage: remove [course|sem] <name>"
    | Malformed | _ -> exceptions sch "Unrecognized Command Entry!"

(** [exceptions sch] handles any exceptions raised during a call to
    [execute adv st cmd]. Gives the user a nice message. *)
and exceptions sch err = 
  ANSITerminal.(print_string [red] "Invalid\n"); 
  print_endline err;
  prompt sch

(** Loads a file and makes a schedule out of it *)
let load f =
  print_endline "This feature will be implemented in the second sprint. 
  Now creating a new schedule.\n";
  prompt Schedule.new_schedule

let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 3110 Project Schedule Planning Tool\n");
  print_endline "Please enter path to schedule you want to load, or leave 
  blank to create new schedule.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "" -> prompt Schedule.new_schedule
  | "quit" -> Stdlib.exit 0
  | file_name -> load file_name

(* Start running the user prompt: *)
let () = main ()