open Schedule
open Command
open ClassRoster
open JSON



(** [prompt sch] is the user's interface with our system. This function handles 
    execution of user commands pertaining to [sch]. Also handles any exceptions 
    raised during the execution of any commands. *)
let rec prompt sch =
  ANSITerminal.(print_string [green] ("\n" ^ (get_name sch) ^ ": "));
  match read_line () with
  | exception End_of_file -> ()
  | "quit" -> Stdlib.exit 0
  | "export" -> Schedule.HTML.export_schedule sch "test.html"; prompt sch
  | "clear" -> ignore (Sys.command "clear"); prompt sch
  | "close" -> init_prompt (read_line ()) (* need to add WARNING about save *)
  | "" -> 
    print_endline "Valid Commands: add | edit | remove | print | export | clear | close | quit";
    print_endline "Enter a command to view usage instructions.";
    prompt sch
  | string_cmd -> 
    try
      prompt (parse_command sch string_cmd)
    with
    | UnknownCourse msg -> 
      exceptions sch ("Invalid/Unknown Course: " ^ msg)
    | UnknownSemester msg -> 
      exceptions sch ("Invalid/Unknown Semester: " ^ msg)
    | UnknownGrade msg -> 
      exceptions sch ("Invalid/Unknown Grade Value: " ^ msg)
    | DuplicateCourse msg -> 
      exceptions sch ("Duplicate: Course Already Exists: " ^ msg)
    | DuplicateSemester msg -> 
      exceptions sch ("Duplicate: Semester Already Exists: " ^ msg)
    | InvalidURL -> 
      exceptions sch "Error Retrieving Course Info from Online"
    | MalformedSemId -> 
      exceptions sch ("Incorrect Semester Entry Format: " ^
                      "use 'fa18' for fall 2018 and 'sp22' for spring 2022")
    | MalformedAdd ->
      exceptions sch ("Usage: add [<course_name> [(optional: <credits>) <grade>"
                      ^ " <category> <semester>] | <semester>]")
    | MalformedEdit ->
      exceptions sch ("Usage: edit [<course_name> <field> <new_value> | " ^ 
                      "name <new_name>]")
    | MalformedRemove ->
      exceptions sch "Usage: remove [<course_name> | <semester>]"
    | Malformed | _ -> 
      exceptions sch 
        ("Unrecognized Command Entry!\n" ^ 
         "Valid Commands: add | edit | remove | print | export | clear | close | quit")

(** [exceptions sch err] prints the promper error message [err] and reloads
    the prompt for the user. *)
and exceptions sch err = 
  ANSITerminal.(print_string [red] "Invalid\n"); 
  print_endline err;
  prompt sch

(** Loads a file and makes a schedule out of it *)
and load f =
  try let verified_f = Yojson.Basic.from_file f in 
    prompt (parse_json (verified_f))
  with
  | Sys_error _ -> print_string ("\nInvalid/Unknown json file.\n"); 
    init_prompt (read_line ())

and init_prompt init_str =
  let split_cmd = String.split_on_char ' ' init_str in
  match split_cmd with 
  | [] -> raise Empty
  | "new"::sch_name::[] -> prompt (edit_name Schedule.new_schedule sch_name)
  | "load"::json::[] -> load json
  | "quit"::[] -> Stdlib.exit 0
  | _ -> print_string ("Unrecognized Command Entry!\n" ^ 
                       "Valid Commands: new | [load <json_file>] | quit");
    init_prompt (read_line ())

let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 3110 Project Schedule Planning Tool\n");
  print_endline ("If you want to open an alredy existing schedule, type 'load' <json_file>, or type " ^
                 "'new' to create a new schedule.\n");
  print_string  "> ";
  init_prompt (read_line ())

(* Starts system *)
let () = main ()