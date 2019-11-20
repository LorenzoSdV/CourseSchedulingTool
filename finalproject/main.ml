open Schedule
open Command
open ClassRoster



(** [prompt sch] is the user's interface with our system. This function handles 
    execution of user commands pertaining to [sch]. Also handles any exceptions 
    raised during the execution of any commands. *)
let rec prompt sch =
  ANSITerminal.(print_string [green] ("\n" ^ (get_name sch) ^ ": "));
  match read_line () with
  | exception End_of_file -> ()
  | "quit" -> Stdlib.exit 0
  | "clear" -> ignore (Sys.command "clear"); prompt sch
  (*| "close" -> main () (* need to add WARNING about save *) *)
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
    | InvalidFile ->
      exceptions sch "File path is not valid. Try again."
    | MalformedSemId -> 
      exceptions sch ("Incorrect Semester Entry Format: " ^
                      "Eg; use 'fa18' for fall 2018 and 'sp22' for spring 2022")
    | MalformedAdd ->
      exceptions sch ("Usage: add [<course_name> [(optional: <credits>) <grade>"
                      ^ " <category> <semester>] | <semester>]")
    | MalformedEdit ->
      exceptions sch ("Usage: edit [<course_name> <field> <new_value> | " ^ 
                      "name <new_name>]")
    | MalformedRemove ->
      exceptions sch "Usage: remove [<course_name> | <semester>]"
    | MalformedExport ->
      exceptions sch "Usage: export <file_path>"
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

let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 3110 Project Schedule Planning Tool\n");
  print_endline ("If you want to open an alredy existing schedule, type the filepath, otherwise leave blank for new schedule.");
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "" -> prompt Schedule.new_schedule
  | "quit" -> Stdlib.exit 0
  | file_name -> prompt (LoadJSON.parse_json file_name)

(* Starts system *)
let () = main ()