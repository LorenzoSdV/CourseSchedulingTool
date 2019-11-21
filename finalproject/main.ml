open Schedule
open Command
open ClassRoster

let save_prompt_helper sch =
  print_string ("Choose the name of the JSON file you will save this schedule to:\n\n");
  match read_line () with
  | "" -> raise InvalidFileForSave
  | file_name -> SaveJSON.save_schedule sch file_name

let rec save_prompt_from_quit sch =
  ANSITerminal.print_string [Bold] "WARNING: You have unsaved changes. Would you like to save now before closing?\n\n";
  match read_line () with
  | "yes" -> save_prompt_helper sch; set_save_status sch true; 
    ANSITerminal.print_string [Bold] "\nSaved!\n"; 
  | "no" -> Stdlib.exit 0
  | _ -> print_endline ("Type 'yes' or 'no' to continue.\n"); save_prompt_from_quit sch

and save_prompt_from_close sch =
  ANSITerminal.print_string [Bold] "WARNING: You have unsaved changes. Would you like to save now before closing?\n\n";
  match read_line () with
  | "yes" -> save_prompt_helper sch; set_save_status sch true; 
    ANSITerminal.print_string [Bold] "\nSaved!\n"; 
  | "no" -> ANSITerminal.(print_string [cyan] "\nHOMEPAGE\n\n"); init_prompt (read_line ())
  | _ -> print_endline ("Type 'yes' or 'no' to continue."); save_prompt_from_close sch

(** [prompt sch] is the user's interface with our system. This function handles 
    execution of user commands pertaining to [sch]. Also handles any exceptions 
    raised during the execution of any commands. *)
and prompt sch =
  ANSITerminal.(print_string [green] ("\n" ^ (get_name sch) ^ ": "));
  match read_line () with 
  | exception End_of_file -> ()
  | "quit" -> if get_save_status sch then Stdlib.exit 0 else save_prompt_from_quit sch
  | "clear" -> ignore (Sys.command "clear"); prompt sch
  | "close" -> 
    begin if get_save_status sch 
      then (ANSITerminal.(print_string [cyan] "\nHOMEPAGE\n\n"); init_prompt (read_line ())) 
      else save_prompt_from_close sch; init_prompt (read_line ())
    end
  | "" -> begin
      print_endline "Valid Commands: add | edit | remove | save | print | export | clear | close | quit";
      print_endline "Enter a command to view usage instructions.";
      prompt sch
    end
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
    | ClassRoster.InvalidURL -> 
      exceptions sch "Error Retrieving Course Info from Online"
    | InvalidFileForExport ->
      exceptions sch "File path cannot be a JSON. Try again."
    | InvalidFileForSave -> 
      exceptions sch "File path must be a JSON. Try again." 
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
      exceptions sch "Usage: export <html_file_name>"
    | MalformedSave ->
      exceptions sch "Usage: save <json_file>"
    | Malformed | _ -> 
      exceptions sch 
        ("Unrecognized Command Entry!\n" ^ 
         "Valid Commands: add | edit | remove | save | print | export | clear | " ^
         "close | quit\n")

(** [exceptions sch err] prints the promper error message [err] and reloads
    the prompt for the user. *)
and exceptions sch err = 
  ANSITerminal.(print_string [red] "Invalid\n"); 
  print_endline err;
  prompt sch

and load file =
  try 
    prompt (LoadJSON.parse_json file)
  with
  | Sys_error _ -> print_string ("\nInvalid/Unknown JSON file.\n"); 
    init_prompt (read_line ())

and init_prompt init_str =
  let split_cmd = String.split_on_char ' ' init_str in
  match split_cmd with 
  | [] -> raise Empty
  | "new"::sch_name::[] -> prompt (edit_name Schedule.new_schedule sch_name)
  | "load"::json::[] -> load json
  | "quit"::[] -> Stdlib.exit 0
  | _ -> print_string ("Unrecognized Command Entry!\n" ^ 
                       "Valid Commands: [new <schedule_name>] | [load <json_file>] | quit\n");
    init_prompt (read_line ())

let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 3110 Project Schedule Planning Tool\n");
  print_endline 
    ("If you want to open an alredy existing schedule, type 'load' " ^
     "<json_file>, or type 'new' <schedule_name> to create a new schedule " ^
     "(NOTE: The name of the schedule must not have any spaces!).\n");
  init_prompt (read_line ())

(* Starts system *)
let () = main ()