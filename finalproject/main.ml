open Schedule
open Command
open ClassRoster

let read_input () = 
  print_string "\n> ";
  read_line ()

let save_prompt_helper sch =
  print_endline 
    ("\nChoose the name of the JSON file you will save this schedule to:");
  match read_input () with
  | "" -> raise InvalidFileForSave
  | file_name -> SaveJSON.save_schedule sch file_name

let rec save_prompt_from_quit sch =
  ANSITerminal.print_string [Bold] "Warning: ";
  print_endline 
    "You have unsaved changes. Would you like to save now before quitting?";
  match read_input () with
  | "yes" -> save_prompt_helper sch; set_save_status sch true; 
    ANSITerminal.print_string [Bold] "\nSaved!\n";
    start_prompt ()
  | "no" -> Stdlib.exit 0
  | _ -> 
    print_endline ("Type 'yes' or 'no' to continue."); 
    save_prompt_from_quit sch

and save_prompt_from_close sch =
  ANSITerminal.print_string [Bold] "Warning: ";
  print_endline 
    "You have unsaved changes. Would you like to save now before closing?";
  match read_input () with
  | "yes" -> save_prompt_helper sch; set_save_status sch true; 
    ANSITerminal.print_string [Bold] "\nSaved!\n"; 
    start_prompt ()
  | "no" -> start_prompt ()
  | _ -> 
    print_endline ("Type 'yes' or 'no' to continue."); 
    save_prompt_from_close sch

(** [prompt sch] is the user's interface with our system. This function handles 
    execution of user commands pertaining to [sch]. Also handles any exceptions 
    raised during the execution of any commands. *)
and prompt sch =
  ANSITerminal.(print_string [green] ("\n" ^ (get_name sch) ^ ": "));
  match read_line () with 
  | exception End_of_file -> ()
  | "quit" -> 
    if get_save_status sch then Stdlib.exit 0 
    else save_prompt_from_quit sch
  | "clear" -> ignore (Sys.command "clear"); prompt sch
  | "close" -> 
    if get_save_status sch then start_prompt ()
    else save_prompt_from_close sch; init_prompt ()
  | "" -> begin
      print_endline ("Valid Commands: add | edit | remove | save |" ^
                     " print | export | clear | close | quit");
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
    | SemesterDoesNotExist ->
      exceptions sch "This semester does not exist in your schedule."
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
         "Valid Commands: add | edit | remove | save | print | export | " ^ 
         "clear | close | quit")

(** [exceptions sch err] prints the promper error message [err] and reloads
    the prompt for the user. *)
and exceptions sch err = 
  ANSITerminal.(print_string [red] "Invalid\n"); 
  print_endline err;
  prompt sch

and load (file_lst: string list) =
  try 
    let file_extra_space = 
      List.fold_left (fun acc str -> acc ^ str ^ " ") "" file_lst in
    let file = 
      String.sub file_extra_space 0 (String.length file_extra_space - 1) in
    let sch = LoadJSON.parse_json file in
    prompt sch
  with
  | _ -> print_string ("\nInvalid/Unknown JSON file.\n"); 
    init_prompt ()

and init_prompt () =
  let split_cmd = String.split_on_char ' ' (read_input ()) in
  match split_cmd with 
  | [] -> raise Empty
  | "new"::sch_name when sch_name <> [] -> begin
      let sch_extra_space = 
        List.fold_left (fun acc str -> acc ^ str ^ " ") "" sch_name in
      let new_name = 
        String.sub sch_extra_space 0 (String.length sch_extra_space - 1) in
      prompt (edit_name Schedule.new_schedule new_name)
    end
  | "load"::json_lst when json_lst <> [] -> load json_lst
  | "quit"::[] -> Stdlib.exit 0
  | _ -> 
    ANSITerminal.(print_string [red] "\nUnrecognized Command Entry!\n");
    print_endline 
      "Valid Commands: [new <schedule_name>] | [load <json_file>] | quit";
    init_prompt ()

and start_prompt () =
  ANSITerminal.(print_string [cyan] "\nSTART PAGE\n"); 
  print_endline 
    "Valid Commands: [new <schedule_name>] | [load <json_file>] | quit";
  init_prompt ()

let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 3110 Project Schedule Planning Tool\n");
  print_endline 
    "If you want to open an already existing schedule, type [load <json_file>]";
  print_endline 
    "Or type [new <schedule_name>] to create a new schedule.";
  init_prompt ()

(* Starts system *)
let () = main ()