open Schedule
open Command
open ClassRoster

(** [valid_commands] is a textual representation of a list of valid commands
    the user can enter in [prompt] *)
let valid_commands = 
  ("Valid Commands: add | edit | remove | swap | move | check | save |" ^
   " set | print | import | export | delete | clear | close | quit")

(** [read_input ()] is [read_line ()] after displaying helpful text prompt
    indicator. *)
let read_input () = 
  print_string "\n> ";
  read_line ()

(** [save_prompt_helper sch] is [sch] after saving [sch] as a json file. *)
let save_prompt_helper sch =
  print_endline 
    ("\nChoose the name of the JSON file you will save this schedule to:");
  save_handler sch (String.split_on_char ' ' (read_input ()))

(** [save_prompt_from_quit sch] prompts the user whether they want to save
    [sch] or not after the Quit command. *)
let rec save_prompt_from_quit sch =
  ANSITerminal.print_string [Bold] "Warning: ";
  print_endline 
    "You have unsaved changes. Would you like to save now before quitting?";
  match read_input () with
  | "cancel" -> prompt sch
  | "yes" -> ignore(save_prompt_helper sch); set_save_status sch true; 
    ANSITerminal.print_string [Bold] "\nSaved!\n";
    start_prompt ()
  | "no" -> Stdlib.exit 0
  | _ -> 
    print_endline ("Type 'yes' or 'no' to continue. 'cancel' to undo this " ^
                   "command."); 
    save_prompt_from_quit sch

(** [save_prompt_from_close sch] prompts the user whether they want to save 
    [sch] or not after the Close command. *)
and save_prompt_from_close sch =
  ANSITerminal.print_string [Bold] "Warning: ";
  print_endline 
    "You have unsaved changes. Would you like to save now before closing?";
  match read_input () with
  | "cancel" -> prompt sch
  | "yes" -> ignore(save_prompt_helper sch); set_save_status sch true; 
    ANSITerminal.print_string [Bold] "\nSaved!\n"; 
    start_prompt ()
  | "no" -> start_prompt ()
  | _ -> 
    print_endline ("Type 'yes' or 'no' to continue. 'cancel' to undo this " ^
                   "command."); 
    save_prompt_from_close sch

(** [delete_prompt sch] is [prompt sch'] where [sch'] is either [sch] or a 
    new empty schedule depending on user's response to prompt. *)
and delete_prompt sch = 
  if get_save_status sch 
  then
    (ANSITerminal.print_string [Bold] "\nWarning: ";
     print_endline "You did not make any changes to this schedule yet.\n";
     print_endline "Are you sure you still want to delete?";
     match read_input () with
     | "yes" ->
       ANSITerminal.print_string [Bold] "\nErased!\n";
       prompt (new_schedule (get_name sch))
     | "no" -> prompt sch
     | _ -> 
       print_endline ("Type 'yes' or 'no' to continue."); 
       delete_prompt sch)
  else
    begin
      ANSITerminal.print_string [Bold] "\nAre you sure?\n";
      print_endline 
        "This will erase all information from the current schedule.";
      match read_input () with
      | "yes" -> 
        ANSITerminal.print_string [Bold] "\nErased!\n";
        prompt (new_schedule (get_name sch))
      | "no" -> prompt sch
      | _ -> 
        print_endline ("Type 'yes' or 'no' to continue."); 
        delete_prompt sch
    end

(** [prompt sch] is the user's interface with our system. This function handles 
    execution of user commands pertaining to [sch]. Also handles any exceptions 
    raised during the execution of any commands. *)
and prompt sch =
  ANSITerminal.(print_string [green] ("\n" ^ (get_name sch) ^ ": "));
  match read_line () with 
  | exception End_of_file -> ()
  | "clear" -> ignore (Sys.command "clear"); prompt sch
  | "quit" -> 
    if get_save_status sch then Stdlib.exit 0 
    else save_prompt_from_quit sch
  | "delete" -> delete_prompt sch
  | "close" -> 
    if get_save_status sch then start_prompt ()
    else save_prompt_from_close sch; init_prompt ()
  | "" -> begin
      print_endline valid_commands;
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
      exceptions sch ("Invalid/Unknown Grade: " ^ msg ^ 
                      "\nValid grades: <letter_grade>, s/sat, u/unsat, "^
                      "w/withdrawn, inc/incomplete, none, transfer")
    | UnknownCategoryCAS msg ->
      exceptions sch ("Invalid/Unknown CAS Category: " ^ msg ^
                      "\nValid CAS Categories: PE, FWS, req/required, core, " ^ 
                      "4000+, tech/technical, spcl/ext, maj/major, " ^ 
                      "project/proj/practicum/pract, ENGRD, ENGRI, liberal, "^
                      "aprv/advisor, extra")
    | UnknownCategoryENG msg ->
      exceptions sch ("Invalid/Unknown ENG Category: " ^ msg ^
                      "\nValid ENG Categories: PE, FWS, req/required, core, " ^ 
                      "4000+, tech/technical, spcl/ext, maj/major, " ^ 
                      "project/proj/practicum/pract, lang/language/foreign, "^
                      "PBS/PBSS, MQR/CA/HA/KCM/LA/SBA/liberal, GB, HB, "^
                      "extra")
    | UnknownSetting msg ->
      exceptions sch ("Invalid/Unknown Setting Attribute: " ^ msg)
    | DuplicateCourse msg -> 
      exceptions sch ("Duplicate Course Already Exists: " ^ msg)
    | DuplicateSemester msg -> 
      exceptions sch ("Duplicate Semester Already Exists: " ^ msg)
    | InvalidAttribute msg ->
      exceptions sch ("Invalid/Unknown Edit Attribute: " ^ msg ^
                      "\nValid attributes: credits, grade, or category.")
    | InvalidSwap ->
      exceptions sch 
        "Cannot swap course with itself or courses that are in same semester."
    | InvalidMove ->
      exceptions sch
        "This course is already located in this semester."
    | ClassRoster.InvalidURL -> 
      exceptions sch "Error Retrieving Course Info from Online"
    | InvalidFileForExport ->
      exceptions sch "File path cannot be a JSON. Try again."
    | InvalidFileForImport ->
      exceptions sch "File must be an iCal in current directory. Try again."
    | InvalidFileForSave -> 
      exceptions sch "File path must be a JSON in current directory. Try again." 
    | SemesterDoesNotExist ->
      exceptions sch "This semester does not exist in your schedule."
    | MalformedSemId -> 
      exceptions sch ("Incorrect Semester Entry Format: " ^
                      "Eg; use 'fa18' for fall 2018 and 'sp22' for spring 2022")
    | MalformedAdd ->
      exceptions sch ("Usage: add [<course_name> (optional: <credits>) <grade>"
                      ^ " (optional: <category>) <semester> | <semester>]")
    | MalformedEdit ->
      exceptions sch ("Usage: edit [<course_name> <field> <new_value> | " ^ 
                      "name <new_name> | school <ENG | CAS> ]")
    | MalformedRemove ->
      exceptions sch "Usage: remove [<course_name> | <semester>]"
    | MalformedExport ->
      exceptions sch "Usage: export <html_file_name>"
    | MalformedSave ->
      exceptions sch "Usage: save <json_file>"
    | MalformedSwap -> 
      exceptions sch "Usage: swap <course_name> <course_name>"
    | MalformedMove ->
      exceptions sch "Usage: move <course_name> <new_semester>"
    | MalformedPrint ->
      exceptions sch "Usage: print [<> | <course_name>]"
    | MalformedSet ->
      exceptions sch "Usage: set <attribute> <new_value>"
    | MalformedImport -> 
      exceptions sch "Usage: import <ics_file>"
    | Malformed | Empty -> 
      exceptions sch 
        ("Unrecognized Command Entry!\n" ^ valid_commands)

(** [exceptions sch err] is [prompt sch] after printing error message for 
    [err]. *)
and exceptions sch err = 
  ANSITerminal.(print_string [red] "Invalid\n"); 
  print_endline err;
  prompt sch

(** [load file_lst] is [prompt sch] where [sch] was parsed from JSON file 
    [file_list]. If [file_list] doesn't point to a valid JSON schedule file, 
    [load file_lst] is [init_pompt ()]. *)
and load (file_lst: string list) =
  try 
    let file_extra_space = 
      List.fold_left (fun acc str -> acc ^ str ^ " ") "" file_lst in
    let file = 
      String.sub file_extra_space 0 (String.length file_extra_space - 1) in
    let sch = LoadJSON.parse_json file in
    prompt sch
  with
  | _ -> print_string ("\nInvalid/Unknown JSON file.\n"); init_prompt ()

(** [init_prompt ()] is the initial user prompt and first entry into the 
    system. *)
and init_prompt () =
  let split_cmd = String.split_on_char ' ' (read_input ()) in
  match split_cmd with 
  | [] -> raise Empty
  | "new"::sch_name when sch_name <> [] -> begin
      let sch_extra_space = 
        List.fold_left (fun acc str -> acc ^ str ^ " ") "" sch_name in
      let new_name = 
        String.sub sch_extra_space 0 (String.length sch_extra_space - 1) in
      if (Str.string_match (Str.regexp "^\s") new_name 0)  
      then 
        begin
          print_endline("\nThe following commands are available for use. Type" 
                        ^ " in any command to see usage instructions.");
          ANSITerminal.(print_string [yellow] valid_commands);
          print_endline("\nThe following are the grade options when adding a " ^
                        "new course to the schedule.");
          ANSITerminal.(print_string [yellow] 
                          ("Valid grades: <letter_grade>, s/sat, " 
                           ^ "u/unsat, w/withdrawn, inc/incomplete, " ^ 
                           "none, transfer\n"));
          print_string("\n");
          prompt (new_schedule new_name)
        end
      else 
        begin
          ANSITerminal.(print_string [red] 
                          ("\nSchedule name cannot contain only"
                           ^" whitespaces!\n"));
          print_endline 
            "Valid commands: [new <schedule_name>] | [load <json_file>] | quit";
          init_prompt ()
        end
    end
  | "load"::json_lst when json_lst <> [] -> load json_lst
  | "quit"::[] -> Stdlib.exit 0
  | _ -> 
    ANSITerminal.(print_string [red] "\nUnrecognized Command Entry!\n");
    print_endline 
      "Valid commands: [new <schedule_name>] | [load <json_file>] | quit";
    init_prompt ()

(** [start_prompt ()] is the low-level prompt the user is taken to when not
    working within an active schedule. *)
and start_prompt () =
  ANSITerminal.(print_string [cyan] "\nStart Page\n"); 
  print_endline 
    "Valid commands: [new <schedule_name>] | [load <json_file>] | quit";
  init_prompt ()

let main () =
  ignore(Sys.command "clear");
  ANSITerminal.(print_string [red]
                  "Welcome to the 3110 Project Schedule Planning Tool\n");
  print_endline 
    "If you want to open an already existing schedule, type [load <json_file>]";
  print_endline 
    "Or type [new <schedule_name>] to create a new schedule.";
  init_prompt ()

(* Starts system *)
let () = main ()