open Schedule
open ClassRoster

exception Empty

exception Malformed
exception MalformedSemId
exception MalformedAdd
exception MalformedEdit
exception MalformedRemove
exception MalformedSwap
exception MalformedMove
exception MalformedSave
exception MalformedExport
exception MalformedImport
exception MalformedPrint
exception MalformedSet

exception InvalidFileForExport
exception InvalidFileForImport
exception InvalidFileForSave

exception SemesterDoesNotExist

(** [is_valid_coursename str] is [true] if [str] has the correct format of a 
    Cornell class. *)
let is_valid_coursename str =
  if (Str.string_match (Str.regexp "^[A-Z][A-Z]+[0-9][0-9][0-9][0-9]$") str 0) 
  then true else false 

(** [sem_id_parse semid] is [sem_id] where [semid] is the string 
    representation of [sem_id]. 
    Raises: [MalformedSemId] if [semid] is not a proper string rep. *)
let sem_id_parse sem_id =
  let uppercase_id = String.uppercase_ascii sem_id in
  if Str.string_match (Str.regexp "^SP[0-9][0-9]$") uppercase_id 0 then
    Spring (int_of_string (String.sub sem_id 2 2))
  else if Str.string_match (Str.regexp "^FA[0-9][0-9]$") uppercase_id 0 then 
    Fall (int_of_string (String.sub sem_id 2 2)) 
  else 
    raise MalformedSemId

(** [sem_exists sem_id_list sem_id] is [()] if [sem_id] is in [sem_id_list]. 
    Rasies [SemesterDoesNotExist] otherwise. *)
let rec sem_exists sem_id_lst sem_id =
  match sem_id_lst with 
  | [] -> raise SemesterDoesNotExist
  | sem::others when sem = (String.uppercase_ascii sem_id) -> ()
  | sem::others -> sem_exists others sem_id

(** [format_sem_id str] is [true] if [str] conforms to a string representation
    of a semester id. *)
let format_sem_id str =
  let id = String.uppercase_ascii(String.sub str 0 2) in
  if id = "SP" || id = "FA" then true else false

(** [guess_deg c_name] is the calculated estimate of a c_name's category 
    based on c_name. *)
let guess_deg c_name = 
  if String.sub c_name 0 2 = "PE" then "PE"
  else if String.sub c_name 0 5 = "ENGRI" then "ENGRI"
  else if String.sub c_name 0 5 = "ENGRD" || c_name = "CS2110" || 
          c_name = "CS2112" then "ENGRD"
  else if String.sub c_name ((String.length c_name) - 1) 1 = "1" then "PRACT"
  else if c_name = "CS1110" || c_name = "CS1112" || c_name = "MATH1910" ||
          c_name = "MATH1920" || c_name = "MATH2940" || c_name = "CHEM2090" ||
          c_name = "CHEM2080" || c_name = "CHEM2150" || c_name = "BTRY3080" || 
          c_name = "ECON3130" || c_name = "MATH2930" || c_name = "MATH4710" || 
          c_name = "PHYS2214" || c_name = "PHYS2218" || c_name = "PHYS1112" ||
          c_name = "PHYS116" || c_name = "PHYS2213" || c_name = "PHYS2217" 
  then "REQUIRED"
  else if c_name = "CS2800" || c_name = "CS2802" || c_name = "CS3110" || 
          c_name = "CS3410" || c_name = "CS3420" || c_name = "ECE3140" || 
          c_name = "CS4410" || c_name = "CS4280"
  then "CORE"
  else if 
    (String.sub c_name 0 3 = "CS4" && (c_name <> "CS4090") 
     && (c_name <> "CS4998") && (c_name <> "CS4999")) 
    || String.sub c_name 0 3 = "CS5" 
  then "4000+" 
  else if String.sub c_name 0 5 = "ECON3" || String.sub c_name 0 5 = "ECON4" 
          || String.sub c_name 0 5 = "ECON5" || String.sub c_name 0 5 = "MATH3" 
          || String.sub c_name 0 5 = "MATH4" || String.sub c_name 0 5 = "MATH5"
          || String.sub c_name 0 5 = "CHEM3" || String.sub c_name 0 5 = "CHEM4"
          || String.sub c_name 0 5 = "CHEM5" || String.sub c_name 0 4 = "BIO3"
          || String.sub c_name 0 4 = "BIO4" || String.sub c_name 0 4 = "BIO5"
          || String.sub c_name 0 3 = "CS3" || c_name = "ENGRD2700" 
          || c_name = "MATH2930"
  then "TECH"
  else 
  if not(Str.string_match 
           (Str.regexp "^[A-Z][A-Z]+[0-2][0-9][0-9][0-9]$") c_name 0)
  then "SPCL"
  else "LIBERAL"

(** [add_others sch str_lst] is [sch] after parsing [str_lst] and adding a 
    new course if [str_lst] is properly formatted. 
    Raises: [MalformedAdd] if [str_lst] not properly formatted. *)
let add_others sch str_lst =
  match str_lst with
  | [] -> raise MalformedAdd
  | sem_id::[] when format_sem_id sem_id -> 
    add_sem sch (create_sem (sem_id_parse sem_id))
  | course_name::grade::sem_id::[] ->
    (sem_exists (sem_ids_to_string sch) sem_id);
    let name = String.uppercase_ascii course_name in
    let guessed_deg = guess_deg name in
    print_endline ("Category Estimation: " ^ guessed_deg);
    add_course sch 
      (create_course name 
         (get_course_creds name 
            (sem_id_parse sem_id)) 
         (Schedule.gradify grade) (guess_deg name)) 
      (sem_id_parse sem_id)
  | course_name::credits::grade::sem_id::[] 
    when Str.string_match (Str.regexp "^[0-9]+$") credits 0 ->
    (sem_exists (sem_ids_to_string sch) sem_id); 
    let name = String.uppercase_ascii course_name in
    let guessed_deg = guess_deg name in
    print_endline ("Category Estimation: " ^ guessed_deg);
    add_course sch (create_course name
                      (int_of_string credits)
                      (Schedule.gradify grade) guessed_deg)
      (sem_id_parse sem_id)
  | course_name::grade::degree::sem_id::[] ->
    (sem_exists (sem_ids_to_string sch) sem_id);
    let name = String.uppercase_ascii course_name in
    add_course sch 
      (create_course name 
         (get_course_creds name 
            (sem_id_parse sem_id)) 
         (Schedule.gradify grade) (String.uppercase_ascii degree)) 
      (sem_id_parse sem_id)
  | course_name::credits::grade::degree::sem_id::[] ->
    (sem_exists (sem_ids_to_string sch) sem_id);
    let name = String.uppercase_ascii course_name in
    add_course sch (create_course name 
                      (int_of_string credits) 
                      (Schedule.gradify grade) (String.uppercase_ascii degree)) 
      (sem_id_parse sem_id)
  | _ -> raise MalformedAdd

(** [add_others sch str_lst] is [sch] after parsing [str_lst] and editing a 
    course value if [str_lst] is properly formatted. 
    Raises: [MalformedEdit] if [str_lst] not properly formatted. *)
let edit_others sch str_lst =
  match str_lst with
  | [] -> raise MalformedEdit
  | "name"::new_val::[] -> edit_name sch new_val
  | "school"::school::[] -> begin
      let school' = (String.uppercase_ascii school) in
      if (school' = "CAS" || school' = "ENG") then
        (set_school sch school'; sch)
      else
        raise MalformedEdit
    end
  | course_name::field::new_val::[] ->
    edit_course sch (String.uppercase_ascii course_name) field new_val
  | _ -> raise MalformedEdit

(** [remove_others sch str_lst] is [sch] after parsing [str_lst] and removing a 
    new course if [str_lst] is properly formatted. 
    Raises: [MalformedRemove] if [str_lst] not properly formatted. *)
let remove_others sch str_lst =
  match str_lst with
  | [] -> raise MalformedRemove
  | sem_id::[] when format_sem_id sem_id && is_valid_coursename sem_id = false-> 
    remove_sem sch (sem_id_parse sem_id)
  | course_name::[] -> remove_course sch (String.uppercase_ascii course_name)
  | _ -> raise MalformedRemove

(** [is_not_json file] is [false] if [file] has the extension ".json".
    Raises: [InvalidFileForExport] if [file] is a .json. *)
let is_not_json file =
  match String.split_on_char '.' file with
  | name::extension::[] when extension <> "json" -> true
  | _ -> raise InvalidFileForExport

(** [export_handler sch str_lst] is [sch] after parsing [str_lst] and exporting
    [sch] as HTML file if [str_lst] is properly formatted. 
    Raises: [MalformedExport] if [str_lst] not properly formatted.
    Raises: [InvalidFileForExport] if filenmae given in [str_lst] isnt valid. *)
let export_handler sch str_lst = 
  (if get_school sch = "ENG" then
     ignore (Requirements.validate sch Requirements.eng_reqs)
   else
     ignore (Requirements.validate sch Requirements.cas_reqs));
  match str_lst with
  | file :: [] -> if is_not_json file
    then (HTML.export_schedule sch file; sch) 
    else raise InvalidFileForExport
  | _ -> raise MalformedExport

(** [import_handler sch str_lst] is [sch] after parsing [str_lst] and importing
    an iCal file if [str_lst] is properly formatted. 
    Raises: [MalformedImport] if [str_lst] not properly formatted.
    Raises: [InvalidFileForImport] if filenmae given in [str_lst] isnt valid. *)
let import_handler sch str_lst = 
  match str_lst with
  | file :: [] -> begin
      let courses, semid = 
        try ICalParser.parse_file file 
        with Sys_error _ -> 
          raise InvalidFileForImport 
      in 
      let sch' = 
        try (sem_exists (sem_ids_to_string sch) semid); sch
        with SemesterDoesNotExist -> 
          add_sem sch (create_sem (sem_id_parse semid))
      in
      List.fold_left 
        (fun acc name -> 
           try add_course acc 
                 (create_course name
                    (get_course_creds name 
                       (sem_id_parse semid))
                    (gradify "none") "None") 
                 (sem_id_parse semid) 
           with DuplicateCourse _ -> acc) sch' courses
    end
  | _ -> raise MalformedImport

(** [is_json file] is [true] if [file] has the extension .json.
    Raises: InvalidFileForSave if [file] is not a .json. *)
let is_json file =
  match String.split_on_char '.' file with
  | name::extension::[] when name <> "" && extension = "json" -> true
  | _ -> raise InvalidFileForSave

(** [save_handler sch str_lst] is [sch] after parsing [str_lst] and saving [sch]
    as JSON file if [str_lst] is properly formatted. 
    Raises: [MalformedSave] if [str_lst] not properly formatted.
    Raises: [InvalidFileForSave] if filenmae given in [str_lst] isnt valid. *)
let save_handler sch str_lst = 
  match str_lst with
  | file :: [] -> if is_json file 
    then (SaveJSON.save_schedule sch file;  
          ANSITerminal.print_string [Bold] "\nSaved!\n"; sch) 
    else raise InvalidFileForSave
  | _ -> raise MalformedSave

(** [swap_others sch str_lst] is [sch] after parsing [str_lst] and swapping
    courses between semesters if [str_lst] is properly formatted. 
    Raises: [MalformedSwap] if [str_lst] not properly formatted. *)
let swap_others sch str_lst =
  match str_lst with
  | course1::course2::[] -> 
    swap_courses (String.uppercase_ascii course1) 
      (String.uppercase_ascii course2) sch
  | _ -> raise MalformedSwap

(** [move_others sch str_lst] is [sch] after parsing [str_lst] and moving course
    to a new semester if [str_lst] is properly formatted. 
    Raises: [MalformedMove] if [str_lst] not properly formatted. *)
let move_others sch str_lst =
  match str_lst with
  | course::sem::[] -> (sem_exists (sem_ids_to_string sch) sem); 
    move_course (String.uppercase_ascii course) (sem_id_parse sem) sch
  | _ -> raise MalformedMove

(** [settings_handler sch str_lst] is [sch] after parsing [str_lst] and changing
    settings in [sch] if [str_lst] is properly formatted. 
    Raises: [MalformedSet] if [str_lst] not properly formatted. *)
let settings_handler sch str_lst = 
  match str_lst with
  | attr::new_val::[] -> edit_settings sch attr new_val
  | _ -> raise MalformedSet

(** [validate_handler sch] is [sch] after chekcing updating validity of 
    schedule. *)
let validate_handler sch = 
  if get_school sch = "ENG" then
    (Requirements.validate sch Requirements.eng_reqs
     |> Requirements.print_validation;
     sch)
  else
    (Requirements.validate sch Requirements.cas_reqs
     |> Requirements.print_validation;
     sch)


let parse_command sch cmd_str = 

  let match_helper first others =
    match first with
    | "add" -> add_others sch others
    | "edit" -> edit_others sch others
    | "remove" -> remove_others sch others
    | "swap" -> swap_others sch others
    | "move" -> move_others sch others
    | "save" -> save_handler sch others
    | "export" -> export_handler sch others
    | "import" -> import_handler sch others
    | "set" -> settings_handler sch others
    | "check" -> validate_handler sch
    | _ -> raise Malformed
  in

  let split_cmd = String.split_on_char ' ' cmd_str in
  match split_cmd with 
  | [] -> raise Empty
  | "print"::[] -> print_schedule sch; sch
  | "print"::c::[] ->
    let nm = String.uppercase_ascii c in
    if is_valid_coursename nm then
      (get_course nm (to_list sch) |> print_course sch; sch)
    else
      raise MalformedPrint
  | fst::others -> 
    let new_sch = match_helper fst others in
    autosave new_sch; new_sch

