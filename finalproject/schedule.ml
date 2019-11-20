type sem_status = Past | Present | Future
type grade = Sat | Unsat | Withdrawn | Incomplete | Letter of string 

type sem_id = Spring of int | Fall of int

type reqs = {
  temp: int;
}

type course = {
  name: string;
  mutable credits: int;
  mutable grade: grade;
  mutable degree: string;
  (* subject/category *)
}

type semester = {
  mutable id: sem_id;
  mutable courses: course list;
  mutable tot_credits: int;
  (*mutable sem_status: sem_status;*)
  mutable sem_gpa: float;
}

type schedule = {
  mutable desc: string;
  mutable semesters: semester list;
  mutable cumul_gpa: float;
  mutable exp_grad: int;
  mutable major: string;
}

exception UnknownCourse of string
exception UnknownSemester of string
exception UnknownGrade of string
exception DuplicateCourse of string
exception DuplicateSemester of string
exception InvalidCredits of string

let grade_map gr = 
  match gr with
  | Letter "A+" -> 4.3
  | Letter "A" -> 4.0
  | Letter "A-" -> 3.7
  | Letter "B+" -> 3.3
  | Letter "B" -> 3.0
  | Letter "B-" -> 2.7
  | Letter "C+" -> 2.3
  | Letter "C" -> 2.0
  | Letter "C-" -> 1.7
  | Letter "D+" -> 1.3
  | Letter "D" -> 1.0
  | Letter "D-" -> 0.7
  | Letter "F" -> 0.0
  | _ -> failwith "Impossible Failure"

let gradify str =
  let str_upper = String.uppercase_ascii str in
  if Str.string_match (Str.regexp "^[A-D][\\+-]?$\\|^F$") str_upper 0 then
    Letter str_upper
  else
    match str_upper with
    | "INCOMPLETE" | "INC" -> Incomplete
    | "W" | "WITHDRAWN" -> Withdrawn
    | "SAT" | "S" -> Sat
    | "UNSAT" | "U" -> Unsat
    | _ -> raise (UnknownGrade str)

let gpa courses =
  let rec fold_credits courses acc =
    match courses with
    | [] -> acc
    | { credits = c; grade = g } :: t -> 
      if (grade_map g > 0.) then fold_credits t (acc + c)
      else fold_credits t acc
  in
  let rec fold_gps courses acc =
    match courses with
    | [] -> acc
    | { credits = c; grade = g } :: t -> 
      if (grade_map g >= 0.) then 
        fold_gps t (acc +. ((float_of_int c) *. grade_map g))
      else fold_gps t acc
  in
  (fold_gps courses 0.) /. (float_of_int (fold_credits courses 0))

let get_credits courses =
  let rec fold courses acc =
    match courses with
    | [] -> acc
    | { credits = c } :: t -> fold t (acc + c)
  in fold courses 0

let to_list sch =
  let rec fold sems acc = 
    match sems with
    | [] -> acc
    | {courses=x} :: t -> fold t (x @ acc)
  in
  fold sch.semesters []

let string_of_semid semid =
  match semid with
  | Spring yr -> "SP" ^ (string_of_int yr)
  | Fall yr -> "FA" ^ (string_of_int yr)

let string_of_grade gr =
  match gr with
  | Sat -> "Satisfactory"
  | Unsat -> "Unsatisfactory"
  | Withdrawn -> "Withdrawn"
  | Incomplete -> "Incomplete"
  | Letter l -> l

(** [sem_compare s1 s2] returns a negative number if [s1] comes before [s2], 
    0 if theyre the same semester, and a positive number if [s1] comes after
    [s2]. *)
let sem_compare s1 s2 =
  match s1.id,s2.id with
  | Fall y1 , Fall y2
  | Spring y1 , Spring y2 -> Stdlib.compare y1 y2
  | Fall y1 , Spring y2 -> if y1 = y2 then 1 else Stdlib.compare y1 y2
  | Spring y1 , Fall y2 -> if y1 = y2 then -1 else Stdlib.compare y1 y2

let create_course name cred gr deg = 
  if cred < 0 then 
    raise (InvalidCredits "Credits have to be greater than or equal to zero.")
  else if not (Str.string_match 
                 (Str.regexp "^[A-Z][A-Z]+[0-9][0-9][0-9][0-9]$") name 0) then
    raise (UnknownCourse ("Invalid Course name - " ^ name))
  else
    {
      name = name;
      credits = cred;
      grade = gr;
      degree = deg;
    }

let rec get_course name courses = 
  match courses with 
  | [] -> raise (UnknownCourse name)
  | h :: t -> if h.name = name then h else get_course name t

let rec get_sem sch sems semid = 
  match sems with 
  | [] -> raise (UnknownSemester (string_of_semid semid))
  | h :: t -> if h.id = semid then h else get_sem sch t semid

let get_sems sch = 
  sch.semesters

let get_sem_courses sem =
  sem.courses 

let add_course sch c semid = 
  try
    let sem = List.find (fun sm -> sm.id = semid) sch.semesters in
    if List.mem c.name (List.map (fun c -> c.name) (to_list sch)) then
      raise (DuplicateCourse (c.name ^ " already in schedule."))
    else begin
      sem.courses <- (c :: sem.courses);
      sem.sem_gpa <- gpa sem.courses;
      sem.tot_credits <- sem.tot_credits + c.credits;
      { sch with cumul_gpa = gpa (to_list sch) }
    end
  with
    Not_found -> raise (UnknownSemester (string_of_semid semid))

let rec get_sem_from_course sch sems course = 
  match sems with 
  | [] -> raise (UnknownCourse course.name)
  | h :: t -> if get_course course.name (to_list sch) = course then h else
      get_sem_from_course sch t course

let edit_course_creds course = 
  true

let edit_course sch cname attr new_val =
  try
    let course = List.find (fun course -> course.name = cname) (to_list sch) in
    let sem  = get_sem_from_course sch sch.semesters course in 
    print_endline (string_of_semid sem.id);
    match attr with
    | "credits" ->
      course.credits <- int_of_string new_val;
      sem.tot_credits <- get_credits sem.courses;
      sem.sem_gpa <- gpa sem.courses;
      sch.cumul_gpa <- gpa (to_list sch); sch
    | "grade" -> 
      course.grade <- gradify new_val;
      sem.sem_gpa <- gpa sem.courses;
      sch.cumul_gpa <- gpa (to_list sch); sch
    | "degree" -> 
      course.degree <- new_val; sch
    | _ -> raise (Failure "Invalid course attribute")
  with
    Not_found -> raise (UnknownCourse cname)

let remove_course sch cname =
  try
    let course = get_course cname (to_list sch) in
    let sem = get_sem_from_course sch sch.semesters course in
    sem.courses <- (List.filter (fun crs -> crs.name <> cname) sem.courses);
    sem.tot_credits <- get_credits sem.courses;
    sem.sem_gpa <- gpa sem.courses;
    sch.cumul_gpa <- gpa (to_list sch); sch
  with 
    Not_found -> raise (UnknownCourse cname)

(* let remove_course sch cname semid =
   try begin
    let sem = List.find 
        (fun smstr -> List.mem cname 
            (List.rev_map 
               (fun course -> course.name) smstr.courses)) 
        sch.semesters 
    in
    let () = sem.courses <- (List.filter (fun crs -> crs.name <> cname) sem.courses) in
    let c = get_course sch cname sem.id in 
    print_endline ("creds = " ^ string_of_int c.credits);
    sem.tot_credits <- sem.tot_credits - c.credits;
    sch
   end
   with
    Not_found -> raise (UnknownCourse cname) *)

let sem_ids sch =
  List.rev_map (fun sem -> sem.id) sch.semesters

let create_sem semid =
  {
    id = semid;
    courses = [];
    tot_credits = 0;
    sem_gpa = 0.
  }

let add_sem sch sem =
  if (List.mem sem.id (sem_ids sch)) then
    raise (DuplicateSemester (string_of_semid sem.id))
  else begin
    sch.semesters <- List.sort sem_compare (sem :: sch.semesters);
    sch.cumul_gpa <- gpa (to_list sch);
    sch end

let remove_sem sch semid = 
  if (not (List.mem semid (sem_ids sch))) then
    raise (UnknownSemester (string_of_semid semid))
  else begin
    sch.semesters <- 
      (List.filter (fun sem -> sem.id <> semid) sch.semesters); 
    sch.cumul_gpa <- gpa (to_list sch);
    sch end

let new_schedule =
  {
    desc = "SCHEDULE 1";
    semesters = [];
    cumul_gpa = 0.;
    exp_grad = 0;
    major = ""
  }

let get_name sch =
  sch.desc

let edit_name sch nm =
  sch.desc <- nm; sch

let print_sem sem =
  print_string ": [ ";
  List.fold_right 
    (fun course _ -> print_string ((course.name) ^ ", ")) 
    sem.courses ();
  print_string (" ] Semester GPA: " ^ (string_of_float sem.sem_gpa));
  print_endline (" | Semester Credits: " ^ string_of_int sem.tot_credits);
  print_endline ""

let print_schedule sch =
  if sch.semesters = [] then 
    ANSITerminal.(print_string [red] "No semesters in current schedule. Try running 'add <semester>'\n")
  else begin
    List.fold_left 
      (fun () sem -> print_string (string_of_semid sem.id); print_sem sem)
      () sch.semesters;
    print_endline ("Cumulative GPA: " ^ (string_of_float sch.cumul_gpa));
    print_endline ("Total Credits: " ^ (string_of_int (get_credits (to_list sch))))
  end


module HTML = struct

  let template =
    let rec input_file acc chan = 
      try
        input_file (acc ^ ((input_line chan) ^ "\n")) chan
      with
        End_of_file -> acc
    in
    input_file "" (open_in "temp.html")

  let html_of_course c =
    "\t\t\t\t<td>\n" ^ 
    "\t\t\t\t\t<h4><strong>" ^ (c.name) ^ "</strong></h4>\n" ^ 
    "\t\t\t\t\t<p>Credits: " ^ (string_of_int c.credits) ^ "</p>\n" ^
    "\t\t\t\t\t<p>Grade: " ^ (string_of_grade c.grade) ^ "</p>\n" ^ 
    "\t\t\t\t\t<p>Category: " ^ (c.degree) ^ "</p>\n" ^ 
    "\t\t\t\t</td>\n"

  let html_of_sem sem =
    match sem.courses with
    | [] -> "\t\t\t<tr><td class=\"noborder\"><h3>" ^ (string_of_semid sem.id) ^ "</h3></td></tr>\n"
    | _ -> begin
        "\t\t\t<tr><td><h3>" ^ (string_of_semid sem.id) ^ "</h3></td>\n" ^
        "\t\t\t<p>Semester GPA: <strong>" ^ (string_of_float sem.sem_gpa) ^ "</strong></p></td>\n" ^ 
        (List.fold_left (fun acc course -> acc ^ (html_of_course course)) "" sem.courses) ^ 
        "\t\t\t</tr>\n" end

  let html_of_schedule sch =
    match (get_sems sch) with
    | [] -> "<p>Schedule is empty!</p>\n"
    | _ -> begin
        "<h1><strong style=\"color:green;\">" ^ sch.desc ^ "</strong></h1>\n" ^ 
        "\t\t<h2>Cumulative GPA: <strong style=\"color:blue;\">" ^ (string_of_float sch.cumul_gpa) ^ "</strong></h2>\n" ^ 
        "\t\t<table>\n" ^ 
        (List.fold_left (fun acc sem -> acc ^ (html_of_sem sem)) "" (get_sems sch)) ^ 
        "\t\t</table>\n" end

  let save filename text = 
    let chan = open_out filename in
    output_string chan text;
    close_out chan

  let export_schedule sch fl = 
    let reg = Str.regexp "<\\?sch>" in
    Str.replace_first reg (html_of_schedule sch) template |> save fl

end

module JSON = struct

  open Yojson.Basic.Util

  let make_json sch = 
    ()

  (** [form_sem_id_helper sem lst] forms a semester id based on the string
      [sem] and the list [lst] which will be parsed to find a year. *)
  let form_sem_id_helper sem lst = 
    match List.rev lst with 
    | [] -> raise (UnknownSemester sem)
    | h :: t -> let yr = int_of_string h + 2000 in 
      match sem with 
      | "Fall" -> Fall yr
      | "Spring" -> Spring yr
      | _ -> raise (UnknownSemester sem)

  (** [form_sem_id semid] determines if the semester in [semid] is referencing
      a fall or spring semester and calls a helper function to help form a 
      semester id. *)
  let form_sem_id semid = 
    if String.contains semid 'F' then let word = "Fall" in 
      let lst = String.split_on_char 'A' semid 
      in form_sem_id_helper word lst
    else 
      let word = "Spring" in 
      let lst = String.split_on_char 'P' semid
      in form_sem_id_helper word lst

  let form_grade grade = 
    match grade with 
    | "Sat" -> Sat
    | "Unsat" -> Unsat
    | "Withdrawn" -> Withdrawn
    | "Incomplete" -> Incomplete
    | _ -> Letter grade

  (** [parse_course json] creates courses by parsing [json]. *)
  let parse_course json = {
    name = json |> member "name" |> to_string;
    credits = json |> member "course credits" |> to_int;
    grade = json |> member "grade" |> to_string |> form_grade;
    degree = json |> member "degree" |> to_string;
  }

  (** [get_semester json] creates semesters by parsing [json]. *)
  let get_semester json = {
    id = json |> member "semester id" |> to_string |> form_sem_id;
    courses = json |> member "course" |> to_list |> List.map parse_course;
    tot_credits = json |> member "semester credits" |> to_int;
    sem_gpa = json |> member "semester gpa" |> to_float;
  }

  let parse_json json = {
    desc = json |> member "description" |> to_string;
    semesters = json |> member "semesters" |> to_list |> List.map get_semester;
    cumul_gpa = json |> member "cumul gpa" |> to_float;
    exp_grad = json |> member "expected grad year" |> to_int;
    major = json |> member "major" |> to_string;
  }

end