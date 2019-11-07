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
  mutable commul_gpa: float;
  mutable exp_grad: int;
  mutable major: string;
}

exception UnknownCourse of string
exception UnknownSemester of string
exception UnkownGrade of string
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
    | _ -> raise (UnkownGrade str)

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

let credits courses =
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

let create_course name cred gr deg = 
  if cred < 0 then 
    raise (InvalidCredits "Too few credits")
  else if not (Str.string_match 
                 (Str.regexp "^[A-Z][A-Z]+[0-9][0-9][0-9][0-9]$") name 0) then
    raise (UnknownCourse ("Invalud Course name - " ^ name))
  else
    {
      name = name;
      credits = cred;
      grade = gr;
      degree = deg;
    }

let add_course sch c semid = 
  try
    let sem = List.find (fun sm -> sm.id = semid) sch.semesters in
    sem.courses <- (c :: sem.courses);
    sem.sem_gpa <- gpa sem.courses;
    { sch with commul_gpa = gpa (to_list sch) }
  with
    Not_found -> raise (UnknownSemester (string_of_semid semid))

let edit_course sch cname attr new_val =
  try
    let course = List.find (fun course -> course.name = cname) (to_list sch) in
    match attr with
    | "credits" ->
      course.credits <- int_of_string new_val; sch
    | "grade" -> 
      course.grade <- gradify new_val; sch
    | "degree" -> 
      course.degree <- new_val; sch
    | _ -> raise (Failure "Invalid course attribute")
  with
    Not_found -> raise (UnknownCourse cname)

let remove_course sch cname =
  try
    let sem = List.find 
        (fun smstr -> List.mem cname 
            (List.rev_map 
               (fun course -> course.name) smstr.courses)) 
        sch.semesters 
    in
    sem.courses <- (List.filter (fun crs -> crs.name <> cname) sem.courses);
    sch
  with
    Not_found -> raise (UnknownCourse cname)

let get_course sch name semid = 
  try
    let sem = List.find (fun sm -> sm.id = semid) sch.semesters in
    List.find (fun c -> c.name = name) sem.courses
  with
    Not_found -> raise (UnknownCourse name)

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
    sch.semesters <- sem :: sch.semesters; 
    sch.commul_gpa <- gpa (to_list sch);
    sch end

let remove_sem sch semid = 
  if (not (List.mem semid (sem_ids sch))) then
    raise (UnknownSemester (string_of_semid semid))
  else begin
    sch.semesters <- 
      (List.filter (fun sem -> sem.id <> semid) sch.semesters); 
    sch.commul_gpa <- gpa (to_list sch);
    sch end

let new_schedule =
  {
    desc = "SCHEDULE 1";
    semesters = [];
    commul_gpa = 0.;
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
  print_endline (" ] Semester GPA: " ^ (string_of_float sem.sem_gpa))

let print_schedule sch =
  List.fold_right 
    (fun sem _ -> print_string (string_of_semid sem.id); print_sem sem) 
    sch.semesters ();
  print_endline ("Cummulative GPA: " ^ (string_of_float sch.commul_gpa));
  print_endline ("Total Credits: " ^ (string_of_int (credits (to_list sch))))