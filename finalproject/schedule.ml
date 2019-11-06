type sem_status = Past | Present | Future
type grade = Sat | Unsat | Withdrawn | Incomplete | Letter of string 

type sem_id = Spring of int | Fall of int

type reqs = {
  temp: int;
}

type course = {
  name: string;
  credits: int;
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
  mutable semesters: semester list;
  mutable commul_gpa: float;
  mutable exp_grad: int;
  mutable major: string;
}


exception InvalidCredits
exception UnknownCourse of string
exception UnknownSemester

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
  | _ -> -1.0

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

let create_course name cred gr deg = 
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
    Not_found -> raise UnknownSemester

let remove_course sch c semid =
  try
    let sem = List.find (fun sm -> sm.id = semid) sch.semesters in
    sem.courses <- (List.filter (fun crs -> crs <> c) sem.courses);
    sch
  with
    Not_found -> raise UnknownSemester

let get_course sch name semid = 
  try
    let sem = List.find (fun sm -> sm.id = semid) sch.semesters in
    List.find (fun c -> c.name = name) sem.courses
  with
    Not_found -> raise (UnknownCourse name)

let sem_ids sch =
  List.rev_map (fun sem -> sem.id) sch.semesters

let create_sem semid courses =
  {
    id = semid;
    courses = courses;
    tot_credits = credits courses;
    sem_gpa = gpa courses
  }

let add_sem sch sem =
  if (List.mem sem.id (sem_ids sch)) then
    raise (Failure "Tried to add semester that already exists!")
  else
    sch.semesters <- sem :: sch.semesters; sch

let remove_sem semid sch = 
  if (not (List.mem semid (sem_ids sch))) then
    raise UnknownSemester
  else
    sch.semesters <- 
      (List.filter (fun sem -> sem.id <> semid) sch.semesters); sch

let string_of_sem semid =
  match semid with
  | Spring yr -> "SP" ^ (string_of_int yr)
  | Fall yr -> "FA" ^ (string_of_int yr)

let new_schedule =
  {
    semesters = [];
    commul_gpa = 0.;
    exp_grad = 0;
    major = ""
  }