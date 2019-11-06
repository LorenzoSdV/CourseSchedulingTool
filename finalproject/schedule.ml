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
  mutable curr_gpa: float;
  mutable exp_grad: int;
  mutable major: string;
}


exception InvalidCredits
exception UnknownCourse of string
exception UnknownSemester

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
    sch
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

let create_sem courses creds stat gpa =
  failwith "unimp"

let add_sem sem sch =
  failwith "unimp"

let remove_sem sem sch = 
  failwith "unimp"

let get_schedule =
  failwith "unimp"

let string_of_sem semid =
  match semid with
  | Spring yr -> "SP" ^ (string_of_int yr)
  | Fall yr -> "FA" ^ (string_of_int yr)