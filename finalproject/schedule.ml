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
  mutable sem_status: sem_status;
  mutable sem_gpa: float;
}

type schedule = {
  mutable semesters: semester list;
  mutable curr_gpa: float;
  mutable exp_grad: int;
  mutable major: string;
}

exception UnknownCourse of string
exception UnknownSemester


let create_course name cred gr deg = 
  failwith "unimp"

let add_course c sem = 
  failwith "unimp"

let remove_course c sem =
  failwith "unimp"

let get_course name sem = 
  failwith "unimp"

let create_sem courses creds stat gpa =
  failwith "unimp"

let add_sem sem sch =
  failwith "unimp"

let remove_sem sem sch = 
  failwith "unimp"

let get_schedule =
  failwith "unimp"

let string_of_sem sem =
  match sem.id with
  | Spring yr -> "SP" ^ (string_of_int yr)
  | Fall yr -> "FA" ^ (string_of_int yr)