type sem_status = Past | Present | Future
type grade = Sat | Unsat | Withdrawn | Incomplete | Letter of string 

type sem_id = 
  | Spring of int
  | Fall of int

type reqs = {
  temp: int;
}

type course = {
  name: string;
  credits: int;
  mutable grade: grade;
  degree: string;
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
