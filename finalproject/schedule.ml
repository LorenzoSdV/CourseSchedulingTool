type sem_status = Past | Present | Future
type grade = Sat | Unsat | Withdrawn | Incomplete | Letter of string 
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
  (*make mutable?*)courses: course list;
  tot_credits: int;
  mutable sem_status: sem_status;
  sem_gpa: float;
}

type schedule = {
  (*make mutable?*)semesters: semester list;
  curr_gpa: float;
  mutable exp_grad: int;
  mutable major: string;

}
