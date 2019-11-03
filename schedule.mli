(** The type representing whether the currently viewed semester is a past 
    semester, a present one, or a future semester. *)
type sem_status = Past | Present | Future

(** The type reepresenting the grade of a course. *)
type grade = Sat | Unsat | Withdrawn | Incomplete | Letter of string 

(** The type representing the prereqs or coreqs of a course. *)
type recs 

(** Type representing a course *)
type course

(** Type representing a semester *)
type semester

(** Type representing a whole schedule *)
type schedule

exception UnknownCourse of string

exception UnknownSemester

(** [create_course name cred gr deg] creates a new course with name [name], 
    number of credits [cred], grade [gr], and degree category [deg]. *)
val create_course : string -> int -> grade -> string -> course

(** [add_course c sem] adds course [c] to the list of courses in a semester
    [sem] if it is not already in there. 
    Raises: [Failure] if course already exists in the semester. *)
val add_course : course -> semester -> semester

(** [remove_course c sem] removes course [c] from the list of courses in a 
    semester [sem] if it exists. 
    Raises: [Failure] if course does not exist in the semester. *)
val remove_course : course -> semester -> semester

(** [get_course name sem] returns the information about the course with name
    [name] if it exists in the semester.
    Raises: [Failure] if course does not exist in the semester. *)
val get_course : string -> semester -> course

(** [create_sem courses creds stat gpa] creates a new semester with course
    list [courses], number of credits [creds], semester status [stat], and 
    semester gpa [gpa]. *)
val create_sem : course list -> int -> sem_status -> float -> semester

(** [add_sem sem sch] adds semester [sem] to the list of semesters in a 
    schedule [sch] if it is not already in there.
    Raises: [Failure] if semester already exists in the schedule. *)
val add_sem : semester -> schedule -> schedule

(** [remove_sem sem sch] removes semester [sem] from the list of semesters in 
    a schedule [sch] if it exists.
    Raises: [Failure] if semester doesn't exists in the schedule. *)
val remove_sem : semester -> schedule -> schedule

(** get_sem? get_schedule? *)