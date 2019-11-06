(** The type representing whether the currently viewed semester is a past 
    semester, a present one, or a future semester. *)
type sem_status = Past | Present | Future

(**  NEEDS COMMENT *)
type sem_id = Spring of int | Fall of int

(** The type representing the grade of a course. *)
type grade = Sat | Unsat | Withdrawn | Incomplete | Letter of string 

(** The type representing the prereqs or coreqs of a course. *)
type reqs 

(** Type representing a course *)
type course

(** Type representing a semester *)
type semester

(** Type representing a whole schedule *)
type schedule

(** [InvalidCredits] raised when a course is to be added to a schedule with an 
    invalid number of credits. *)
exception InvalidCredits

(** [UnknownCourse nm] raised when course with name [nm] passed as a real course
    but isn't recognized as such. *)
exception UnknownCourse of string

(** [UnknownSemester] raised when function attempts to work with non-existent
    semester. *)
exception UnknownSemester

(** [create_course name cred gr deg] is a new course type with name [name], 
    number of credits [cred], grade [gr], and degree category [deg]. *)
val create_course : string -> int -> grade -> string -> course

(** [add_course sch c semid] is the schedule with course [c] added to semester
    with id [sem_id].
    Raises: [Failure] if course already exists in the semester. *)
val add_course : schedule -> course -> sem_id -> schedule

(** [add_course sch c semid] is the schedule with course [c] removed
    from semester id [semid]. If course [c] is not in semester [semid] then
    [add_course sch c semid] is [sch] *)
val remove_course : schedule -> course -> sem_id -> schedule



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

(** [get_schedule] returns the current schedule with all the smeesters and 
    courses. *)
val get_schedule : schedule

(** [string_of_sem s] is the string representation of semester id [s]. String
    representations are like FA20 or SP18, etc. *)
val string_of_sem : sem_id -> string

(** [to_list sch] is the list of all courses contained in each semester in 
    [sch] *)
val to_list : schedule -> course list
