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

(** [get_course sch name semid] returns the course record with name [name]
    found in semester with id [semid] in [sch].
    Raises: [UnkownCourse name] if course does not exist in the semester. *)
val get_course : schedule -> string -> sem_id -> course

(** [gpa courses] is the GPA of all the courses in [courses] that have been
    given a letter grade. *)
val gpa : course list -> float

(** [credits courses] is the sum of all the credits of each course in 
    [courses]. *)
val credits : course list -> int

(** [create_sem courses semid] is a semester with courses [courses] and id
    [semid]. Automatically calculates GPA and # credits.*)
val create_sem : sem_id -> course list -> semester

(** [add_sem sch sem] is the schedule [sch] with semester [sem] added to its
    list of semesters, and GPA updated.
    Raises: [Failure] if semester with the same idalready exists in the 
    schedule. *)
val add_sem : schedule -> semester -> schedule

(** [remove_sem sch sem] is [sch] with the semester [sem] removed from list of 
    semesters. Updates GPA accordingly.
    Raises: [UnkownSemester] if semester doesn't exists in the [sch]. *)
val remove_sem : sem_id -> schedule -> schedule

(** [string_of_sem s] is the string representation of semester id [s]. String
    representations are like FA20 or SP18, etc. *)
val string_of_sem : sem_id -> string

(** [to_list sch] is the list of all courses contained in each semester in 
    [sch] *)
val to_list : schedule -> course list
