(** The type representing whether the currently viewed semester is a past 
    semester, a present one, or a future semester. *)
type sem_status = Past | Present | Future

(** NEEDS COMMENT *)
type sem_id = Spring of int | Fall of int | None

(** The type representing the grade of a course. *)
type grade = Sat | Unsat | Withdrawn | Incomplete | None | Letter of string 

(** The type representing the prereqs or coreqs of a course. *)
(* WILL BE IMPLEMENTED IN SPRINT 3 *)
(*type reqs *)

(** Type representing a course *)
type course

(** Type representing a semester *)
type semester

(** Type representing a whole schedule *)
type schedule

(*(** [InvalidCredits] raised when a course is to be added to a schedule with an 
    invalid number of credits. *)
  exception InvalidCredits*)

(** [UnknownCourse nm] raised when course with name [nm] passed as a real course
    but isn't recognized as such. *)
exception UnknownCourse of string

(** [UnknownSemester sem] raised when function attempts to work with 
    non-existent semester. *)
exception UnknownSemester of string

(** [UnknownGrade grd] raised when function attempts to work with invalid 
    string representation of a grade. *)
exception UnknownGrade of string

(** [DuplicateCourse nm] raised when course with name [nm] is added to a
    semester where a course with same name already exists. *)
exception DuplicateCourse of string

(** [DuplicateSemester sem] raised when semester with string-id [sem] is added 
    to a schedule where a semester with the same id already exists. *)
exception DuplicateSemester of string

(** [InvalidSwap] is raised when an illegal swap is attempted. Swap is illegal 
    if both courses are in same semester or both are same course. *)
exception InvalidSwap

(** [gradify str] is the grade representation of [str] where is some grade value 
    represented as a string.
    Requires: [str] is a valid string rep of a grade, like: 
    "A+" or "b" or "unsat" or "w". 
    Raises: [Failure "Unknown Grade"] if [str] is not a valid grade 
    representation. *)
val gradify: string -> grade

(** [create_course name cred gr deg] is a new course type with name [name], 
    number of credits [cred], grade [gr], and degree category [deg]. *)
val create_course : string -> int -> grade -> string -> course

(** [add_course sch c semid] is the schedule with course [c] added to semester
    with id [sem_id].
    Raises: [Failure] if course already exists in the semester. *)
val add_course : schedule -> course -> sem_id -> schedule

(** [edit_course sch c attr new_val] is the schedule that results from chaning 
    the course field [attr] to [new_val] for course with name [c] in 
    schedule [sch]. 
    Raises: [Failure] with various error messages if [attr] is not a valid field
    of course record, or [new_val] is not valid. 
    Raises: [UnkownCourse c] if course is not in [sch]. *)
val edit_course : schedule -> string -> string -> string -> schedule

(** [add_course sch c semid] is the schedule with course name [c] removed
    from semester id [semid]. If course [c] is not in semester [semid] then
    [add_course sch c semid] is [sch]. *)
val remove_course : schedule -> string -> schedule

(** [swap_courses c1_name c2_name sch] is the schedule with c1_name and c2_name 
    swapped. [c1_name] cannot be the same course as [c2_name], and [c1_name]'s 
    semester cannot be the same semester as [c2_name]'s semester. *)
val swap_courses : string -> string -> schedule -> schedule

(** [get_course name courses] is the course record with name [name]
    found in [courses].
    Raises: [UnknownCourse name] if course does not exist in [courses].*)
val get_course : string -> course list -> course

(** [get_sem sch sems semid] returns the semester with the semester id [sem_id]
    in schedule [sch]. 
    Raises: [UnknownSemester sem_id] if no such semester exists. *)
val get_sem : schedule -> semester list -> sem_id -> semester

(** [get_sems sch] returns the semesters in [sch]. *)
val get_sems : schedule -> semester list

(** [get_sem_courses sem] returns a list of all the courses in the semester
    [sem]. *)
val get_sem_courses : semester -> course list

(** [gpa courses] is the GPA of all the courses in [courses] that have been
    given a letter grade. *)
val gpa : course list -> float

(** [get_credits sch] is the sum of all the credits in the schedule [sch]. *)
val get_credits : schedule -> int

(** [get_credits cl] is the calculated sum of credits of each course in [cl] *)
val calc_credits : course list -> int

(** [create_sem courses semid] is a semester with courses [courses] and id
    [semid]. Automatically calculates GPA and # credits. *)
val create_sem : sem_id -> semester

(** [add_sem sch sem] is the schedule [sch] with semester [sem] added to its
    list of semesters, and GPA updated.
    Raises: [Failure] if semester with the same idalready exists in the 
    schedule. *)
val add_sem : schedule -> semester -> schedule

(** [remove_sem sch sem] is [sch] with the semester [sem] removed from list of 
    semesters. Updates GPA accordingly.
    Raises: [UnkownSemester] if semester doesn't exists in the [sch]. *)
val remove_sem : schedule -> sem_id -> schedule

(** [string_of_semid s] is the string representation of semester id [s]. String
    representations are like FA20 or SP18, etc. *)
val string_of_semid : sem_id -> string

(** [sem_ids s] is the list of semester ids from each semester in 
    schedule [sch]. *)
val sem_ids : schedule -> sem_id list

(** [sem_ids_to_string sch] is the list of semester ids as strings from each 
    semester in schedule [sch]. *)
val sem_ids_to_string : schedule -> string list

(** [to_list sch] is the list of all courses contained in each semester in 
    [sch]. *)
val to_list : schedule -> course list

(** [new_schedule nm] is a new empty schedule with name [nm] but no courses 
    or semesters. *)
val new_schedule : string -> schedule

(** COMMENT *)
val print_course : schedule -> course -> unit

(** COMMENT *)
val print_schedule : schedule -> unit

(** [get_save_status sch] is whether or not [sch] has been saved. *)
val get_save_status : schedule -> bool

(** [set_save_status sch] sets save status of [sch] to [bool]. *)
val set_save_status : schedule -> bool -> unit

(** [get_name sch] is the user-defined name of schedule [sch]. *)
val get_name : schedule -> string

(** [edit_name sch nm] is the schedule that results from changing the name of 
    [sch] to [nm]. *)
val edit_name : schedule -> string -> schedule

module HTML : sig

  (** [export_schedule sch fl] takes schedule [sch] and exports it in visual 
      HTML representation to HTML file given by path [fl]. *)
  val export_schedule : schedule -> string -> unit

end

module LoadJSON : sig 

  (** [parse_json json] takes a JSON file and parses it to create a schedule
      so a user can use a saved schedule. *)
  val parse_json : string -> schedule

end

module SaveJSON : sig

  (** [save_schedule sch fl] takes schedule [sch] and exports it into a JSON 
      file named [fl] that can save the current schedule. *)
  val save_schedule : schedule -> string -> unit

end