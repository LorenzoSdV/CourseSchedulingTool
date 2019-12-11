(**
   Data Types and Functions to get course information from Class Roster website
   @author Chris O'Brian (co253), Radha (rdp89), 
   and Lorenzo Scotto di Vettimo (ls769)
*)

open Schedule

(** Exception to be raised with any problems getting course information from
    Class Roster site. *)
exception InvalidURL

(** [get_course_creds nm sem] is the number of credits
    course [nm] is worth during semester [sem] as indicated by class roster.
    Raises: [UnkownCourse nm] if course name isn not a valid course. 
            [InvalidURL] if information can't be obtained from class roster
    for any reason. *)
val get_course_creds : string -> sem_id -> int

(** [get_FWS_status nm sem] is [true] if course with name [nm] is an FWS
    course as per it's name on Class Roster.
    Raises: [UnkownCourse nm] if course name isn not a valid course. 
            [InvalidURL] if information can't be obtained from class roster
    for any reason. *)
val get_FWS_status : string -> sem_id -> bool

(** [has_distribution_categroy nm sem] is [true] if course with name [nm] is 
    has a distribution category listed on Class Roster.
    Raises: [UnkownCourse nm] if course name isn not a valid course. 
            [InvalidURL] if information can't be obtained from class roster
    for any reason. *)
val has_distribution_categroy : string -> sem_id -> bool

(** [string_of_url url] is the source HTML at URL [url].
    Raises: [InvalidURL] if [url] is not a valid URL or ocurl cannot
    get data from it for any reason (like no internet) *)
val string_of_url : string -> string