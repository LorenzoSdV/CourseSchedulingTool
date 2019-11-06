open Schedule

(** [get_course_creds nm sem] is the number of credits
    this course is worth during semester [sem] as indicated by class roster.
    Raises: [UnkownCourse nm] if course name isn not a valid course. 
    Raises: *)
val get_course_creds : string -> sem_id -> int

(** [string_of_url url] is the source HTML at URL [url]. *)
val string_of_url : string -> string -> string

(** [valid_course name sem credits] is *)
val valid_course : string -> sem_id -> int -> bool