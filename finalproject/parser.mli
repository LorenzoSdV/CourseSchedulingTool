open Schedule

(** [get_course_info nm sem] is a [course] populated with info on 
    course with name [nm] during semester [sem] gotten from Class
    Roster website. *)
val get_course_info : string -> sem_id -> course option

(** [get_course_creds nm sem] is [Some c] where [c] is the number of credits
    this course is worth during semester [sem] as indicated by class roster.
    Returns [None] if class wasn't offered during this semester or semester 
    out of bounds of what's available on CR. *)
val get_course_creds : string -> sem_id -> int option

(** [string_of_url url] is the source HTML at URL [url]. *)
val string_of_url : string -> string -> string

(** [valid_course name sem credits] is *)
val valid_course : string -> sem_id -> int -> bool