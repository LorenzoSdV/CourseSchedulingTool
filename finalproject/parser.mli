open Schedule

(** [get_course_info nm sem] is a [course] populated with info on 
    course with name [nm] during semester [sem] gotten from Class
    Roster website. *)
val get_course_info : string -> sem_id -> course