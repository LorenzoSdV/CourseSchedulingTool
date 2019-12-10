open Schedule

(** The type representing the requirements of a CS major in either arts or
    engineering. *)
type recs

(** The requirements for a CS degree in the College of Engineering. *)
val eng_reqs : reqs

(** The requirements for a CS degree in the College of Arts & Sciences. *)
val arts_reqs : reqs

(** NEEDS COMMENT *)
val check_required : schedule -> string list -> string list

(** NEEDS COMMENT *)
val check_categories : schedule -> (string * int) list -> (string * int) list
