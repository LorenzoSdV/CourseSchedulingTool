open Schedule

(** The type representing the requirements of a CS major in either arts or
    engineering. *)
type reqs

(** The requirements for a CS degree in the College of Engineering. *)
val eng_reqs : reqs

(** The requirements for a CS degree in the College of Arts & Sciences. *)
(*val arts_reqs : reqs*)

(** [validate sch] is the validation information about [sch] after this
    information has been stored in [sch]. *)
val validate : schedule -> validation

(** [print_validation v] is [()] after printing to the terminal the validation
    information about some schedule whos validation info is [v]. *)
val print_validation : validation -> unit
