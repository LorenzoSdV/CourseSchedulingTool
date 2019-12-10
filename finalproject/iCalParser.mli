(**
   Data Types and Functions to parse an iCal file and add to schedule.
   @author Chris O'Brian (co253), Radha (rdp89), 
   and Lorenzo Scotto di Vettimo (ls769)
*)

(** [parse_file file] is a the tuple of course names and a semester identifier
    for an ical schedule [file] downlaoded from Class Roster.

    Requires: [file] is a .ics file downloaded from Class Roster that has not 
    been modified in any fashion. *)
val parse_file : string -> string list * string