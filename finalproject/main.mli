(** 
   The main entry point for the game interface.
*)

(** [load f] loads the schedule found in file f *)
val load : string -> unit

(** [main ()] prompts for the game to run the scheduler, then starts it. *)
val main : unit -> unit