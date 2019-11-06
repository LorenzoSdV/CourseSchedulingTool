open Schedule
open Prompt

(** [prompt sch] is the prompt where the user can enter a command to interact
    with current schedule [sch]. *)
let rec prompt sch =
  ANSITerminal.(print_string [green] ("\n" ^ (name sch) ^ ": "));
  match read_line () with
  | exception End_of_file -> ()
  | "quit" -> Stdlib.exit 0
  | string_cmd -> 
    try
      prompt (parse_command sch string_cmd)
    with
      _ -> exception_handler sch

(* (** [execute sch cmd] handles executing the user's command from prompt 
    [prompt sch] and either quitting if desired or creating a new [prompt] 
    after action carried out. *)
   and execute sch cmd = 
   (*... this is where your prompt functionality is called upon!*)
   prompt sch *)

(** [exception_handler sch] handles any exceptions raised during a call to
    [execute adv st cmd]. Gives the user a nice message. *)
and exception_handler sch = 
  ANSITerminal.(print_string [red]
                  ("This is an illegal command or invalid syntax; 
                  please try again!\n")); 
  prompt sch

(** Loads a file and makes a schedule out of it *)
let load f =
  print_endline "This feature will be implemented in the second sprint. 
  Now creating a new schedule.\n";
  prompt Schedule.new_schedule

let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 3110 Project Schedule Planning Tool\n");
  print_endline "Please enter path to schedule you want to load, or leave 
  blank to create new schedule.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "quit" -> Stdlib.exit 0
  | "" -> prompt Schedule.new_schedule
  | file_name -> load file_name

(* Start running the user prompt: *)
let () = main ()