open Prompt

(** [prompt sch] is the prompt where the user can enter a command to interact
    with current schedule [sch]. *)
let rec prompt sch =
  ANSITerminal.(print_string [green] "\nSchedule: ");
  print_endline (name sch);
  print_string  "\n> ";
  match read_line () with
  | exception End_of_file -> ()
  | string_cmd -> 
    try
      execute adv st (parse string_cmd)
    with
    | Malformed 
    | Exit 
    | UnknownExit _ 
    | UnknownRoom _ -> exception_handler adv st

(** [execute adv st cmd] handles executing the user's command from prompt 
    [prompt adv st] and either quitting if desired or creating a new [prompt] 
    action carried out. *)
and execute adv st cmd = 
  match cmd with
  | Quit -> 
    ANSITerminal.(print_string [red]
                    "\nSorry to see you go!\n\n");
    Stdlib.exit 0
  | Go x -> match (go (String.concat " " x) adv st) with
    | Legal new_state -> prompt adv new_state
    | Illegal -> raise Command.Malformed

(** [exception_handler adv st] handles any exceptions raised during a call to
    [execute adv st cmd]. Gives the user a nice message. *)
and exception_handler adv st = 
  ANSITerminal.(print_string [red]
                  ("This is an illegal command or not a valid place to go; " 
                   ^ "please try again!\n")); 
  prompt adv st

(** Loads a file and makes a schedule out of it *)
let load f =
  print_endline "This feature will be implemented in the second sprint. Now creating a new schedule.\n";
  prompt Schedule.new_schedule

let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 3110 Project Schedule Planning Tool\n");
  print_endline "Please enter path to schedule you want to load, or leave blank to create new schedule.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "quit" -> ()
  | "" -> prompt Schedule.new_schedule
  | file_name -> load file_name

(* Start running the user prompt: *)
let () = main ()