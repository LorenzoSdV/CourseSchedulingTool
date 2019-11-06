open Adventure
open State
open Command


(** [prompt adv st] prompts the user to make an action when playing adventure
    [adv] and in current state [st] *)
let rec prompt adv st =
  ANSITerminal.(print_string [green]
                  "\nCurrently: ");
  print_endline (current_room_id st |> description adv);
  ANSITerminal.(print_string [blue]
                  "\nWhat do you want to do? Use commands 'go' or 'quit'.");
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

let play_game f =
  let adv = from_json (Yojson.Basic.from_file f) in
  ANSITerminal.(print_string [red]
                  "Game Loaded! Welcome.\n");
  prompt adv (init_state adv)

let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 3110 Text Adventure Game engine.\n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

(* Execute the game engine. *)
let () = main ()
