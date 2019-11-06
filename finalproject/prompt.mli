open Schedule

(** Raised when command is missing verb as the first word. *)
exception Empty

(** Raised when an invalid verb is encountered. *)
exception Malformed

(** Raised when a command has an invalid sem_id. *)
exception MalformedSemId

(** Raised when an Add command has invalid words *)
exception MalformedAdd

(** Raised when an Edit command has invalid words *)
exception MalformedEdit

(** Raised when an Remove command has invalid words *)
exception MalformedRemove

(** [parse_command cmd_str sch] parses [cmd_str] to be an action performed on
    [sch] to produce a new schedule. The first word (i.e., consecutive sequence 
    of non-space characters) of [str] becomes the verb. The rest of the words,
    if any, become the string list of words.

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is malformed. A command
    is malformed when its first word is not one of the verbs from type command 
    and/or when there are too many/few words after the verb. *)
val parse_command : schedule -> string -> schedule