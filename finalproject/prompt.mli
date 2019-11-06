
(** The type [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. *)
type command =
  | Add of string list
  | Edit of string list
  | Remove of string list
  | Open of string
  | Close of string

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse_command str] parses a user's input into a [command], as follows. 
    The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the verb. The rest of the words, if any, become the string list of words or,
     if there's one word, then string.

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is malformed. A command
    is malformed when its first word is not one of the verbs from type command 
    and/or when there are too many/few words after the verb. *)
val parse_command : string -> command