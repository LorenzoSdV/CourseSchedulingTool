open Schedule

(** Returns body of URL as string *)
let string_of_url url nm = 
  try
    let connection = Curl.init () and result = ref "" in
    Curl.set_writefunction connection
      (fun x -> result := !result ^ x; String.length x);
    Curl.set_url connection url;
    Curl.perform connection;
    Curl.global_cleanup ();
    !result
  with
    _ -> raise (Failure "Error retreiving course information")

(** ADD COMMENT *)
let course_html name sem =
  let course = String.split_on_char ' ' name in
  let url = "https://classes.cornell.edu/browse/roster/" ^ 
            (string_of_sem sem) ^ "/class/" ^ List.hd course ^ "/" ^ 
            List.hd (List.tl course) in
  string_of_url url name

(** Returns # of credits for course. Returns -1 on error*)
let parse_credits html =
  let reg = Str.regexp_string "<span class=\"credit-val\">" in
  try
    int_of_string (String.sub html ((Str.search_forward reg html 0) + 25) 1)
  with
    _ -> raise (Failure "Error getting credits from Class Roster")

let get_course_creds name sem =
  let n_upper = String.uppercase_ascii name in
  let reg = Str.regexp "^[A-Z]{1,5} [0-9]{4}$" in
  if (Str.string_match reg n_upper 0) then
    parse_credits (course_html n_upper sem)
  else
    raise (UnknownCourse name)

let valid_course name sem credits =
  try
    if (get_course_creds name sem = credits) then true
    else raise InvalidCredits;
  with
    _ -> raise (UnknownCourse name)