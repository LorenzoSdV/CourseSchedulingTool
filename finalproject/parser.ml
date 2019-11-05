open Schedule

(** Returns body of URL as string *)
let string_of_url url = 
  try
    let connection = Curl.init () and result = ref "" in
    Curl.set_writefunction connection
      (fun x -> result := !result ^ x; String.length x);
    Curl.set_url connection url;
    Curl.perform connection;
    Curl.global_cleanup ();
    !result
  with
    _ -> failwith "URL to string error"

let course_html name sem =
  let course = String.split_on_char ' ' name in
  let url = "https://classes.cornell.edu/browse/roster/" ^ 
            (string_of_sem sem) ^ "/class/" ^ List.hd course ^ "/" ^ 
            List.hd (List.tl course) in
  string_of_url url

(** Returns # of credits for course. Returns -1 on error*)
let parse_credits html =
  let reg = Str.regexp_string "<span class=\"credit-val\">" in
  try
    int_of_string (String.sub html ((Str.search_forward reg html 0) + 25) 1)
  with
    _ -> -1

let get_course_info name sem : Schedule.course option =
  let reg = Str.regexp "^[A-Z]{1,5} [0-9]{4}$" in
  if (Str.string_match reg name 0) then
    let credits = parse_credits (course_html name sem) in
    Some (create_course name credits Incomplete "")
  else
    None