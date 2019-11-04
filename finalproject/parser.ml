open Schedule

let get_course_info name sem =
  failwith "unip"

(** Returns body of URL as string *)
let string_of_url url = 
  let connection = Curl.init () and result = ref "" in
  Curl.set_writefunction connection
    (fun x -> result := !result ^ x; String.length x);
  Curl.set_url connection url;
  Curl.perform connection;
  Curl.global_cleanup ();
  !result

let course_html name sem =
  let course = String.split_on_char ' ' name in
  let url = "https://classes.cornell.edu/browse/roster/" ^ 
            string_of_sem sem ^ "/class/" ^ List.hd course ^ "/" ^ 
            List.hd (List.tl course) in
  string_of_url url


let get_credits html =
  let reg = Str.regexp_string "<span class=\"credit-val\">" in
