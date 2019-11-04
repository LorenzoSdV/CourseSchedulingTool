open Schedule

let get_course_info name sem =
  failwith "unip"

(** Returns body of URL as string *)
let http url =
  failwith "unip"

let string_of_url url = 
  let connection = Curl.init () and result = ref "" in
  Curl.set_writefunction connection
    (fun x -> result := !result ^ x; String.length x);
  Curl.set_url connection url;
  Curl.perform connection;
  Curl.global_cleanup ();
  !result