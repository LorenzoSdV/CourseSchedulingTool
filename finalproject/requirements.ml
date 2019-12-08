

let eng_reqs = {
  required = ["CS2800"; "CS3110"];
  categories = [];
}

let cehck_reqs sch =
  let rec loop courses reqs =
    match reqs with 
    | [] -> true
    | h :: t ->
      if List.mem h courses then
        loop courses t
      else false
  in
  loop (to_list sch) eng_reqs.required