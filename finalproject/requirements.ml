open Schedule

type reqs = {
  required : string list;
  categories : (string * int) list;
  subs : (string * string) list
}

let eng_reqs = {
  required = ["CS2110"; "CS2800"; "CS3110"];
  categories = [("Technical", 9); ("Ext Spec", 9); ("4000+", 9)];
  subs = [("CS2110","CS2112")]
}

let substitutues course subs = 
  List.fold_left

let check_required sch required =
  let rec loop courses reqs acc =
    match reqs with 
    | [] -> acc
    | h :: t ->
      if List.mem h courses then
        loop courses t acc
      else
        loop courses t (h :: acc)
  in
  loop (List.map get_course_name (to_list sch)) required []

let check_categories sch categories = 
