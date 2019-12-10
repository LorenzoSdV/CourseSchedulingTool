open Schedule

type reqs = {
  required : string list;
  categories : (string * int) list;
  subs : string list list
}

let eng_reqs = {
  required = ["MATH1910"; "MATH1920"; "MATH2940"; "CS3110"; "CS4410"; "CS4280"];

  categories = [("Technical", 9); ("Ext Spec", 9); ("4000+", 9); 
                ("Lib Studies", 18); ("FWS", 6); ("PE", 2); 
                ("Advisor-Approved Elective", 6); ("Free Elective", 3);
                ("Practicum", 2); ("ENGRI", 3); ("ENGRD", 3)];

  subs = [["CS1110"; "CS1112"]; ["CS2110"; "CS2112"]; ["CS2800"; "CS2802"];
          ["CHEM2080"; "BTRY3080"; "ECON3130"; "MATH2930"; "MATH4710"; 
           "PHYS2214"; "PHYS2218"]; ["CS3410"; "CS3420"; "ECE3140"];
          ["ENGRD2700"; "BTRY3080"; "CS4850"; "ECE3100"; "ECON3130"; 
           "MATH4710"]; ["CHEM2090"; "CHEM2150"]]
}

(*let arts_reqs = {
  required = ["MATH1110"; "MATH1120"; "MATH2210"; "CS1110"; "CS2110"; "CS3110";
              "CS3410"; "CS2800"; "CS4410"; "CS4820"; "Project Course";
              "Free Elective"; "ENGRD2700"];
  categories = [("4000+", 9); ("Technical", 9); ("Ext Spec", 9); ("FWS", 6);
                ("PE", 2); ("Lang", 11); ("GB", 3); ("HB", 3)];
  subs = [("MATH1110", "MATH1910"); ("MATH1120", "MATH1220"); 
          ("MATH1120", "MATH1920"); ("MATH2210", "MATH2940"); ("CS1110", "CS1112");
          ("CS2110", "CS2112"); ("CS2800", "CS2802"); ("CS3410", "CS3420");
          ("ENGRD2700", "BTRY3080");
          ("ENGRD2700", "CS4850"); ("ENGRD2700", "ECE3100"); ("ENGRD2700", "ECON3130");
          ("ENGRD2700", "MATH4710"); ("GB", "GHB"); ("HB", "GHB")];
  }*)

(*let substitutes course subs = 
  let _, result = List.filter (fun (c,sub) -> c = course) subs |> List.split
  in result*)

(** [check_required sch reqs] is a ist of courses that are required by [reqs] 
    but not included in [sch]. 

    If [check_required sch reqs] = [] then all course requirments are met. *)
let check_required sch reqs =
  let rec loop courses required acc =
    match required with 
    | [] -> acc
    | h :: t ->
      if List.mem h courses then
        loop courses t acc
      else
        loop courses t (h :: acc)
  in
  loop (List.map get_course_name (to_list sch)) reqs.required []


(** [check_categories sch reqs] is a list of tuples of categories and credits
    required for each category that aren't met in [sch]

    For example, if [check_categories sch reqs] = [("CS4000+", 3)], then [sch] 
    is missing one course satisfying the CS4000+ requirement. If 
    [check_categories sch reqs] = [] then all category requirment are met. *)
let check_categories sch reqs =
  let rec loop courses categories acc =
    match categories with 
    | [] -> acc
    | (cat,creds) :: t ->
      let satisf = List.filter (fun c -> get_course_cat c = cat) courses in
      let creds_satsift = calc_credits satisf in
      if creds_satsift < creds then
        loop courses t ((cat, (creds - creds_satsift)) :: acc)
      else
        loop courses t acc
  in
  loop (to_list sch) reqs.categories []


(** [satisfied l1 l2] is [true] if at least one element of [l1] is in [l2]. *)
let satisfies list1 list2 = 
  let rec loop test ref = 
    match test with
    | [] -> false
    | h :: t ->
      if List.mem h ref then true
      else loop t ref
  in
  loop list1 list2

(** [check_subs sch reqs] is a list of groups (lists) of courses where [sch] is
    missing at least one course from each group.

    If [check_subs sch reqs] = [] then [sch] contains at least one course from
    each group as per [reqs] and requirement is satisfied. *)
let check_subs sch reqs =
  let rec loop courses subs acc =
    match subs with 
    | [] -> acc
    | h :: t ->
      if satisfies h courses then
        loop courses t acc
      else
        loop courses t (h :: acc)
  in
  loop (List.map get_course_name (to_list sch)) reqs.subs []

let validate sch =
  let validness = 
    { 
      needed = check_required sch eng_reqs;
      needed_cat = check_categories sch eng_reqs;
      needed_subs = check_subs sch eng_reqs
    } 
  in set_valid sch (Some validness); validness

let print_validation v = 
  ANSITerminal.(print_string [Bold] 
                  "\nNOW CHECKING CS Engineering Requirements:\n");

  let print_required_course c =
    ANSITerminal.(print_string [red] "\nMissing Required Course: ");
    print_string c 
  in
  List.fold_left (fun _ c -> print_required_course c) () v.needed;
  print_newline ();

  let print_required_cat cat =
    ANSITerminal.(print_string [red] "\nNot enough courses from category: ");
    print_string cat 
  in
  List.fold_left (fun _ (c,_) -> print_required_cat c) () v.needed_cat;
  print_newline ();

  let print_required_subs c_lst =
    ANSITerminal.(print_string [red] "\nNo course from required group: ");
    print_string (string_of_list c_lst) 
  in
  List.fold_left (fun _ c -> print_required_subs c) () v.needed_subs;
  print_newline ()
