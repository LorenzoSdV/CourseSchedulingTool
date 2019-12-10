open Schedule

type reqs = {
  required : string list;
  categories : (string * int) list;
  subs : (string * string) list
}

let eng_reqs = {
  required = ["CS1110"; "CS2110"; "CS2800"; "CS3110"; "CS3410"; "CS4410"; 
              "CS4820"; "MATH1910"; "MATH1920"; "MATH2940"; "CHEM2090";
              "CHEM2080"; "PHYS1112"; "PHYS2213"; "ENGRI"; "ENGRD";
              "ENGRD2700"; "Project Course"; "Free Elective"; "Eng Comm";];
  categories = [("Technical", 9); ("Ext Spec", 9); ("4000+", 9); 
                ("Lib Studies", 18); ("FWS", 6); ("PE", 2); 
                ("Advisor-Approved Elective", 6); ];
  subs = [("CS1110", "CS1112"); ("CS2110","CS2112"); ("CS2800", "CS2802");
          ("CHEM2080", "BTRY3080"); ("CHEM2080", "ECON3130"); ("CHEM2080", "MATH2930");
          ("CHEM2080", "MATH4710"); ("CHEM2080", "PHYS2214"); ("CHEM2080", "PHYS2218");
          ("CS3410", "CS3420"); ("CS3410", "ECE3140"); ("ENGRD2700", "BTRY3080");
          ("ENGRD2700", "CS4850"); ("ENGRD2700", "ECE3100"); ("ENGRD2700", "ECON3130");
          ("ENGRD2700", "MATH4710") ]
}

let arts_reqs = {
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
}

let substitutes course subs = 
  List.fold_left

let check_required sch required =
  let rec loop courses reqs acc =
    match reqs with 
    | [] -> acc
    | h :: t ->
      if List.mem h courses || List.mem h (substitutes h) then
        loop courses t acc
      else
        loop courses t (h :: acc)
  in
  loop (List.map get_course_name (to_list sch)) required []

let check_categories sch categories = failwith "Unimplemented"