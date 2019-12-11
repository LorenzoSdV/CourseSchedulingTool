(** TEST PLAN: 

    We tested the functions in the Schedule, LoadJSON, SaveJSON, iCalParser, 
    and classRoster modules using OUnit testing because these were the basic 
    functions. For the rest of the code (main.ml, command.ml, requirements.ml),
    we used manual/interactive testing. Obviously, this could only be done once
    main.ml was funcitonal and "make run" worked. 

    Manual testing made sure loading and saving JSON files worked, exporting
    to HTML worked, the settings worked, and the basic functionality. After
    MS1, our system was more complicated and simpler to test manually by
    running it. Manual testing was better for checking if courses and semesters
    were being added, edited, and removed correctly because a lot of our fields
    were mutable -- which made them difficult to test with OUnit cases.

    Most of the testing was done after the function being tested was written,
    however, the person testing the function was not the same person who
    wrote it, therefore, most of the testing was black-box. The testing
    was based off of functions in schedule.mli and their documentation and 
    not based on the implementation of the function.

    We believe that (primarily) manual testing was a sufficient strategy for
    this system because this is an interactive tool, and we felt the best way
    to ensure its success was to thoroughly test it as the user would use it.
*)

open OUnit2
open Schedule

(** [string_of_sch sch] is a string representation of [sch] astisfying that 
    each unique [sch] has a unique string representation via this function
    (more or less). *)
let string_of_sch sch =
  let courses = to_list sch in
  List.fold_left (fun acc course -> acc ^ get_course_name course) "" courses

(** [make_int_test name expected_output actual_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output] with
    [actual_output] for ints. *)
let make_int_test
    (name: string)
    (expected_output: int) 
    (actual_output: int) : test = 
  name >:: (fun _ -> assert_equal expected_output actual_output
               ~printer:string_of_int)

(** [make_float_test name expected_output actual_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output] with
    [actual_output] for floats. *)
let make_float_test
    (name: string)
    (expected_output: float) 
    (actual_output: float) : test = 
  name >:: (fun _ -> assert_equal expected_output actual_output
               ~printer:string_of_float)              

(** [make_string_tests name expected_output actual_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output] with
    [actual_output] for strings. *)
let make_string_tests
    (name: string)
    (expected_output: string) 
    (actual_output: string) : test = 
  name >:: (fun _ -> assert_equal expected_output actual_output) 

(** [make_list_tests name expected_output actual_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output] with
    [actual_output] for lists. *)
let make_list_tests
    (name: string)
    (expected_output: string list)
    (actual_output: string list) : test = 
  name >:: (fun _ -> assert_equal (List.sort_uniq compare expected_output)
               (List.sort_uniq compare actual_output))   

(* 
    Code used in all test cases:
*)

let fall19 = create_sem (Fall 19)
let sp20 = create_sem (Spring 20)

let cs2800 = create_course "CS2800" 3 (Letter "C") "CORE"
let cs4820 = create_course "CS4820" 4 (Letter "C+") "CORE"
let phys2213 = create_course "PHYS2213" 4 (Letter "A-") "REQUIRED"
let cs3110 = create_course "CS3110" 4 (Letter "B") "CORE"

(*
    Schedule module tests
*)

let sch = new_schedule "Sch1"

let _ = add_sem sch fall19
let _ = add_sem sch sp20

let _ = add_course sch cs2800 (Fall 19)
let _ = add_course sch cs4820 (Spring 20)
let _ = add_course sch cs3110 (Fall 19)
let _ = add_course sch phys2213 (Spring 20)

let sch_rem_sem = new_schedule "Sch2"
let _ = add_sem sch_rem_sem (create_sem (Fall 19))
let _ = add_sem sch_rem_sem (create_sem (Spring 20))
let _ = remove_sem sch_rem_sem (Fall 19)

let _ = add_course sch_rem_sem cs2800 (Spring 20)
let _ = add_course sch_rem_sem cs3110 (Spring 20)
let _ = add_course sch_rem_sem cs4820 (Spring 20)
let _ = remove_course sch_rem_sem "CS2800"

let edit_creds_sch = new_schedule "Sch3"
let _ = add_sem edit_creds_sch (create_sem (Fall 20))
let _ = add_course edit_creds_sch phys2213 (Fall 20)
let _ = edit_course edit_creds_sch "PHYS2213" "credits" "3"

let edit_grade = new_schedule "Sch4"
let _ = add_sem edit_grade (create_sem (Fall 20))
let _ = add_course edit_grade phys2213 (Fall 20)
let _ = edit_course edit_grade "PHYS2213" "grade" "B"

let unswapped = new_schedule "Sch5"
let _ = add_sem unswapped (create_sem (Fall 20))
let _ = add_sem unswapped (create_sem (Spring 21))
let _ = add_course unswapped cs2800 (Spring 21)
let _ = add_course unswapped cs3110 (Fall 20)

let swapped = new_schedule "Sch6"
let _ = add_sem swapped (create_sem (Fall 20))
let _ = add_sem swapped (create_sem (Spring 21))
let _ = add_course swapped cs2800 (Spring 21)
let _ = add_course swapped cs3110 (Fall 20)
let _ = swap_courses "CS2800" "CS3110" swapped

let basic_schedule_tests = [
  make_list_tests "Semesters are added to schedule successfully"
    ["FA19"; "SP20"] (sem_ids_to_string sch);
  make_list_tests "Semester removed from schedule successfully" ["SP20"]
    (sem_ids_to_string sch_rem_sem);
  make_int_test "Courses added to schedule successfully (number)" 
    4 (to_list sch |> List.length);
  make_string_tests "Courses added to schedule successfully (print)"
    "CS3110CS2800PHYS2213CS4820" (string_of_sch sch);
  make_int_test "Schedule has correct number of credits" 15
    (get_credits sch);
  make_string_tests "Schedule has correct GPA" "2.80"
    (get_gpa sch |> gpa_to_string);
  make_string_tests "Schedule correctly removed course (print)"
    "CS4820CS3110" (string_of_sch sch_rem_sem);
  make_int_test "Schedule correctly removed course (number)"
    2 (to_list sch_rem_sem |> List.length);
  make_int_test "Removed course schedule has correct number of credits"
    8 (get_credits sch_rem_sem);
  make_string_tests "Removed course schedule has correct GPA"
    "2.65" (get_gpa sch_rem_sem |> gpa_to_string);
  make_int_test "Edited course credits - check num credits" 3 
    (get_credits edit_creds_sch);
  make_string_tests "Edited course credits - check GPA" "3.70"
    (get_gpa edit_creds_sch |> gpa_to_string);
  make_string_tests "Edited course grade - check GPA" "3.00"
    (get_gpa edit_grade |> gpa_to_string);
  make_string_tests "Schedule added two courses successfully" 
    "CS3110CS2800" (string_of_sch unswapped);
  make_string_tests "Schedule swapped the two courses in the previous test
  successfully" "CS2800CS3110" (string_of_sch swapped);

]

let command_tests = [

]

(*
    LoadJSON Tests
*)

let example_sch = LoadJSON.parse_json "example.json"
let fa19_courses = get_sem example_sch (Fall 19) |> get_sem_courses
let sp20_courses = get_sem example_sch (Spring 20) |> get_sem_courses

let load_schedule_tests = [
  make_int_test "Credits of example.json schedule" 15 (get_credits example_sch);
  make_string_tests
    "Cumulative GPA for example.json" "3.03" 
    (gpa (to_list example_sch) |> gpa_to_string);
  make_string_tests 
    "Desc for example.json schedule" "Example Schedule" (get_name example_sch);
  make_string_tests 
    "FA19 GPA for example.json sch" "3.30" (gpa fa19_courses |> gpa_to_string);
  make_int_test "Correct number of courses in FA19 in example.json" 2
    (List.length fa19_courses);
  make_string_tests 
    "SP20 GPA for example.json sch" "2.80" (gpa sp20_courses |> gpa_to_string);
  make_int_test "Correct number of courses in SP20 in example.json" 2
    (List.length sp20_courses);
  make_string_tests "Sch is correct in example.json" 
    "CS2800CS3110PHYS2213CS4820" (string_of_sch example_sch)
]

let ical = ICalParser.parse_file "example.ics"

let ical_tests = [

]


(*
    SaveJSON Tests
    These tests work by saving and then re-loading a schedule.
*)




(* 
    END Test Cases!
*)

let test_suite = [
  basic_schedule_tests;
  load_schedule_tests;
  ical_tests;
  command_tests;
]

let suite = "Main Test Suite" >::: List.flatten test_suite

let _ = run_test_tt_main suite