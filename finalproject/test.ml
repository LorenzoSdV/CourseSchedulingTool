(** TEST PLAN: 
    We tested the functions in schedule.ml using OUnit testing because these
    were the basic functions. This was mostly done for MS1. Manual testing
    was done once was the main.ml file was created and make run worked. 
    Manual testing made sure loading and saving JSON files worked, exporting
    to HTML worked, the settings worked, and the basic functionality. After
    MS1, our system was more complicated and simpler to test manually by
    running it. Manual testing was better for checking if courses and semesters
    were being added, edited, and removed correctly because a lot of our fields
    were mutable.

    NEED TO FINISH WRITING*)

open OUnit2
open Schedule

(** [string_of_sch sch] is a string representation of [sch] astisfying that 
    each unique [sch] has a unique string representation via this function
    (more or less). *)
let string_of_sch sch =
  let courses = to_list sch in
  List.fold_left (fun acc course -> acc ^ get_course_name course) "" courses

let make_int_test
    (name: string)
    (expected_output: int) 
    (actual_output: int) : test = 
  name >:: (fun _ -> assert_equal expected_output actual_output
               ~printer:string_of_int)

let make_float_test
    (name: string)
    (expected_output: float) 
    (actual_output: float) : test = 
  name >:: (fun _ -> assert_equal expected_output actual_output
               ~printer:string_of_float)              

let make_string_tests
    (name: string)
    (expected_output: string) 
    (actual_output: string) : test = 
  name >:: (fun _ -> assert_equal expected_output actual_output)  

let make_list_tests
    (name: string)
    (expected_output: string list)
    (actual_output: string list) : test = 
  name >:: (fun _ -> assert_equal (List.sort_uniq compare expected_output)
               (List.sort_uniq compare actual_output))   

let sch = new_schedule "Sch1"
let fall19 = create_sem (Fall 19)
let sp20 = create_sem (Spring 20)
let sch1 = add_sem sch fall19
let sch2 = add_sem sch sp20

let sch_rem_sem = new_schedule "Sch2"
let sch_add_sem1 = add_sem sch_rem_sem (create_sem (Fall 19))
let sch_add_sem2 = add_sem sch_rem_sem (create_sem (Spring 20))
let sch_rem_sem1 = remove_sem sch_rem_sem (Fall 19)

let cs2800 = create_course "CS2800" 3 (Letter "C") "core"
let cs4820 = create_course "CS4820" 4 (Letter "C+") "core"
let add_course1 = add_course sch cs2800 (Fall 19)
let add_course2 = add_course sch cs4820 (Spring 20)

(*let sch2 = add_sem sch fall_sem
  let cs3110 = create_course "CS3110" 4 (Letter "B") "CScore"
  let cs2800 = create_course "CS2800" 3 (Letter "C") "CScore"
  let cs4820 = create_course "CS4820" 4 (Letter "C+") "CScore"
  let phys2213 = create_course "PHYS2213" 4 (Letter "A-") "Engineering"
  let sch3 = add_course sch2 cs3110 (Fall 19)
  let sch4 = add_course sch3 cs2800 (Fall 19)
  let sp_sem = create_sem (Spring 20)
  let sch5 = add_sem sch sp_sem
  let sch6 = add_course sch5 cs4820 (Spring 20)
  let sch7 = add_course sch6 phys2213 (Spring 20)
  let sch8 = remove_course sch7 "CS3110"
  let sch9 = edit_course sch8 "PHYS2213" "credits" "3"
  let sch10 = edit_course sch9 "CS2800" "credits" "4"*)


let basic_schedule_tests = [
  make_list_tests "Semesters are added to schedule successfully"
    ["FA19"; "SP20"] (sem_ids_to_string sch2);
  make_list_tests "Semester removed from schedule successfully" ["SP20"]
    (sem_ids_to_string sch_rem_sem1);
  make_string_tests "Course added to schedule successfully" "CS2800CS4820"
    (string_of_sch sch);
  make_int_test "Schedule has correct number of credits" 7
    (get_credits sch);
  make_string_tests "Schedule has correct GPA" "2.17"
    (get_gpa sch |> gpa_to_string);
]

let example_sch = LoadJSON.parse_json "example.json"
let fa19_courses = 
  get_sem example_sch (Fall 19) |> get_sem_courses
let sp20_courses = 
  get_sem example_sch (Spring 20) |> get_sem_courses

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

let test_suite = [
  basic_schedule_tests;
  load_schedule_tests
]

let suite = "Main Test Suite" >::: List.flatten test_suite

let _ = run_test_tt_main suite