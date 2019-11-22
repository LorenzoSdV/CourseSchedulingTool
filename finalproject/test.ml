open OUnit2
open Schedule

let sch = new_schedule
let fall_sem = create_sem (Fall 2019)
let sch2 = add_sem sch fall_sem
let cs3110 = create_course "CS3110" 4 (Letter "B") "CScore"
let cs2800 = create_course "CS2800" 3 (Letter "C") "CScore"
let cs4820 = create_course "CS4820" 4 (Letter "C+") "CScore"
let phys2213 = create_course "PHYS2213" 4 (Letter "A-") "Engineering"
let sch3 = add_course sch2 cs3110 (Fall 2019)
let sch4 = add_course sch3 cs2800 (Fall 2019)
let sp_sem = create_sem (Spring 2020)
let sch5 = add_sem sch sp_sem
let sch6 = add_course sch5 cs4820 (Spring 2020)
let sch7 = add_course sch6 phys2213 (Spring 2020)
let sch8 = remove_course sch7 "CS3110"
let sch9 = edit_course sch8 "PHYS2213" "credits" "3"
let sch10 = edit_course sch9 "CS2800" "credits" "4"

let make_sch_creds_tests
    (name: string)
    (sched: schedule)
    (expected_output: int) : test = 
  name >:: (fun _ -> assert_equal expected_output (get_credits sched)
               ~printer:string_of_int)

let make_sem_creds_tests
    (name: string)
    (sem: semester)
    (expected_output: int) : test = 
  name >:: 
  (fun _ -> assert_equal expected_output (get_sem_courses sem |> calc_credits)
      ~printer:string_of_int)        

let tests = [
  (** Schedule tests *)
  make_sch_creds_tests "total credits of sch10" sch10 11;
  make_sch_creds_tests "total credits with removed class" sch8 11;
  make_sch_creds_tests "total credits with edited class, raised # credits"
    sch10 11;

]

let suite = "search test suite" >::: tests 

let _ = run_test_tt_main suite; HTML.export_schedule sch10 "test.html"