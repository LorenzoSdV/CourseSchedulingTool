(* Will work on tonight! *)

let template =
  let chan = open_in "../temp.html" in
    Std.input_list chan

let html_of_course c =
  "<td>" ^ 
  "<p><strong>" ^ c.name ^ "</strong></p>\n" ^ 
  "<p>Credits: " ^ (string_of_int c.credits) ^ "</p>\n" ^
  "<p>Grade: " ^ (Schedule.string_of_grade c.grade) ^ "</p>\n" ^ 
  "</td>"

let html_of_sem sem =
  match sem.courses with
  | [] -> "<tr><td><h3>" ^ (Schedule.string_of_sem sem) ^ "</h3></td></tr>\n"
  | _ -> begin
      "<tr><td><h3>" ^ (Schedule.string_of_sem sem) ^ "</h3></td>\n" ^
      "<p>Semester GPA: <strong>" ^ sch.gpa ^ "</strong></p></td>\n" ^ 
      (List.fold_left (fun acc course -> acc ^ (html_of_course course)) "" sem.courses) ^ 
      "</tr>\n" end

let html_of_sch sch =
  match sch.semesters with
  | [] -> "<p>Schedule is empty!</p>\n"
  | _ -> begin
    "<h1>Schedule: <strong>" ^ sch.name ^ "</strong></h1>\n" ^ 
    "<h2>Cumulative GPA: <strong>" ^ sch.gpa ^ "</strong></h2>\n" ^ 
    "<table>\n" ^ 
    (List.fold_left (fun acc sem -> acc ^ (html_of_sem sem)) "" sch.semesters) ^ 
    "</table>\n" end

let save filename text = 
  let chan = open_out filename in
  output_string chan text;
  clost_out chan

let export_html sch fl = 
  let reg = Str.regexp {|^<\\?sch>$|} in
  exp_data = Str.global_first reg (html_of_schedule sch) template |> save fl



