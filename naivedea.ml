type dea =
  { states  : string array;
    inputs  : string array;
    delta   : int -> int -> int;
    start   : string;
    ends    : string array;
  }

let rec get_index arrayof element =
  if arrayof.(0) = element then 0 else 1 + get_index (Array.sub arrayof 1 (Array.length arrayof - 1)) element;;

let rec deltahat dea state inputs =
  if inputs = "" then state else deltahat dea dea.states.(
      dea.delta (get_index dea.states state) (get_index dea.inputs (String.sub inputs 0 1)))
      (String.sub inputs 1 (String.length inputs - 1));;

let s = [|"q0";"q1"|];;

let i = [|"a";"b"|];;

let d state input =
  if state == 0 then
    if input == 0 then
      0
    else
      1
  else
    1;;

let st = "q0";;

let e = [|"q0"|];;

let mydea = {states = s; inputs = i; delta = d; start = "q0"; ends = e};;

print_string (deltahat mydea mydea.start "abab");print_newline ();
print_string (deltahat mydea mydea.start "aaaa");print_newline ();
print_string (deltahat mydea mydea.start "aaba");print_newline ();
