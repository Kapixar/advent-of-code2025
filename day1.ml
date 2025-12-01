let read_lines name =
	let ic = open_in name in
	let try_read () =
	  try Some (input_line ic) with End_of_file -> None in
	let rec loop acc = match try_read () with
	  | Some s -> loop (s :: acc)
	  | None -> close_in ic; List.rev acc in
	loop [];;


(* let lines = read_lines "txt/day1.txt";; *)
let lines = read_lines "txt/day1.txt";;

let start = 50;;
let ending = 100;;

let value_of c =
  match c with
  | '0' -> 0
  | '1' -> 1
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | _ -> 0

let parseCommand str = 
  (value_of (String.get str 0) - 2, int_of_string (String.sub str 1 (String.length str - 1)));;

(* let rec solve acc current lines = 
  match lines with
  [] -> acc
  | head :: tail -> 
    let (command, raw_value) = parseCommand head in
    let value = raw_value mod ending in
    let new_current = match command with
    'L' -> current - value
    | 'R' -> current + value
    | _ -> current
  in
  let diff_count = raw_value / 100 in
  let didPassZero = if (compare new_current 0 + compare current 0) == 0 then 1 else 0 in

  print_int new_current;
  print_string " ";
  print_int diff_count;
  print_string " | ";
  solve (acc + (if new_current mod 100 == 0 then 1 else 0) + diff_count) new_current tail
;; *)

let rec solve2 lines acc current = 
  let rec helper left direction zeroCount current =
    match left with
    0 -> (zeroCount, current)
    | ll -> 
      let new_current = current + direction in
      (* print_int left;
      print_string " ";
      print_int new_current;
      print_string " | "; *)
      helper (ll - 1) direction (zeroCount + (if (new_current mod 100) == 0 then 1 else 0)) new_current

  in match lines with
  [] -> acc
  | head :: tail -> 
    let (command, raw_value) = parseCommand head in
    let (zeroCount, new_current) = helper raw_value command 0 current in
    print_int raw_value;
    print_string " ";
    print_int zeroCount;
    print_string " | ";
    solve2 tail (acc + zeroCount) new_current;;

(* first 1158 *)

(* 8018 too high *)
(* 170975 too high *)

solve2 lines 0 start;;