let read_lines name =
	let ic = open_in name in
	let try_read () =
	  try Some (input_line ic) with End_of_file -> None in
	let rec loop acc = match try_read () with
	  | Some s -> loop (s :: acc)
	  | None -> close_in ic; List.rev acc in
	loop [];;


(* let lines = read_lines "txt/day2.txt";; *)
let line = read_lines "txt/day2.txt";;

let isEvenLength n =
  (String.length n) mod 2 = 0

let isInvalidPart1 str =
  let len = String.length str in
  let half = len / 2 in
  let firstHalf = String.sub str 0 half in
  let secondHalf = String.sub str half (len - half) in
  firstHalf = secondHalf
;;

let count lower upper func = 
  (* match (isEvenLength lower, isEvenLength upper) with
  (true, true) -> print_string "s ";6
  | (true, false) -> print_string "c ";7
  | (false, true) -> print_string "b ";5
  | (false, false) -> print_string "a ";0
  (* print_string lower; *)
  (* print_string " ";
  print_int (int_of_string upper - int_of_string lower);
  print_string " ";
  (* print_string upper; *)
  print_string " | "; *) *)
  let rec iter current acc =
    if current > int_of_string upper then acc
    else 
      let str_current = string_of_int current in
      let new_acc = if func str_current then 
        (print_int current; print_string " "; acc + current) 
      
    else acc in
      iter (current + 1) new_acc
  in iter (int_of_string lower) 0
;;


let solve line func = 
  let rec iter list acc = 
    match list with
      [] -> acc
      | head :: tail -> 
        match String.split_on_char '-' head with
          [bottom; upper] -> iter tail (count bottom upper func + acc)
          | _ -> print_string "Error in parsing range";0
  in iter (String.split_on_char ',' line) 0;;

let isInvalidPart2 str =
  let len = String.length str in
  let half = len / 2 in
  let rec check currentLen =
    if currentLen > half then false
    else
      if len mod currentLen != 0 then check (currentLen + 1)
      else 
        let firstPart = String.sub str 0 currentLen in
        let rec buildString times =
          if times <= 0 then ""
          else firstPart ^ (buildString (times - 1))
        in
        let built = buildString (len / currentLen) in
        if built = str then true
        else check (currentLen + 1)

  in check 1;;

solve (List.hd line) isInvalidPart1;;

(* 22680554930 *)
(* too low *)
solve (List.hd line) isInvalidPart2;;

isInvalidPart2 "824824824";;