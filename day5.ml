let read_lines name =
	let ic = open_in name in
	let try_read () =
	  try Some (input_line ic) with End_of_file -> None in
	let rec loop acc = match try_read () with
	  | Some s -> loop (s :: acc)
	  | None -> close_in ic; List.rev acc in
	loop [];;


let lines = read_lines "txt/day5.txt";;
let split_point = 4;

(* let ranges = List.split *)
