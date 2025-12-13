let read_lines name =
	let ic = open_in name in
	let try_read () =
	  try Some (input_line ic) with End_of_file -> None in
	let rec loop acc = match try_read () with
	  | Some s -> loop (s :: acc)
	  | None -> close_in ic; List.rev acc in
	loop [];;


let lines = read_lines "txt/day7.txt";;

let lines_filtered = List.tl (List.filteri (fun i _ -> i mod 2 = 0) lines);;

let length = List.length lines_filtered;;

let memo = Hashtbl.create 1000;;

let rec checkDown x y =
  let key = (x, y) in
  if Hashtbl.mem memo key then Hashtbl.find memo key
  else
    let result =
      if y >= length then 1
      else 
        let line = List.nth lines_filtered y in
        if String.get line x = '^' then
          checkDown (x - 1) (y + 1) + checkDown (x + 1) (y + 1)
        else
          checkDown x (y + 1)
    in
    Hashtbl.add memo key result;
    result;;


checkDown (String.length (List.hd lines_filtered) / 2) 0;;