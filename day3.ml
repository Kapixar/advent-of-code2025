let read_lines name =
	let ic = open_in name in
	let try_read () =
	  try Some (input_line ic) with End_of_file -> None in
	let rec loop acc = match try_read () with
	  | Some s -> loop (s :: acc)
	  | None -> close_in ic; List.rev acc in
	loop [];;

let rec solve lines acc func = 
match lines with
  [] -> acc
  | head :: tail -> 
    solve tail (acc + func head) func;;


let lines = read_lines "txt/day3_test.txt";;
(* let lines = read_lines "txt/day3.txt";; *)


module StringMap = Map.Make(String)

let index_map : int list StringMap.t = StringMap.empty;;

let string_to_char_list s =
  s |> String.to_seq |> List.of_seq

let findLargest str = 
  let string = string_to_char_list str in
  (* let rec iter prev curr left = 
    match left with
      [] -> true
      | head :: tail -> 
        if head > curr then false
        else iter curr head tail
      in
      iter  *)
  (* let len = String.length string in *)
  let rec parse left map idx= 
    match left with
      [] -> map
      | head :: tail -> 
        let key = String.make 1 head in
        match StringMap.find_opt key map with
        Some list -> 
          let newMap = StringMap.add key (idx :: list) map in
          parse tail newMap (idx + 1)
        | None -> 
          let newMap = StringMap.add key [idx] map in
          parse tail newMap (idx + 1)
          in 
          
  let mapz = parse string index_map 0 in
  (* starting from 9, find smallest index in map, save to largest, then find largest index, also starting from 9 *)
  let rec find_from_digit digit largest smallest =
    if digit < '0' then (largest, smallest)
    else 
      let key = String.make 1 digit in
      match StringMap.find_opt key mapz with
        Some indices -> 
          let sorted_indices = List.sort compare indices in
          let new_largest = match largest with
            Some v -> Some (max v (List.hd (List.rev sorted_indices)))
            | None -> Some (List.hd (List.rev sorted_indices)) in
          let new_smallest = match smallest with
            Some v -> Some (min v (List.hd sorted_indices))
            | None -> Some (List.hd sorted_indices) in
          find_from_digit (Char.chr (Char.code digit - 1)) new_largest new_smallest
        | None -> find_from_digit (Char.chr (Char.code digit - 1)) largest smallest
  in
  let (largest_opt, smallest_opt) = find_from_digit '9' None None in
  match (largest_opt, smallest_opt) with
    (Some largest, Some smallest) -> largest * 10 + smallest
    | _ -> 0

;;
  

solve lines 0 findLargest;;
