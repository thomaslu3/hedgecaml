type phrase = string list

type command = 
  | Help
  | Companies
  | Buy of phrase
  | Sell of phrase
  | Balance
  | Price of phrase
  | Portfolio
  | Wait of phrase
  | Save
  | Round
  | Quit

exception EmptyCommand

exception ErrorParsing

let make_command first lst =
  if first = "help" && List.length lst = 0 then Help
  else if first = "companies" && List.length lst = 0 then Companies
  else if first = "buy" && List.length lst <> 0 then Buy lst
  else if first = "sell" && List.length lst <> 0 then Sell lst
  else if first = "balance" && List.length lst = 0 then Balance
  else if first = "price" && List.length lst <> 0 then Price lst
  else if first = "portfolio" && List.length lst = 0 then Portfolio
  else if first = "wait" && List.length lst <> 0 then Wait lst
  else if first = "wait" && List.length lst = 0 then Wait []
  else if first = "round" && List.length lst = 0 then Round
  else if first = "quit" && List.length lst = 0 then Quit
  else if first = "save" && List.length lst = 0 then Save
  else raise (ErrorParsing)

let parse str = 
  let cmnd_lst = (String.split_on_char ' ' str |>
                  List.filter (fun x -> x <> "")) in
  match cmnd_lst with
  | [] -> raise (EmptyCommand)
  | h::t -> make_command (String.lowercase_ascii h) t

(** [to_list_helper p] is the list of strings [acc] in phrase [p]. *)
let rec to_list_helper (p : phrase) acc = 
  match p with
  | [] -> List.rev acc
  | h::t -> to_list_helper t (h::acc)

let to_list (p:phrase) = to_list_helper p []