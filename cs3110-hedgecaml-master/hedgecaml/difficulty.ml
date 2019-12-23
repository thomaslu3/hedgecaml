open String

type difficulty =
  | Easy
  | Medium
  | Hard
  | Sandbox

exception Empty
exception Error 

let make_difficulty diff = let diff = lowercase_ascii diff in
  if diff = "easy" then Easy
  else if diff = "medium" then Medium
  else if diff = "hard" then Hard
  else if diff = "sandbox" then Sandbox
  else raise (Error)

let parse_diff str =
  let diff = (String.split_on_char ' ' str |>
              List.filter (fun x -> x <> "")) in
  match diff with
  | [] -> raise (Empty)
  | h::t -> make_difficulty (String.lowercase_ascii h)


