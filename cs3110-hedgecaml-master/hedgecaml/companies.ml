(** A [Companies] is a representation of a game's commands.

    This module represents the commands of a game.
*)
type name = string

(** The type of company tickers. *)
type ticker = string
exception UnknownCompany of ticker

open Yojson.Basic.Util
open Difficulty

(** The type of a company. *)
type company = {
  name: name;
  ticker : ticker;
  price : float;
  volatility : float;
  past_prices : float list;
  trend : float
}

(** The type of companies. *)
type t = (company) list

(** [read_companies j] is the list of companies that [j] represents.
    Requires: [j] is a valid JSON representation of a company list. *)
let read_companies json = 
  Random.self_init ();
  let price_json = json |> member "price" |> to_float in
  {
    name = json |> member "name" |> to_string;
    ticker = json |> member "ticker" |> to_string;
    price = price_json;
    volatility = json |> member "volatility" |> to_float;
    past_prices = [];
    trend = (Random.float (price_json/.100.))-.(price_json/.200.)
  }

let read_json json : t = json |> member "companies" |> to_list |> 
                         List.map read_companies

let rec name_list (c:t) acc = 
  match c with
  | [] -> acc
  | h::t -> name_list t ((h.name, h.ticker)::acc)

let rec name (c:t) tkr = 
  match c with
  | [] -> raise (UnknownCompany tkr)
  | h::t -> if h.ticker = tkr then h.name else name t tkr

let rec price (c:t) tkr =
  match c with
  | [] -> raise (UnknownCompany tkr)
  | h::t -> if h.ticker = tkr then h.price else price t tkr

let rec volatility (c:t) tkr =
  match c with
  | [] -> raise (UnknownCompany tkr)
  | h::t -> if h.ticker = tkr then h.volatility else volatility t tkr

let rec past_prices (c:t) tkr = 
  match c with
  | [] -> raise (UnknownCompany tkr)
  | h::t -> if h.ticker = tkr then h.past_prices else past_prices t tkr

(** [truncatef v] truncates the float value [v] to two decimal places. *)
let truncatef value = 
  let sub = mod_float value 0.01 in
  value -. sub

(** [trend_helper t p v l] is a helper function for [change_trend c] that
    that updates a company's trend given previous trend, volatility, and 
    and previous stock prices. *)
let rec trend_helper trend p v prev = 
  match p with
  | [] -> trend
  | h::t -> let new_trend = trend+.(v*.(h-.prev)) in
    if (List.length t > 1) then
      trend_helper new_trend t v (List.nth t 1)
    else new_trend

(** [change_trend c] is the updated trend value for company c *)
let change_trend c = 
  let last_price = if List.length c.past_prices > 0 then List.hd c.past_prices
    else c.price in
  let new_trend = trend_helper c.trend (c.price::c.past_prices) c.volatility 
      (last_price) in
  if (c.price < 10. && (abs_float new_trend) > 2.*.c.price) then
    if (new_trend>0.) then 2.*.c.price
    else (-2.0)*.c.price 
  else if ((abs_float new_trend) > (c.price/.4.)) then
    if (new_trend>0.) then c.price/.4.
    else (c.price/.4.)*.(-1.0)
  else new_trend

(** [price_change p t] is a semi-randomized updated price given an old price 
    [p] and value for current trend [t] *)
let price_change price trend = 
  Random.self_init ();
  let price = (if (price<=0.) then 0. else price) in
  let add_to = Random.float trend in
  let add_too = 
    (if (price=0.) then 0.
     else
       (if add_to=0. then (Random.float 2.)-.1.
        else add_to)) in
  let final_price = 
    if (price +. add_too > 9999.) then 9999.
    else price +. add_too 
  in
  truncatef (final_price)

(** [update_helper c] is company [c] with its price, trend, and list of past 
    prices updated. *)
let update_helper c d = 
  Random.self_init ();
  let old_price = c.price in
  let new_pp = c.price::c.past_prices in
  let new_trend = 
    let reset = Random.float 1. in
    (* Each round, 40% possibility that trend randomly resets for the company *)
    if (reset > 0.6) then
      if (d=Easy) then (Random.float (old_price/.100.))-.(old_price/.220.) 
      else if (d=Hard) then (Random.float (old_price/.100.))-.(old_price/.170.) 
      else (Random.float (old_price/.100.))-.(old_price/.200.)
    else change_trend c
  in
  {
    name = c.name;
    ticker = c.ticker;
    price = price_change old_price c.trend;
    volatility = c.volatility;
    past_prices = new_pp;
    trend = new_trend
  }

(** [update_helper2 c u d] is the [update] helper function which updates each 
    company in [c] and places it in the new list of companies [u], given 
    difficulty [d]. *)
let rec update_helper2 (c:t) (updated:t) (d:difficulty) : t = 
  match c with
  | [] -> List.rev updated
  | h::t -> update_helper2 t ((update_helper h d)::updated) d

let update c d = update_helper2 c [] d

(** [to_list c lst] gives companies [c] as a company list [lst]. *)
let rec to_list_helper c lst = 
  match c with
  | [] -> List.rev lst
  | h::t -> to_list_helper t (h::lst)

let to_list c = to_list_helper c []


(* Implementing save feature *)
(** [pp_to_string pp pp_str] returns [pp_str] the JSON string representation of 
    a companies' past share prices [pp]. *)
let rec pp_to_string pp pp_str c =
  match pp with
  | [] -> pp_str^"]"
  | h::t -> pp_to_string (if c >= 11 then [] else t) 
              (if pp_str = ""
               then pp_str^"["^string_of_float h^"0"
               else pp_str^","^string_of_float h^"0") (c+1)

let rec c_to_string c_lst c_str = 
  match c_lst with
  | [] -> c_str
  | {name = nm; 
     ticker = tkr; 
     price = pr; 
     volatility = vol; 
     past_prices = pp; 
     trend = tr}::t -> c_to_string t 
                         (if c_str = "" 
                          then c_str^"{\n"
                               ^"\"name\": \""^nm^"\",\n"
                               ^"\"ticker\": \""^tkr^"\",\n"
                               ^"\"price\": "^string_of_float(pr)^"0,\n"
                               ^"\"volatility\": "^string_of_float(vol)^"0,\n"
                               ^"\"past_prices\": "^(pp_to_string pp "" 0)^",\n"
                               ^"\"trend\": "^string_of_float(tr)^"0}"
                          else c_str^",\n{\n"
                               ^"\"name\": \""^nm^"\",\n"
                               ^"\"ticker\": \""^tkr^"\",\n"
                               ^"\"price\": "^string_of_float(pr)^"0,\n"
                               ^"\"volatility\": "^string_of_float(vol)^"0,\n"
                               ^"\"past_prices\": "^(pp_to_string pp "" 0)^",\n"
                               ^"\"trend\": "^string_of_float(tr)^"0}\n")

(** [read_pp lst j] is the list of past prices that [j] represents.
    Requires: [j] is a valid JSON representation of list of past prices. *)
let rec read_pp lst json =
  match json with
  | [] -> List.rev lst 
  | h::t -> read_pp ((h |> to_float)::lst) t

(** [read_saved_companies j] is the saved company that [j] represents.
    Requires: [j] is a valid JSON representation of a company. *)
let read_saved_company json = {
  name = json |> member "name" |> to_string;
  ticker = json |> member "ticker" |> to_string;
  price = json |> member "price" |> to_float;
  volatility = json |> member "volatility" |> to_float;
  past_prices = 
    json |> member "past_prices" |> 
    Yojson.Basic.Util.to_list |> read_pp [];
  trend = json |> member "trend" |> to_float
}

(** [read_saved_json j] is the saved company list that [j] represents.
    Requires: [j] is a valid JSON representation of a company list. *)
let read_saved_json json : t = json |> member "companies" |> 
                               Yojson.Basic.Util.to_list |> 
                               List.map read_saved_company