open Companies
open Difficulty

exception Invalid_ticker
exception OverTrade
type name = string
type ticker = string
type base = float

type ct = Companies.t

type t = {
  difficulty: string;
  start_balance: float;
  balance: float;
  portfolio: (ticker * float) list;
  portfolio_value : float;
  companies: ct;
  round: int;
}

let init_state c_lst diff = 
  if diff = Easy 
  then
    {
      difficulty = "easy";
      start_balance = 10000.0;
      balance = 10000.0;
      portfolio = [];
      portfolio_value = 0.0;
      companies = c_lst;
      round = 1;
    }
  else if diff = Medium
  then
    {
      difficulty = "medium";
      start_balance = 10000.0;
      balance = 10000.0;
      portfolio = [];
      portfolio_value = 0.0;
      companies = c_lst;
      round = 1;
    }
  else if diff = Hard
  then
    {
      difficulty = "hard";
      start_balance = 10000.0;
      balance = 10000.0;
      portfolio = [];
      portfolio_value = 0.0;
      companies = c_lst;
      round = 1;
    }
  else
    {
      difficulty = "sandbox";
      start_balance = 10000.0;
      balance = 10000.0;
      portfolio = [];
      portfolio_value = 0.0;
      companies = c_lst;
      round = 1;
    }

let difficulty (g:t) =
  g.difficulty

let rec comp_list (g:t) acc = 
  name_list g.companies []

let comp_name g tkr = 
  name g.companies tkr

(** [current_share_price c_lst c] represents the current share price for company 
    [c] in company list [c_lst]. 
    Raises: Invalid_ticker if company with ticker [c] is not in [c_lst]. *)
let rec current_share_price_helper c_lst c = 
  match c_lst with
  | [] -> raise Invalid_ticker
  | h::t -> 
    if h.ticker = c 
    then h.price 
    else current_share_price_helper t c

let current_share_price g c = 
  current_share_price_helper (Companies.to_list g.companies) c

(** [last_share_price c_lst c] represents the most recent share price that is 
    not the current share price for company [c] in company list [c]. 
    Raises: Invalid_ticker if company with ticker [c] is not in [c_lst]. *)
let rec last_share_price_helper c_lst c =
  match c_lst with
  | [] -> raise Invalid_ticker
  | h::t ->
    if h.ticker = c
    then
      let price_lst = h.past_prices in
      match price_lst with
      | [] -> h.price
      | h::t -> h
    else last_share_price_helper t c

let last_share_price g c =
  last_share_price_helper (Companies.to_list g.companies) c

(** [price_history_helper c_lst c] represents the list of the last 10 share 
    prices for company [c] in company list [c_lst]. 
    Raises: Invalid_ticker if company with ticker [c] is not in [c_lst]. *)
let rec price_history_helper c_lst c =
  match c_lst with
  | [] -> raise Invalid_ticker
  | h::t ->
    if h.ticker = c
    then
      let rec last_ten_prices lst count acc =
        begin
          match count with
          | 0 -> acc
          | _ -> 
            match lst with
            | [] -> last_ten_prices [] (count - 1) (-1.::acc)
            | h::t -> last_ten_prices (List.tl lst) (count - 1) 
                ((List.hd lst)::acc)
        end
      in
      last_ten_prices h.past_prices 10 []
    else price_history_helper t c

let price_history g c =
  price_history_helper (Companies.to_list g.companies) c

(** [share_price_list c_lst p_lst] represents the list of current share prices 
    [p_lst] for all companies in company list [c_lst] *)
let rec share_price_list_helper c_lst p_lst =
  match c_lst with
  | [] -> List.rev p_lst
  | h::t -> share_price_list_helper t (h.price :: p_lst)
(* this seems kind of off *)
(* ((current_share_price_helper c_lst h.ticker)::p_lst) *)

let share_price_list g = 
  share_price_list_helper (Companies.to_list g.companies) []

(* let rec current_balance_helper pfolio g acc = 
   match pfolio with
   | [] -> acc
   | h::t -> current_balance_helper t g 
              (acc +. (*(current_share_price g (fst h))*) -. (snd h)) *)

let current_balance g = g.balance

let start_balance g = g.start_balance

let to_ticker str : ticker = str

let to_string (tck : ticker) : string = tck

let to_string_name (name : name) : string = name

let rec prnt_space num =
  match num with
  | 0 -> 0
  | _ -> ANSITerminal.(print_string [Underlined; default]  " ");
    prnt_space (num - 1)

let valid_ticker g tck = 
  let c_list = Companies.to_list g.companies in
  let rec check lst tck = 
    match lst with
    | [] -> false
    | h::t -> if h.ticker = tck then true else check t tck
  in
  check c_list tck

let rec port_list g acc = 
  let lst = g.portfolio in
  let rec loop lst acc =
    match lst with
    | [] -> List.rev acc
    | h::t -> loop t ((fst h)::acc)
  in loop lst []

let rec get_portfolio g = 
  let lst = g.portfolio in
  let rec loop lst acc =
    match lst with
    | [] -> List.rev acc
    | h::t -> loop t (h::acc)
  in loop lst []

let portfolio_ticker g tck =
  let p_list = port_list g [] in
  let rec check lst tck = 
    match lst with
    | [] -> false
    | h::t -> if h = tck then true else check t tck
  in
  check p_list tck

let rec portfolio_value_helper g (pfolio : (ticker * float) list) acc =
  match pfolio with
  | [] -> acc
  | h::t -> 
    portfolio_value_helper g t (acc +. (current_share_price g (fst h) *. snd h))

let portfolio_value g pfolio = portfolio_value_helper g pfolio 0.

(** [update_comp_prices c diff] updates prices in company list [c] depending on
    difficulty [diff] *)
let update_comp_prices (c : Companies.t) diff =
  match String.lowercase_ascii diff with
  | "easy" -> update c Easy
  | "medium" -> update c Medium
  | "hard" -> update c Hard
  | _ -> update c Sandbox

(** [update_portfolio_buy g tkr n v] updates the portfolio in game [g]
    with [n] shares of company with ticker [tkr] and of total value [v]. 
    Raises: OverTrade if v > g.balance. *)
let update_portfolio_buy g tck amnt value = 
  let balance = g.balance in
  if value > balance
  then raise OverTrade
  else begin
    let rec srch_port lst tck amnt acc add_bool =
      match lst with
      | [] -> if add_bool then List.rev acc else List.rev ((tck, amnt)::acc)
      | h::t -> begin
          if (fst h) = tck 
          then let curr_amnt = snd h in
            srch_port t tck amnt ((tck,curr_amnt +. amnt)::acc) true
          else srch_port t tck amnt (h::acc) add_bool
        end
    in srch_port g.portfolio tck amnt [] false
  end

(** [update_portfolio_sell g tkr n] sells [n] shares of company with 
    ticker [tkr] in game [g]. 
    Raises: OverTrade if n > amount of stock in portfolio. *)
let update_portfolio_sell g tck amnt = 
  let rec comp_port lst tck amnt acc =
    match lst with
    | [] -> List.rev acc
    | h::t -> begin
        if (fst h) = tck then begin
          let stock_amnt = snd h in
          if amnt > stock_amnt
          then raise OverTrade
          else if amnt = stock_amnt
          then comp_port t tck amnt acc
          else comp_port t tck amnt ((tck, stock_amnt -. amnt)::acc)
        end
        else comp_port t tck amnt (h::acc)
      end
  in comp_port g.portfolio tck amnt []

let buy_stock g tck amnt = 
  if valid_ticker g tck
  then
    let value = 
      (current_share_price g tck) *. amnt in 
    {
      difficulty = g.difficulty;
      start_balance = g.start_balance;
      balance = g.balance -. value; 
      portfolio = update_portfolio_buy g tck amnt value;
      portfolio_value = g.portfolio_value +. value;
      companies = update_comp_prices g.companies g.difficulty;
      round = g.round + 1;
    }
  else raise Invalid_ticker

let sell_stock g tck amnt = 
  if valid_ticker g tck && portfolio_ticker g tck
  then
    let value = 
      (current_share_price g tck) *. amnt in 
    {
      difficulty = g.difficulty;
      start_balance = g.start_balance;
      balance = g.balance +. value; 
      portfolio = update_portfolio_sell g tck amnt;
      portfolio_value = g.portfolio_value -. value;
      companies = update_comp_prices g.companies g.difficulty;
      round = g.round + 1;
    }
  else 
  if not (valid_ticker g tck) then raise Invalid_ticker else raise OverTrade

let wait g num =
  let companiest = g.companies in
  let orig_num = num in
  let comp' =
    let rec loop comp num =
      match num with
      | 0 -> comp
      | _ -> loop (update_comp_prices comp g.difficulty) (num - 1) 
    in loop companiest num 
  in
  {
    difficulty = g.difficulty;
    start_balance = g.start_balance;
    balance = g.balance; 
    portfolio = g.portfolio;
    portfolio_value = g.portfolio_value;
    companies = comp';
    round = g.round + orig_num;
  }

let delta_price_value g tkr =
  let curr_price = current_share_price g tkr in
  let old_price = last_share_price g tkr in
  curr_price -. old_price

let delta_price g tkr =
  let diff = delta_price_value g tkr |> truncatef in
  if diff = 0.
  then "(/) $" ^ Float.to_string (Float.abs diff)
  else if diff > 0.
  then "(+) $" ^ Float.to_string (Float.abs diff)
  else "(-) $" ^ Float.to_string (Float.abs diff)

let growth g tkr (history_lst : float list) (period : int) : (float * float) =
  let rec find_comp_price lst count =
    match lst with
    | [] -> raise OverTrade
    | h::t -> if count = 0 then h else find_comp_price t (count - 1)
  in
  let price_old = find_comp_price history_lst period in
  if price_old = -1.
  then (Float.neg_infinity, 0.)
  else begin
    let price_curr = current_share_price g tkr in
    let price_diff = price_curr -. price_old in
    let grow_raw = (price_curr /. price_old) -. 1. in
    let sub = mod_float grow_raw 0.01 in
    ((grow_raw -. sub), price_diff)
  end

let round g = g.round

let net_worth g = 
  let lst = get_portfolio g in
  (current_balance g) +. (portfolio_value g lst)

(* Implementing save feature *)
open Yojson.Basic

(** [p_to_string p p_str] returns [p_str] the JSON string representation of the 
    player's portfolio [p]. *)
let rec p_to_string p p_str = 
  match p with
  | [] -> p_str
  | (tkr, n)::t -> p_to_string t 
                     (if p_str = "" 
                      then p_str^"{\"ticker\": \""^tkr^"\",\n"
                           ^"\"amount\": "^(string_of_float n)^"0}"
                      else p_str^",\n{\"ticker\": \""^tkr^"\",\n"
                           ^"\"amount\": "^(string_of_float n)^"0}")  

(** [g_to_string g] returns the JSON string representation of the game [g]. *)
let g_to_string g =
  "{\n"^"\"difficulty\": \""^g.difficulty^"\",\n"
  ^"\"start_balance\": "^string_of_float(g.start_balance)^"0,\n"
  ^"\"balance\": "^string_of_float(g.balance)^"0,\n"
  ^"\"portfolio\": [\n"^(p_to_string g.portfolio "")^"\n]\n,"
  ^"\"portfolio_value\": "^string_of_float(g.portfolio_value)^"0,\n"
  ^"\"companies\": ["^(c_to_string g.companies "")^"],"
  ^"\"round\": "^string_of_int(g.round)^"\n}"

(** [save g] saves the game [g] in JSON file "save.json". *)
let save g =
  let oc = open_out "save.json" in
  output_string oc (g_to_string g);
  close_out oc

open Yojson.Basic.Util

(** [read_portfolio j] is the portfolio that [j] represents.
    Requires: [j] is a valid JSON representation of a portfolio. *)
let read_portfolio json = 
  (json |> member "ticker" |> to_string, json |> member "amount" |> to_float)

(** [read_game j] is the game that [j] represents.
    Requires: [j] is a valid JSON representation of a game. *)
let read_game json = 
  {
    difficulty = json |> member "difficulty" |> to_string; 
    start_balance = json |> member "start_balance" |> to_float;
    balance = json |> member "balance" |> to_float;
    portfolio = json |> member "portfolio" |> to_list |> 
                List.map read_portfolio;
    portfolio_value = json |> member "portfolio_value" |> to_float;
    companies = Companies.read_saved_json json;
    round = json |> member "round" |> to_int
  }

let load () = let f = Yojson.Basic.from_file "save.json" in 
  read_game f