(** A [Game] is a representation of the current game state.

    This module represents the state of a players' portfolio and
    company price histories.
*)

open Companies
open Difficulty

(** Raised when an invalid ticker is input. *)
exception Invalid_ticker

(** Raised when a player tries to trade more stocks than they own or 
    can afford. *)
exception OverTrade

(** The type of company names. *)
type name

(** The type of company tickers. *)
type ticker

(** The type of companies. *)
type ct = Companies.t

(** The type of games. *)
type t

(** [init_state c_lst] is the initial state of the game. In that state,
    the player has no stocks in their portfolio and current share prices
    are taken from company list [c_lst]. *)
val init_state : ct -> difficulty -> t

(** [difficulty g] is the difficulty of the game [g]. *)
val difficulty : t -> string

(** [comp_list g] is the association list of company names and tickers in
    game [g]. *)
val comp_list : t -> (name * ticker) list -> (string * string) list

(** [comp_name g c] is the name for company [c] in game [g]. *)
val comp_name : t -> ticker -> name

(** [current_share_price g c] represents the current share price for company 
    [c] in game [g]. *)
val current_share_price : t -> ticker -> float

(** [last_share_price g c] represents the most recent share price that is not
    the current share price for company [c] in game [g]. *)
val last_share_price : t -> ticker -> float

(** [price_history g c] represents the list of the last 10 share prices
    for company [c] in game [g]. *)
val price_history : t -> ticker -> float list

(** [share_price_list g] represents the current share prices for all companies
    in game [g] *)
val share_price_list : t -> float list

(** [current_balance g] represents the current amount of cash that the player
    holds in game [g]. *)
val current_balance : t -> float

(** [start_balance g] represents the current value of the players' 
    stock portfolio in game [g]. *)
val start_balance: t -> float

(** [to_ticker s] is the ticker representation of string [s]. *)
val to_ticker : string -> ticker

(** [to_string tkr] is the string representation of ticker [tkr]. *)
val to_string : ticker -> string

(** [to_string_name nm] is the string representation of name [nm]. *)
val to_string_name : name -> string

(** [prnt_space n] prints [n] number of spaces. *)
val prnt_space : int -> int

(** [valid_ticker g tkr] checks if ticker [tkr] is a valid ticker 
    for a company in game [g]. *)
val valid_ticker : t -> ticker -> bool

(** [port_list g acc] is the list of tickers [acc] in the player's portfolio
    in game [g] *)
val port_list : t -> ticker list -> ticker list

(** [get_portfolio g] is the list of tickers and amounts in the players' 
    portfolio in game [g] *)
val get_portfolio : t -> (ticker * float) list

(** [portfolio_ticker g tkr] checks if ticker [tkr] is a valid ticker
    in the player's portfolio in game [g] *)
val portfolio_ticker : t -> ticker -> bool

(** [portfolio_value g p] is the value [acc] of the shares in portfolio
    [p] in game [g] *)
val portfolio_value : t -> (ticker * float) list -> float

(** [buy_stock g tkr n] adds [n] shares of the stock with ticker [tkr]  
    to the player's portfolio in game [g], and removes the corresponding
    value from the player's balance.
    Requirements: [tkr] is a valid ticker. [n] is the float representation
    of an integer greater than or equal to 1. *)
val buy_stock : t -> ticker -> float -> t

(** [sell_stock g tkr n] removes [n] shares of the stock with ticker [tkr]  
    to the player's portfolio in game [g], and adds the corresponding value
    to the player's balance.
    Requirements: [tkr] is a valid ticker. [n] is the float representation
    of an integer greater than or equal to 1. *)
val sell_stock : t -> ticker -> float -> t

(** [wait g n] let's the player wait [n] rounds in game [g].
    Requirements: [n] is an integer greater than or equal to 1. *)
val wait : t -> int -> t

(** [delta_price g tkr] is string representation of the change in share price 
    for a stock over a round in game [g]. *)
val delta_price : t -> ticker -> string

(** [delta_price_value g tkr] is float representation of the change in share 
    price for a stock over a round in game [g]. *)
val delta_price_value : t -> ticker -> float

(** [growth g tkr hist pd] is the association list of the percentage growth
    and monetary growth of a company with ticker [tkr] and price history [hist] 
    over a period [pd] in game [g]. *)
val growth : t -> ticker -> float list -> int -> (float * float)

(** [save g] converts game [g] to JSON and writes it to file "save.json". *)
val save : t -> unit

(** [load] loads the most recent save file "save.json"
    Requirements: "save.json" exists. *)
val load : unit -> t

(** [round g] is the current round that the player is on in game [g]. *)
val round : t -> int

(** [net_worth g] is the combined value of the player's cash balance and 
    stock portfolio in game [g]. *)
val net_worth : t -> float
