(** A [Company] is a representation of all companies' data.

    This module represent the state of all company share prices.
*)

open Difficulty

(** The type of company names. *)
type name = string

(** The type of company identifiers. *)
type ticker = string

(** Raised when an unknown company is encountered. *)
exception UnknownCompany of ticker

(** The abstract type of values representing all companies and their stocks. *)
type t

(** The type of companies. *)
type company = {
  name: name;
  ticker : ticker;
  price : float;
  volatility : float;
  past_prices : float list;
  trend : float
}

(** [read_json j] is the set of companies that [j] represents.
    Requires: [j] is a valid JSON representation of companies. *)
val read_json : Yojson.Basic.t -> t

(** [name_lst c_lst acc] is the association list [acc] of company names and 
    company tickers in company list [c_lst]. *)
val name_list : t -> (name * ticker) list -> (name * ticker) list

(** [name c tkr] is the name of a company in [c] with identifier [tkr]. *)
val name : t -> ticker -> name

(** [price c tkr] is the stock price of a company in [c] with identifier 
    [tkr]. *)
val price : t -> ticker -> float

(** [volatility c tkr] is the volatility of a company in [c] with identifier 
    [tkr]. *)
val volatility : t -> ticker -> float

(** [past_prices c tkr] is the list of past prices for a company in [c] with 
    identifier [tkr]. *)
val past_prices : t -> ticker -> float list

val truncatef : float -> float

(** [update c] gives all of the companies in [c] updated for the next round. *)
val update : t -> difficulty -> t

(** [to_list c] gives companies [c] as a company list. *)
val to_list : t -> company list

(** [c_to_string c_lst c_str] returns [c_str] the JSON string representation 
    of the company list [c_lst]. *)
val c_to_string : t -> string -> string

(** [read_saved_json j] is the set of companies that [j] represents.
    Requires: [j] is a valid JSON representation of companies. *)
val read_saved_json : Yojson.Basic.t -> t