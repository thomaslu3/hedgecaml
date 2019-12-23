(** A [Graph] is a representation of a graph of company price history.

    This module represents the graph of a company's price history.
*)

(** Type of data to be graphed *)
type datum

(** Type of graph. *)
type t

(** [init_graph f_lst x y] is the initial state of a graph. In that state, 
    the graph has an ordered list of datum [f_lst] and x unit and y unit 
    [x] and [y] respectively. *)
val init_graph : float list -> int -> int -> t

(** [print_axes gr] prints the axes for graph [gr].
    Requirements: [gr] is a valid representation of a graph. *)
val print_axes : t -> t

(** [print_scale gr] prints the labels for the axes for graph [gr].
    Requirements: [gr] is a valid representation of a graph. *)
val print_scale : t -> t

(** [print_points gr] prints the point for graph [gr].
    Requirements: [gr] is a valid representation of a graph. *)
val print_points : t -> t

(** [print_line gr] prints the lines between points for graph [gr].
    Requirements: [gr] is a valid representation of a graph. *)
val print_line : t -> unit

(* val make_datum_lst : float list -> (datum * int) list

   val float_to_datum : float -> datum *)
