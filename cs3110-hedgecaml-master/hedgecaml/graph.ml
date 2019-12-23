type datum = float

type t = {
  data: (datum * int) list;
  x_unit : int;
  y_unit : int;
  scale : float;
  scale_floor : float;
  offset_list : (int * int) list;
  origin : (int * int);
}

(** [float_to_datum fl] converts [fl] to type datum. *)
let float_to_datum (float : float) : datum = float

(** [datum_to_float dm] converts [dm] to type float. *)
let datum_to_float (datum : datum) : float = datum

(** [make_datum_lst f_lst] makes an ordered association list of values
    from [f_lst]. *)
let make_datum_lst float_lst =
  let rec lst_loop lst count acc =
    match lst with
    | [] -> List.rev acc
    | h::t ->
      let datum = float_to_datum h in
      let acc' = (datum, count) :: acc in
      lst_loop t (count + 1) acc'
  in
  lst_loop float_lst 1 []

(** [prnt_points d_lst] prints the point for datum list [d_lst].
    Requirements: [gr] is a valid representation of a graph. *)
let rec prnt_points data_lst =
  match data_lst with
  | [] -> print_endline ("");
  | h::t ->
    let value = datum_to_float (fst h) in
    let x_pnt = snd h in
    print_string ("(" ^ string_of_float value ^ ", " ^ 
                  string_of_int x_pnt ^ "), ");
    prnt_points t

(** [prnt_points_int d_lst] prints a list of (int * int tuples) [d_lst].
    Requirements: [gr] is a valid representation of a graph. *)
let rec prnt_points_int (data_lst : (int * int) list) =
  match data_lst with
  | [] -> print_endline ("");
  | h::t ->
    let value = (fst h) in
    let x_pnt = snd h in
    print_string ("(" ^ string_of_int value ^ ", " ^ 
                  string_of_int x_pnt ^ "), ");
    prnt_points_int t

let init_graph float_lst x_unit y_unit = begin
  let data_lst = make_datum_lst float_lst in
  {
    data = data_lst;
    x_unit = x_unit;
    y_unit = y_unit;
    scale = 0.0;
    scale_floor = 0.0;
    offset_list = [];
    origin = (0, 0);
  }
end

(** [update_origin t] updates the origin of graph [t].
    Requirements: [t] is a valid representation of a graph. *)
let update_origin t =
  {
    data = t.data;
    x_unit = t.x_unit;
    y_unit = t.y_unit;
    scale = t.scale;
    scale_floor = t.scale_floor;
    offset_list = t.offset_list;
    origin = ANSITerminal.pos_cursor();
  }


(** [update_scale t sc sc_flr] updates the scale of graph [t] with scale [sc]
    and [sc_flr] as the lowest value on the scale [sc].
    Requirements: [t] is a valid representation of a graph. *)
let update_scale t scale scale_flr =
  {
    data = t.data;
    x_unit = t.x_unit;
    y_unit = t.y_unit;
    scale = scale;
    scale_floor = scale_flr;
    offset_list = t.offset_list;
    origin = t.origin;
  }

(** [update_points t ofst_lst] updates the graph [t] with list of x and y 
    offsets [ofst_lst].
    Requirements: [t] is a valid representation of a graph. *)
let update_points t ofst_lst =
  {
    data = t.data;
    x_unit = t.x_unit;
    y_unit = t.y_unit;
    scale = t.scale;
    scale_floor = t.scale_floor;
    offset_list = ofst_lst;
    origin = t.origin;
  }

(** [repeat_print space strng count] prints string of spaces [space] and string
    [strng] [count] number of times. *)
let rec repeat_print space strng count =
  match count with
  | 1 -> 
    ANSITerminal.(print_string [Bold; default] space);
    ANSITerminal.(print_string [Bold; default] strng);
  | _ -> 
    ANSITerminal.(print_string [Bold; default] space);
    ANSITerminal.(print_string [Bold; default] strng);
    repeat_print space strng (count - 1)

(** [print_y_axis y sp str fst] is the y axis given number of y labels
    [y], sequence of spaces [sp], string [strng], and [fst] a bool that
    tracks whether or not this is the first iteration of print_y_axis. *)
let rec print_y_axis y_unit space strng fst =
  match y_unit with
  | 1 -> 
    repeat_print space strng 4;
    ANSITerminal.(print_string [Bold; default] space); 
    ANSITerminal.(print_string [Bold; Underlined; default] "| \n");
  | _ ->
    if fst 
    then begin
      repeat_print space strng 1;
      ANSITerminal.(print_string [Bold; default] space); 
      ANSITerminal.(print_string [Bold; Underlined; default] "| \n");
      print_y_axis (y_unit - 1) space strng false;
    end
    else begin
      repeat_print space strng 4;
      ANSITerminal.(print_string [Bold; default] space); 
      ANSITerminal.(print_string [Bold; Underlined; default] "| \n");
      print_y_axis (y_unit - 1) space strng false
    end

(** [print_x_axis x fs u e fst] is the x axis given number of x labels
    [x], first string [fs], unit string [u], and end string [e], and [fst] a 
    bool that tracks whether or not this is the first iteration of 
    print_y_axis. *)
let rec print_x_axis x_unit fst_str unit_str end_str fst =
  match x_unit with
  | 0 -> ANSITerminal.(print_string [Bold; default] end_str);
  | _ ->
    if fst 
    then begin
      ANSITerminal.(print_string [Bold; default] fst_str);
      print_x_axis (x_unit) fst_str unit_str end_str false;
    end
    else begin
      ANSITerminal.(print_string [Bold; default] unit_str);
      print_x_axis (x_unit - 1) fst_str unit_str end_str false;
    end

let print_axes t = begin
  print_endline "\n\n";
  let space = "       " in
  let strng = "|\n" in
  print_y_axis (t.y_unit) space strng true;
  let graph_y_end = "       |\n       /\n       \\\n       /\n       \\" in
  ANSITerminal.(print_string [Bold; default] graph_y_end);
  let graph_x_start = "_ __ __|" in
  let graph_x_unit = "__ __ __|" in
  let graph_x_end = "__" in
  print_x_axis (t.x_unit) graph_x_start graph_x_unit graph_x_end true;
  ANSITerminal.move_bol();
  update_origin t
end

(** [find_max d_lst ret] returns [ret] the max datum in list of datum 
    [d_lst]. *)
let rec find_max d_lst ret =
  match d_lst with
  | [] -> ret
  | h::t ->
    let value = datum_to_float (fst h) in
    if value > ret then find_max t value else find_max t ret

(** [scale_ceiling v] rounds value [v] up to the nearest tenth. *)
let scale_ceiling value =
  let int_val = Float.to_int value in
  let diff = int_val mod 10 in
  int_val + (10 - diff)

(** [find_min d_lst ret] returns [ret] the min datum in list of datum 
    [d_lst]. *)
let rec find_min d_lst ret =
  match d_lst with
  | [] -> ret
  | h::t ->
    let value = datum_to_float (fst h) in
    if value < ret && value <> -1. then find_min t value else find_min t ret

(** [scale_ceiling v] rounds value [v] down to the nearest tenth. *)
let scale_floor value =
  let int_val = Float.to_int value in
  let diff = int_val mod 10 in
  int_val - diff

(** [make_scale_lst max sc acc c] makes a list of scale values in [sc] 
    with max value [max] and number of values [count]. *)
let rec make_scale_lst max scale acc count =
  match count with
  | 0 -> acc
  | _ -> make_scale_lst (max -. scale) scale ((max, count)::acc) (count - 1)

(** [print_scale_helper s_lst] is the labels for the axes for scale list 
    [s_lst].
    Requirements: [s_lst] is a valid representation of a scale list. *)
let rec print_scale_helper scl_lst =
  match scl_lst with
  | [] -> ()
  | h::t ->
    let offset = -5 in
    ANSITerminal.move_cursor 0 offset;
    ANSITerminal.(print_string [Bold; default] (string_of_float (fst h)));
    ANSITerminal.move_bol();
    print_scale_helper t

let print_scale t = begin
  let data_lst = t.data in
  let max_val = find_max data_lst (-1.0) |> scale_ceiling |> float_of_int in
  let min_val = find_min data_lst (Float.max_float) |> scale_floor  |> 
                float_of_int in
  let scale = (max_val -. min_val) /. 4. in
  let scale_lst = make_scale_lst max_val scale [] 5 in
  ANSITerminal.set_cursor (fst t.origin) (snd t.origin);
  print_scale_helper scale_lst;
  ANSITerminal.set_cursor (fst t.origin) (snd t.origin);
  ANSITerminal.move_cursor 7 0;
  let t' = update_origin t in
  update_scale t' scale min_val;
end

(** [float_mod v u] is [v] - ([v] % [u]). *)
let float_mod value unit =
  let div = value /. unit in
  let flr_div = Float.round div in
  let diff = value -. (unit *. flr_div) in
  value -. diff

(** [calc_y_off v sc sc_flr] calculates the offset y based on
    value [v], scale [sc], and scale floor [scale_flr]. *)
let calc_y_off value scale scale_flr = begin
  let unit = scale /. 5.0 in
  let rnd_value = float_mod value unit in
  Float.to_int ((rnd_value -. scale_flr) /. unit) + 5
end

(** [print_points_helper d_lst og sc sc_flr corr acc] is the list of points 
    [acc] given list of datum [data_lst], origin value [og], scale [sc],
    scale floor [sc_flr], and correction [corr]. *)
let rec print_points_helper data_lst origin scale scale_flr correction acc =
  begin
    ANSITerminal.set_cursor (fst origin) (snd origin);
    match data_lst with
    | [] -> List.rev acc
    | h::t ->
      if (fst h) = -1. 
      then print_points_helper t origin scale scale_flr (correction + 1) acc
      else
        let x_offset = (((snd h) - correction) * 9) - 1 in
        let y_offset = -1 * (calc_y_off (fst h) scale scale_flr) in
        ANSITerminal.move_cursor x_offset y_offset;
        ANSITerminal.(print_string [Bold; cyan] "O");
        print_points_helper t origin scale scale_flr correction 
          ((x_offset, y_offset)::acc)
  end

let print_points t = begin
  let data_lst = t.data in
  let origin = t.origin in
  let scale = t.scale in
  let scale_flr = t.scale_floor in
  let ofst_lst = print_points_helper data_lst origin scale scale_flr 0 [] in
  ANSITerminal.set_cursor (fst t.origin) (snd t.origin);
  (* print_endline "";
     prnt_points_int ofst_lst; *)
  update_points t ofst_lst;
end

(** [print_segment x_off y_off mvt_lst c fst] prints the segment between
    two consecutive points with the first point given by [x_off] [y_off],  
    the relation between consecutive pixels given by [mvt_lst], the number of
    pixels given by [count], and the boolean indicating the first iteration
    [fst]. *)
let rec print_segment x_offset y_offset mvt_lst count first = begin
  match count with
  | 0 -> ()
  | x ->
    if first then begin
      ANSITerminal.move_cursor (x_offset + 1) (y_offset);
      let mvt = List.hd mvt_lst in
      ANSITerminal.move_cursor (fst mvt) (snd mvt);
      ANSITerminal.(print_string [Bold; red] "*");
      print_segment x_offset y_offset (List.tl mvt_lst) (count - 1) false
    end
    else begin
      let mvt = List.hd mvt_lst in
      ANSITerminal.move_cursor (fst mvt) (snd mvt);
      ANSITerminal.(print_string [Bold; red] "*");
      print_segment x_offset y_offset (List.tl mvt_lst) (count - 1) false
    end
end

(** [make_mvt_lst sl acc lst c] makes a list of movements [acc] 
    (relations between consecutive pixels) given slope [sl], list of 
    points [lst], number of pixels [c]. *)
let rec make_mvt_lst slope acc lst_pnt count = begin
  match count with
  | 9 -> List.rev acc
  | x ->
    let y_val = slope *. Float.of_int x in
    let y_rnd = Float.round y_val |> Float.to_int in
    let pnt_diff = y_rnd - lst_pnt in
    if pnt_diff < 0
    then
      make_mvt_lst slope ((0, (-1 * pnt_diff))::acc) 
        (y_rnd) (count + 1)
    else
      make_mvt_lst slope ((0, (-1 * pnt_diff))::acc) 
        (y_rnd) (count + 1)
end

(** [print_line_helper ofst_lst og] initializes the line segment between two
    consecutive points in [ofst_lst] on a graph with origin [og]. *)
let rec print_line_helper ofst_lst origin = begin
  match ofst_lst with
  | [] -> ()
  | h::[] -> ()
  | h::m::t ->
    ANSITerminal.set_cursor (fst origin) (snd origin);
    let x_offset = fst h in
    let y_offset = snd h in
    let y_range = ((snd m) - (snd h)) * (-1) in
    let slope = Float.of_int y_range /. 9. in
    let mvt_lst = make_mvt_lst slope []  0 1 in
    print_segment x_offset y_offset mvt_lst 8 true;
    print_line_helper (m::t) origin
end

let print_line t = begin
  let ofst_lst = t.offset_list in
  let origin = t.origin in
  print_line_helper ofst_lst origin;
  ANSITerminal.set_cursor (fst t.origin) (snd t.origin);
end