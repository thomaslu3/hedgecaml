open Game
open Companies
open Command
open Difficulty
open Graph

(* let prnt_lst (t : Game.t) = 
   print_endline (string_of_float (Game.current_balance t)) *)
(** Raised when an invalid argument is entered. *)
exception Invalid_argument

(** [repl g] starts the REPL for game [g]. *)
let rec repl (g : Game.t) : unit =
  (* let curr_bal = current_balance g in *)
  (* let pfolio = get_portfolio g in *)
  (* let pfolio_value = portfolio_value g pfolio in
     let start_bal = start_balance g in *)
  let diff = difficulty g in
  let worth = net_worth g in
  print_endline "";
  let comm = (
    if ((diff = "easy" || diff = "medium" || diff = "hard") && 
        (Game.round g > 100)) 
    then
      ((if ((diff = "easy" && worth >= 20000.0) || 
            (diff = "medium" && worth >= 30000.0) || 
            (diff = "hard" && worth >= 50000.0)) 
        then
          let print_win = "Wow, you have managed to reach the target value! Congratulations on making in big on the stock market. You win!!"
          in
          print_endline print_win;
        else
          let print_lost = "Oh no, it seems as though your investments have fallen through. You weren't able to achieve the target value.\nBetter luck next time."
          in
          print_endline print_lost;);
       parse "quit")
    else
      (print_string "> ";
       let string : string = read_line() in parse string)) in
  try match comm with
    | Help ->
      print_endline ("HedgeCaml is an interactive stock market game. Below are the commands that will allow you to trade on the market.\n");
      print_endline ("[Companies] - Displays the companies whose shares are publicly traded on the market as well as their tickers.");
      print_endline ("[Buy] <Ticker> <Amount> - Buys the number of shares given by \"Amount\" of the company registered as the listing \"Ticker\".");
      print_endline ("[Sell] <Ticker> <Amount> - Sells the number of shares given by \"Amount\" of the company registered as the listing \"Ticker\".");
      print_endline ("[Balance] - Displays your current balance.");
      print_endline ("[Price] <Ticker> - Displays the current price listing for a share of the company listed as \"Ticker\".");
      print_endline ("[Portfolio] - Displays your portfolio, which includes the companies whose shares you own as well as how many.");
      print_endline ("[Wait] <Turns (Optional)> - Wait for the number of turns given by \"Turns\" and see how the market changes.
              If \"Turns\" isn't specified then wait for a single turn.");
      print_endline ("[Round] - Displays current round number.");
      print_endline ("[Save] - Saves the game.");
      print_endline ("[Quit] - Exits the game.");
      repl g
    | Companies -> 
      let comp_list = comp_list g [] in
      ANSITerminal.(print_string [Bold; Underlined; black] 
                      "                                                                          \n");
      ANSITerminal.(print_string [Bold; Underlined; cyan] 
                      "  Company:          Ticker:          Price:          Marginal Change:     \n");
      let rec name_prnt list =
        match list with
        | [] ->  
          ANSITerminal.(print_string [Bold; Underlined; black] 
                          "                                                                          \n");
          repl g
        | h::t ->
          let print1 = "| " ^ fst h in
          ANSITerminal.(print_string [Bold; Underlined; default] print1);
          let len = String.length print1 in
          let loop = 20 - len in
          ignore (prnt_space loop);
          let print2 = snd h in
          ANSITerminal.(print_string [Bold; Underlined; default] print2);
          let len' = String.length print2 in
          let loop' = 17 - len' in
          ignore (prnt_space loop');
          let share_price = (current_share_price g (to_ticker (snd h))) in
          let print3 = "$" ^
                       Float.to_string share_price in
          ANSITerminal.(print_string [Bold; Underlined; default] print3);
          let len'' = String.length print3 in
          let loop'' = 16 - len'' in
          ignore (prnt_space (loop'' + 4));
          let print4 = delta_price g (to_ticker (snd h)) in
          ANSITerminal.(print_string [Bold; Underlined; default] print4);
          let len_fin = String.length print4 in
          let loop_fin = 15 - len_fin in
          ignore (prnt_space (loop_fin));
          let del = delta_price_value g (to_ticker (snd h)) in
          (if del < 0.
           then begin
             ANSITerminal.(print_string [Bold; red] "-");
           end
           else if del > 0.
           then begin
             ANSITerminal.(print_string [Bold; green] "+");
           end
           else
             ANSITerminal.(print_string [Bold; white] "/"));
          ANSITerminal.(print_string [Bold; Underlined; default] "|");
          (if share_price = 0.
           then
             ANSITerminal.(print_string [Bold; red] " BANKRUPT\n")
           else
             ANSITerminal.(print_string [Bold; default] "\n"));
          name_prnt t
      in
      name_prnt comp_list
    | Buy p -> 
      let instr = to_list p in
      begin 
        match instr with
        | [] -> raise Invalid_argument
        | h::t::[] -> let h = String.uppercase_ascii h in
          let g' = (Game.buy_stock g (Game.to_ticker h) (Float.of_string t))
          in
          ANSITerminal.(print_string [Bold; Underlined; black] 
                          "                                                          \n");
          let print1a = "Balance:" in
          let print1b = " $" ^ 
                        (string_of_float (Game.current_balance g')) ^ "\n" in
          ANSITerminal.(print_string [Bold; Underlined; default] print1a);
          ANSITerminal.(print_string [Bold; default] print1b);
          let pfolio = get_portfolio g' in
          let print2a = "Portfolio Value:" in
          let print2b = " $" ^
                        (string_of_float (portfolio_value g pfolio)) in
          ANSITerminal.(print_string [Bold; Underlined; default] print2a);
          ANSITerminal.(print_string [Bold; default] print2b);
          let diff = (portfolio_value g pfolio) -. 
                     (portfolio_value g (get_portfolio g)) in
          let unit_price = diff /. (Float.of_string t) in
          if diff < 0.
          then begin
            let print3 = " (-$" ^ string_of_float (Float.abs diff) ^
                         " @ $" ^  string_of_float (Float.abs unit_price) ^ 
                         " per share)\n" in
            ANSITerminal.(print_string [Bold; red] print3);
            ANSITerminal.(print_string [Bold; Underlined; black] 
                            "                                                          \n");
            repl g'
          end
          else if diff > 0.
          then begin
            let print4 = " (+$" ^ string_of_float diff ^
                         " @ $" ^  string_of_float unit_price ^ 
                         " per share)\n" in
            ANSITerminal.(print_string [Bold; green] print4);
            ANSITerminal.(print_string [Bold; Underlined; black] 
                            "                                                          \n");
            repl g'
          end
          else
            let print5 = " (/$0.00" ^
                         " @ $0.00 per share)\n" in
            ANSITerminal.(print_string [Bold; white] print5);
            ANSITerminal.(print_string [Bold; Underlined; black] 
                            "                                                          \n");
            repl g'
        | _ -> raise Invalid_argument
      end
    | Sell p -> 
      let instr = to_list p in
      begin 
        match instr with
        | [] -> raise Invalid_argument
        | h::t::[] -> let h = String.uppercase_ascii h in
          let g' = (Game.sell_stock g (Game.to_ticker h) (Float.of_string t))
          in
          ANSITerminal.(print_string [Bold; Underlined; black] 
                          "                                                          \n");
          let print1a = "Balance:" in
          let print1b = " $" ^ 
                        (string_of_float (Game.current_balance g')) ^ "\n" in  
          ANSITerminal.(print_string [Bold; Underlined; default] print1a);
          ANSITerminal.(print_string [Bold; default] print1b);
          let pfolio = get_portfolio g' in
          let print2a = "Portfolio Value:" in
          let print2b = " $" ^
                        (string_of_float (portfolio_value g pfolio)) in
          ANSITerminal.(print_string [Bold; Underlined; default] print2a);
          ANSITerminal.(print_string [Bold; default] print2b);
          let diff = (portfolio_value g pfolio) -. 
                     (portfolio_value g (get_portfolio g)) in
          let unit_price = diff /. (Float.of_string t) in
          if diff < 0.
          then begin
            let print3 = " (-$" ^ string_of_float (Float.abs diff) ^
                         " @ $" ^  string_of_float (Float.abs unit_price) ^ 
                         " per share)\n" in
            ANSITerminal.(print_string [Bold; red] print3);
            ANSITerminal.(print_string [Bold; Underlined; black] 
                            "                                                          \n");
            repl g'
          end
          else if diff > 0.
          then begin
            let print4 = " (+$" ^ string_of_float diff ^
                         " @ $" ^  string_of_float unit_price ^ 
                         " per share)\n" in
            ANSITerminal.(print_string [Bold; green] print4);
            ANSITerminal.(print_string [Bold; Underlined; black] 
                            "                                                          \n");
            repl g'
          end
          else
            let print5 = " (/$0.00" ^
                         " @ $0.00 per share)\n" in
            ANSITerminal.(print_string [Bold; white] print5);
            ANSITerminal.(print_string [Bold; Underlined; black] 
                            "                                                          \n");
            repl g'
        | _ -> raise Invalid_argument
      end
    | Balance -> 
      ANSITerminal.(print_string [Bold; Underlined; black] 
                      "                            \n");
      let curr_balance = Game.current_balance g in
      let print1a = "Current Balance:" in
      let print1b = " $" ^ 
                    (string_of_float curr_balance) ^ "\n" in
      ANSITerminal.(print_string [Bold; Underlined; default] print1a);
      ANSITerminal.(print_string [Bold; default] print1b);
      let print2 = "Profits to Date:"
      in
      ANSITerminal.(print_string [Bold; Underlined; default] print2);
      let profits = curr_balance -. (start_balance g)
      in
      if profits < 0.
      then begin
        let print3 = " -$" ^ string_of_float (Float.abs profits) ^ "\n" in
        ANSITerminal.(print_string [Bold; red] print3);
        ANSITerminal.(print_string [Bold; Underlined; black] 
                        "                            \n");
        repl g
      end
      else if profits > 0.
      then begin
        let print4 = " +$" ^ string_of_float profits ^ "\n" in
        ANSITerminal.(print_string [Bold; green] print4);
        ANSITerminal.(print_string [Bold; Underlined; black] 
                        "                            \n");
        repl g
      end
      else
        let print5 = " /$0.00\n" in
        ANSITerminal.(print_string [Bold; white] print5);
        ANSITerminal.(print_string [Bold; Underlined; black] 
                        "                            \n");
        repl g
    | Price p -> 
      begin
        ANSITerminal.(print_string [Bold; Underlined; black] 
                        "                                                                                                                      \n");
        match p with
        | [] -> raise Error
        | h::t -> 
          let h = String.uppercase_ascii h in
          let curr_share_price = current_share_price g (Game.to_ticker h) in
          let print1a = "Current Price:" in
          let print1b = " $" ^ 
                        (string_of_float curr_share_price ^
                         "\n") in
          ANSITerminal.(print_string [Bold; Underlined; default] print1a);
          ANSITerminal.(print_string [Bold; default] print1b);
          let last_price_list = price_history g (to_ticker h) in

          let print2a = "Short Term Growth:" in
          ANSITerminal.(print_string [Bold; Underlined; default] print2a);
          let growth2 = growth g (to_ticker h) (List.rev last_price_list) 2 in
          (if (fst growth2) = Float.neg_infinity
           then
             let print2b = " - Time Scale Too Small -\n" in
             ANSITerminal.(print_string [default] print2b);
           else if (fst growth2) < 0.
           then begin
             let print2c = " -" ^ string_of_float (Float.abs (fst growth2)) ^
                           "% ($" ^ string_of_float (snd growth2) ^ ")\n"
             in
             ANSITerminal.(print_string [Bold; red] print2c);
           end
           else if (fst growth2) > 0.
           then begin
             let print2d = " +" ^ string_of_float (fst growth2) ^ "% ($" ^
                           string_of_float (snd growth2) ^ ")\n" in
             ANSITerminal.(print_string [Bold; green] print2d);
           end
           else
             let print2e = " 0.00%\n" in
             ANSITerminal.(print_string [Bold; white] print2e););

          let print3a = "Long Term Growth:" in
          ANSITerminal.(print_string [Bold; Underlined; default] print3a);
          let growth10 = growth g (to_ticker h) (List.rev last_price_list) 9 in
          (if (fst growth10) = Float.neg_infinity
           then
             let print3b = " - Time Scale Too Small -\n" in
             ANSITerminal.(print_string [default] print3b);
           else if (fst growth10) < 0.
           then begin
             let print3c = " -" ^ string_of_float (Float.abs (fst growth10)) ^
                           "% (-$" ^ string_of_float (Float.abs (snd growth10))
                           ^ ")\n"
             in
             ANSITerminal.(print_string [Bold; red] print3c);
           end
           else if (fst growth10) > 0.
           then begin
             let print3d = " +" ^ string_of_float (fst growth10) ^ "% ($" ^
                           string_of_float (snd growth10) ^ ")\n" in
             ANSITerminal.(print_string [Bold; green] print3d);
           end
           else
             let print3e = " 0.00%\n" in
             ANSITerminal.(print_string [Bold; white] print3e););

          print_endline "";
          let print4 = "Price History (Oldest -> Recent):\n" in
          ANSITerminal.(print_string [Bold; Underlined; default] print4);
          let rec print_history lst bool_frst =
            match lst with
            | [] -> 
              if bool_frst
              then begin
                print_endline ("- No Price History -");
                (* ANSITerminal.(print_string [Bold; Underlined; black] 
                                "                                                                                                   \n"); *)
                (* repl g *)
              end
              else begin
                print_endline ("");
                (* ANSITerminal.(print_string [Bold; Underlined; black] 
                                "                                                                                                   \n"); *)
                (* repl g *)
              end
            | h::t -> 
              if h = -1.
              then begin
                print_history t true
              end
              else begin
                if bool_frst
                then begin
                  ANSITerminal.(print_string [Bold; default] 
                                  (Float.to_string h));
                  (print_history t false)
                end
                else begin
                  ANSITerminal.(print_string [Bold; default] 
                                  (" ; " ^ Float.to_string h));
                  (print_history t false)
                end
              end
          in
          print_history last_price_list true;


          let graph_lst = curr_share_price::(List.rev last_price_list) in
          let graph_t = init_graph (List.rev graph_lst) 10 5 in
          let graph_t' = print_axes graph_t in
          let graph_t'' = print_scale graph_t' in
          let graph_t''' = print_points graph_t'' in
          print_line graph_t''';
          print_endline "\n\n";

          ANSITerminal.(print_string [Bold; Underlined; black] 
                          "                                                                                                                      \n");
          repl g
      end
    | Portfolio ->
      ANSITerminal.(print_string [Bold; Underlined; black] 
                      "                                                                        \n");
      let pfolio = get_portfolio g in 
      let print1a = "Portfolio Value:" in
      let print1b = " $" ^ (string_of_float (portfolio_value g pfolio)) ^
                    "\n\n" in
      ANSITerminal.(print_string [Bold; Underlined; default] print1a);
      ANSITerminal.(print_string [Bold; default] print1b);
      ANSITerminal.(print_string [Bold; Underlined; default] "Portfolio\n");
      ANSITerminal.(print_string [Bold; Underlined; magenta] 
                      "  Company:          Ticker:          Shares Owned:          Value:     \n");
      begin 
        let rec reploop lst = 
          match lst with
          | [] ->  
            ANSITerminal.(print_string [Bold; Underlined; black] 
                            "                                                                        \n");
            repl g
          | h::t -> 
            let comp_name = comp_name g (fst h) |> to_string_name in
            let print1 = "| " ^ comp_name in
            ANSITerminal.(print_string [Bold; Underlined; default] print1);
            let len = String.length print1 in
            let loop = 20 - len in
            ignore (prnt_space loop);
            let print2 = fst h |> to_string in
            ANSITerminal.(print_string [Bold; Underlined; default] print2);
            let len' = String.length print2 in
            let loop' = 17 - len' in
            ignore (prnt_space (loop' + 5));
            let print3 = snd h |> Float.floor |> Float.to_int |> 
                         string_of_int in
            ANSITerminal.(print_string [Bold; Underlined; default] print3);
            let len'' = String.length print3 in
            let loop'' = 18 - len'' in
            ignore (prnt_space loop'');
            let print4 = "$" ^ ((snd h *. (current_share_price g (fst h))) |> 
                                string_of_float) in
            ANSITerminal.(print_string [Bold; Underlined; default] print4);
            let len_fin = String.length print4 in
            let loop_fin = 10 - len_fin in
            ignore (prnt_space (loop_fin));
            ANSITerminal.(print_string [Bold; Underlined; default] "|\n");
            reploop t
            (* print_endline (to_string (fst h) ^ ",    " ^ 
               Float.to_string (snd h)); loop t *)
        in
        reploop pfolio
      end
    | Wait p -> 
      begin
        try match p with
          | h::t -> 
            let num = int_of_string h in
            let g' = wait g num in
            repl g'
          | [] ->
            let num = 1 in
            let g' = wait g num in
            repl g'
        with
        | _ -> raise Error
      end
    | Save -> save g; 
      ANSITerminal.(print_string [Bold; default] "Game saved."); repl g
    | Round -> 
      ANSITerminal.(print_string [Bold; Underlined; black] 
                      "                            \n");
      let rnds = Game.round g in
      let print1a = "Current Round:" in
      let print1b = " " ^ (string_of_int rnds) ^ "\n" in
      ANSITerminal.(print_string [Bold; Underlined; default] print1a);
      ANSITerminal.(print_string [Bold; default] print1b);
      ANSITerminal.(print_string [Bold; Underlined; black] 
                      "                            \n");
      repl g
    | Quit ->
      print_endline "Thanks for playing!"
  with
  | ErrorParsing -> print_endline "This is an invalid command. Type [Help] to see a list of commands."; 
    repl g
  | EmptyCommand -> print_endline "This command is missing some arguments. Type [Help] to see a list of commands.";
    repl g
  | OverTrade -> print_endline "You do not have the required resouces to execute this command.";
    repl g
  | Invalid_ticker -> print_endline "This ticker is not associated with any publicly traded companies. Type [Companies] to see a list of tradable company shares.";
    repl g
  | UnknownCompany t -> print_endline "This ticker is not associated with any publicly traded companies. Type [Companies] to see a list of tradable company shares.";
    repl g
  | Invalid_argument -> print_endline "This argument for this command is invalid. Type [Help] to see a list of commands or [Companies] to see a list of companies.";
    repl g

(** [play_game f] starts a new game from file [f] with difficulty [diff]. *)
let play_game f diff = 
  let c_lst = 
    ANSITerminal.(print_string [Bold; green] "\nEnjoy the game! Type [Help] to see a list of commands.\n");
    (Yojson.Basic.from_file f |> Companies.read_json) in
  try 
    Game.init_state c_lst diff |> repl
  with
  | _ ->
    print_endline "This is an invalid command. Type [Help] to see a list of commands."; 
    Game.init_state c_lst diff |> repl

(** [new_main ()] prompts user for difficulty and starts game. 
    Raises: End_of_file if unable to read file. *)
let rec new_main () =
  ANSITerminal.(print_string [Bold; green]
                  "Enter one of the following game modes:\n      - Easy\n      - Medium\n      - Hard\n      - Sandbox\n      - Quit (If you want to play later)\n";);
  print_string  "> ";
  try match read_line () with
    | exception End_of_file -> ()
    | difficulty -> 
      if (String.lowercase_ascii difficulty = "quit")
      then print_endline "See you soon!"
      else play_game "companies.json" (parse_diff difficulty)
  with 
  | _ -> 
    ANSITerminal.(print_string [Bold; red] "Sorry, this is not a valid game mode\n");
    new_main ()

(** [main ()] prompts user if they want to resume a saved game and starts a game 
    from the most recently saved game if so.
    Raises: End_of_file if unable to read file. *)
let rec main () =
  ANSITerminal.(print_string [Bold; green]
                  "\n\nWelcome to HedgeCaml, a stock market game. For the best experience, make your terminal full screen!\n");

  ANSITerminal.(print_string [Bold; green]
                  "\n\nTo win the game, you must accumulate the following total net worth (given your difficulty level) in 100 rounds:\n      - Easy: $20,000\n      - Medium: $30,000\n      - Hard: $50,000\n");

  ANSITerminal.(print_string [Bold; green]
                  "\n\nWould you like to load a saved game? (Y/N)\n");
  try match read_line () with
    | exception End_of_file -> ()
    | yn -> 
      if (String.lowercase_ascii yn = "y" || String.lowercase_ascii yn = "yes")
      then begin 
        try let g = load () in 
          ANSITerminal.(print_string [Bold; green] 
                          "\nEnjoy the game! Type [Help] to see a list of commands.\n"); 
          repl g with
        | _ -> ANSITerminal.(print_string [Bold; red] 
                               "Sorry, no save file has been found."); main () 
      end
      else if 
        (String.lowercase_ascii yn = "n" || String.lowercase_ascii yn = "no") 
      then new_main ()
      else begin 
        ANSITerminal.(print_string [Bold; red] 
                        "Sorry, this is not a valid answer"); main () 
      end
  with 
  | _ -> 
    ANSITerminal.(print_string [Bold; red] "Sorry, this is not a valid answer");
    main ()

let () = main ()