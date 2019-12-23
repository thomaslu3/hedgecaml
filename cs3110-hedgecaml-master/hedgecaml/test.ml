(** Our test suite accounts for all vital properties of the different companies
    and properties of the continuously updating game state. However, there are
    several aspects of the game that rely on semi-random functions, so these, as well
    as GUI functionality, were manually tested, not using OUnit. Test cases were
    mostly developed using a "black-box" approach.

    The test suite tests that the fields for different companies are read correctly 
    from the json file. Specifically we tested "name" and "volatility" as those
    are unchanging values for each distinct company (no test cases were made 
    specfically for "ticker" as we can verify this field works properly through our 
    other tests).

    We also test that the company's list of past prices updates properly using the
    game module. Rather than specifically creating test cases for the buy, sell,
    and wait functionalities, we just test that the player's balance, portfolio and
    game state updates properly after executing those functions a number of times.
    As previously mentioned, trend heavily relies on the use of randomly generated
    numbers (as well as game difficulty level and the recent changes seen in a 
    company's stock price), so this was not tested using OUnit. This covers all of
    the vital aspects of how our game state updates, making it easy to test other
    functionality that is more random and arbitrary using manual testing by playing
    our game.

    Finally we tested that commands are read properly by our system using OUnit.
    An expansive list of test cases were used to account for many different possible
    ways that the player could enter a command that should be read properly.
*)

open OUnit2
open Companies
open Game
open Command

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(* These tests demonstrate how to use [cmp_set_like_lists] and 
   [pp_list] to get helpful output from OUnit. *)
let cmp_demo = 
  [
    "order is irrelevant" >:: (fun _ -> 
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          ["foo"; "bar"] ["bar"; "foo"]);
    (* Uncomment this test to see what happens when a test case fails.
       "duplicates not allowed" >:: (fun _ -> 
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          ["foo"; "foo"] ["foo"]);
    *)
  ]

(* You are welcome to add strings containing JSON here, and use them as the
   basis for unit tests.  Or you can add .json files in this directory and
   use them, too.  Any .json files in this directory will be included
   by [make zip] as part of your CMS submission. *)

(** [name_test name c tkr expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [name c tkr]. *)
let name_test 
    (name : string) 
    (c : Companies.t)
    (tkr : Companies.ticker) 
    (expected_output : Companies.name) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Companies.name c tkr))

(** [price_test name c tkr expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [price c tkr]. *)
let price_test 
    (name : string) 
    (c : Companies.t)
    (tkr : Companies.ticker) 
    (expected_output : float) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Companies.price c tkr))

(** [volatility_test name c tkr expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [volatility c tkr]. *)
let volatility_test 
    (name : string) 
    (c : Companies.t)
    (tkr : Companies.ticker) 
    (expected_output : float) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Companies.volatility c tkr))

(** [past_history_test name g tkr expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [past_prices g tkr]. *)
let price_history_test 
    (name : string) 
    (g : Game.t)
    (tkr : Game.ticker) 
    (expected_output : float list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Game.price_history g tkr))

(** [share_price_list_test name g expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [share_price_list g]. *)
let share_price_list_test 
    (name : string) 
    (g : Game.t)
    (expected_output : float list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Game.share_price_list g))

(** [balance_test name g expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [current_balance g]. *)
let balance_test 
    (name : string) 
    (g : Game.t)
    (expected_output : float) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Game.current_balance g))

(** [portfolio_test name g expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [get_portfolio g]. *)
let portfolio_test 
    (name : string) 
    (g : Game.t)
    (expected_output : (Game.ticker*float) list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Game.get_portfolio g))

(** [portfolio_val_test name g expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [get_portfolio g]. *)
let portfolio_val_test 
    (name : string) 
    (g : Game.t)
    (p : (Game.ticker*float) list)
    (expected_output : float) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Game.portfolio_value g p))

(** [parse_test name s expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [get_portfolio g]. *)
let parse_test 
    (name : string) 
    (s : string)
    (expected_output : Command.command) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Command.parse s))

let g_tkr = (Game.to_ticker "GOOGL")
let a_tkr = (Game.to_ticker "AAPL")
let f_tkr = (Game.to_ticker "FB")
let am_tkr = (Game.to_ticker "AMZN")
let n_tkr = (Game.to_ticker "NFLX")
let m_tkr = (Game.to_ticker "MSFT")
let u_tkr = (Game.to_ticker "UBER")
let l_tkr = (Game.to_ticker "LYFT")
let w_tkr = (Game.to_ticker "WORK")

let comps = Companies.read_json (Yojson.Basic.from_file "companies.json")

let game = Game.init_state comps Medium
let init_balance = Game.start_balance game
let gprice = Game.current_share_price game g_tkr
let aprice = Game.current_share_price game a_tkr

let game2 = Game.buy_stock game g_tkr 1.
let gprice2 = Game.current_share_price game2 g_tkr
let aprice2 = Game.current_share_price game2 a_tkr
let fprice2 = Game.current_share_price game2 f_tkr
let amprice2 = Game.current_share_price game2 am_tkr
let nprice2 = Game.current_share_price game2 n_tkr
let mprice2 = Game.current_share_price game2 m_tkr
let uprice2 = Game.current_share_price game2 u_tkr
let lprice2 = Game.current_share_price game2 l_tkr
let wprice2 = Game.current_share_price game2 w_tkr

let game3 = Game.buy_stock game2 a_tkr 3.
let gprice3 = Game.current_share_price game3 g_tkr
let aprice3 = Game.current_share_price game3 a_tkr

let game4 = Game.sell_stock game3 a_tkr 2.
let gprice4 = Game.current_share_price game4 g_tkr
let aprice4 = Game.current_share_price game4 a_tkr

let game5 = Game.sell_stock game4 g_tkr 1.
let gprice5 = Game.current_share_price game5 g_tkr
let aprice5 = Game.current_share_price game5 a_tkr

let game6 = Game.wait game5 1
let gprice6 = Game.current_share_price game6 g_tkr
let aprice6 = Game.current_share_price game6 a_tkr

let game7 = Game.wait game6 1
let gprice7 = Game.current_share_price game7 g_tkr
let aprice7 = Game.current_share_price game7 a_tkr

let game8 = Game.wait game7 1
let gprice8 = Game.current_share_price game8 g_tkr
let aprice8 = Game.current_share_price game8 a_tkr

let game9 = Game.wait game8 1
let gprice9 = Game.current_share_price game9 g_tkr
let aprice9 = Game.current_share_price game9 a_tkr

let game10 = Game.wait game9 1
let gprice10 = Game.current_share_price game10 g_tkr
let aprice10 = Game.current_share_price game10 a_tkr

let game11 = Game.wait game10 1
let gprice11 = Game.current_share_price game11 g_tkr
let aprice11 = Game.current_share_price game11 a_tkr

let game12 = Game.wait game11 1
let gprice12 = Game.current_share_price game12 g_tkr
let aprice12 = Game.current_share_price game12 a_tkr

let companies_tests =
  [
    name_test "" comps "FB" "Facebook";
    name_test "" comps "AAPL" "Apple";
    name_test "" comps "AMZN" "Amazon";
    name_test "" comps "NFLX" "Netflix";
    name_test "" comps "GOOGL" "Alphabet";
    name_test "" comps "MSFT" "Microsoft";
    name_test "" comps "UBER" "Uber";
    name_test "" comps "LYFT" "Lyft";
    name_test "" comps "WORK" "Slack"; 

    price_test "" comps "FB" 193.62;
    price_test "" comps "AAPL" 255.82;
    price_test "" comps "AMZN" 1791.44;
    price_test "" comps "NFLX" 286.81;
    price_test "" comps "GOOGL" 1272.25;
    price_test "" comps "MSFT" 143.72;
    price_test "" comps "UBER" 31.37;
    price_test "" comps "LYFT" 42.98;
    price_test "" comps "WORK" 21.94; 

    volatility_test "" comps "FB" 0.66;
    volatility_test "" comps "AAPL" 0.7;
    volatility_test "" comps "AMZN" 0.2;
    volatility_test "" comps "NFLX" 0.85;
    volatility_test "" comps "GOOGL" 0.32;
    volatility_test "" comps "MSFT" 0.3;
    volatility_test "" comps "UBER" 0.73;
    volatility_test "" comps "LYFT" 0.65;
    volatility_test "" comps "WORK" 0.69;
  ]

let game_tests =
  [
    price_history_test "" game g_tkr [-1.0;-1.0;-1.0;-1.0;-1.0;-1.0;-1.0;-1.0;-1.0;-1.0];
    price_history_test "" game a_tkr [-1.0;-1.0;-1.0;-1.0;-1.0;-1.0;-1.0;-1.0;-1.0;-1.0];
    price_history_test "" game5 g_tkr [-1.0; -1.0; -1.0; -1.0; -1.0; -1.0; gprice; gprice2; gprice3; gprice4];
    price_history_test "" game5 a_tkr [-1.0; -1.0; -1.0; -1.0; -1.0; -1.0; aprice; aprice2; aprice3; aprice4];
    price_history_test "test that only 10 most recent prices are in prices history"
      game12 g_tkr [gprice2; gprice3; gprice4; gprice5; gprice6; gprice7;
                    gprice8; gprice9; gprice10; gprice11];
    price_history_test "test that only 10 most recent prices are in prices history"
      game12 a_tkr [aprice2; aprice3; aprice4; aprice5; aprice6; aprice7;
                    aprice8; aprice9; aprice10; aprice11];

    share_price_list_test "" game [193.62; 255.82; 1791.44; 286.81; 1272.25; 143.72; 31.37; 42.98; 21.94];
    share_price_list_test "" game2 [fprice2;aprice2;amprice2;nprice2;gprice2;mprice2;uprice2;lprice2;wprice2];

    balance_test "balance after buying 1 GOOGL stock" game2 (init_balance-.gprice);
    balance_test "balance after buying 3 additional AAPL stocks" game3 (init_balance-.gprice-.(3.0*.aprice2));
    balance_test "balance after selling 2 AAPL stocks" game4 (init_balance-.gprice-.(3.0*.aprice2)+.(2.*.aprice3));
    balance_test "balance after selling 1 GOOGL stock" game5 (init_balance-.gprice-.(3.0*.aprice2)+.(2.*.aprice3)+.gprice4);

    portfolio_test "portfolio after buying 1 GOOGL stock" game2 [(g_tkr,1.)];
    portfolio_test "portfolio after buying 3 additional AAPL stocks" game3 [(g_tkr,1.); (a_tkr, 3.)];
    portfolio_test "portfolio after selling 2 AAPL stock" game4 [(g_tkr,1.); (a_tkr, 1.)];
    portfolio_test "portfolio after selling 1 GOOGL stock" game5 [(a_tkr,1.)];

    portfolio_val_test "portfolio value after buying 1 GOOGL stock" game2 (Game.get_portfolio game2) gprice2;
    portfolio_val_test "portfolio value after buying 3 AAPL stock" game3 (Game.get_portfolio game3) (gprice3+.(3.0*.aprice3)); 
    portfolio_val_test "portfolio value after selling 2 AAPL stock" game4 (Game.get_portfolio game4) (gprice4+.aprice4); 
    portfolio_val_test "portfolio value after selling 1 GOOGL stock" game5 (Game.get_portfolio game5) (aprice5); 
  ]

let command_tests = 
  [
    parse_test "test companies command" "companies" Companies;
    parse_test "test companies command" "companies   " Companies;
    parse_test "test companies command" "Companies" Companies;
    parse_test "test companies command" "COMPANIES" Companies;

    parse_test "test buy command" "buy amzn 3" (Buy ["amzn"; "3"]);
    parse_test "test buy command" "BUY    aapl    5" (Buy ["aapl"; "5"]);
    parse_test "test buy command" "bUy uber    2  " (Buy ["uber"; "2"]);

    parse_test "test sell command" "sell amzn 3" (Sell ["amzn"; "3"]);
    parse_test "test sell command" "SELL    aapl    5" (Sell ["aapl"; "5"]);
    parse_test "test sell command" "sElL uber    2  " (Sell ["uber"; "2"]);

    parse_test "test balance command" "balance" Balance;
    parse_test "test balance command" "balance   " Balance;
    parse_test "test balance command" "baLaNce" Balance;
    parse_test "test balance command" "BALANCE" Balance;

    parse_test "test price command" "price amzn" (Price ["amzn"]);
    parse_test "test price command" "price    aapl    " (Price ["aapl"]);
    parse_test "test price command" "PRICE      lyft   " (Price ["lyft"]);

    parse_test "test portfolio command" "portfolio" Portfolio;
    parse_test "test portfolio command" "PORTFOLIO   " Portfolio;

    parse_test "test wait command" "wait" (Wait []);
    parse_test "test wait command" "wait 3" (Wait ["3"]);
    parse_test "test wait command" "WAIT   26" (Wait ["26"]);

    parse_test "test save command" "save" Save;
    parse_test "test save command" "SAVE   " Save;

    parse_test "test round command" "round" Round;
    parse_test "test round command" "ROUND   " Round;

    parse_test "test quit command" "quit" Quit;
    parse_test "test quit command" "QUIT   " Quit;

    parse_test "test help command" "help" Help;
    parse_test "test help command" "HELP   " Help;
  ]
let suite =
  "test suite for A2"  >::: List.flatten [
    companies_tests;
    game_tests;
    command_tests;
  ]

let _ = run_test_tt_main suite