open! Core
open! Async
open! Game_strategies_common_lib

module Exercises = struct
  (* Here are some functions which know how to create a couple different
     kinds of games *)
  let empty_game = Game.empty Game.Game_kind.Tic_tac_toe
  let empty_omok_game = Game.empty Game.Game_kind.Omok

  let place_piece (game : Game.t) ~piece ~position : Game.t =
    let board = Map.set game.board ~key:position ~data:piece in
    { game with board }
  ;;

  let win_for_x =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
  ;;

  let non_win =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
  ;;

  let minmax_test =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 1 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
  ;;

  let omok_game =
    let open Game in
    empty_omok_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 7; column = 7 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 8; column = 8 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 8; column = 7 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 4; column = 4 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
  ;;

  let print_game (game : Game.t) =
    (* ignore game; *)
    let open Game in
    let board_length = Game_kind.board_length game.game_kind in
    let board_in_2d =
      List.init board_length ~f:(fun row ->
        List.init board_length ~f:(fun col ->
          let key = { Position.row; column = col } in
          match Map.find game.board key with
          | None -> " "
          | Some piece -> Piece.to_string piece))
    in
    let row_list =
      List.map board_in_2d ~f:(fun row -> String.concat row ~sep:" | ")
    in
    let final_str =
      String.concat
        row_list
        ~sep:"\n--------------------------------------------------------\n"
    in
    print_endline final_str
  ;;

  let print_scores (positions_with_score : (Game.Position.t * int) list) =
    let open Game in
    let position_to_score_map = Map.of_alist_exn (module Position) positions_with_score in
    let board_in_2d =
      List.init (15) ~f:(fun row ->
        List.init (15) ~f:(fun col ->
          let key = { Position.row; column = col } in
          match Map.find position_to_score_map key with
          | None -> " "
          | Some piece -> Int.to_string piece))
    in
    let row_list =
      List.map board_in_2d ~f:(fun row -> String.concat row ~sep:" | ")
    in
    let final_str =
      String.concat
        row_list
        ~sep:"\n--------------------------------------------------------\n"
    in
    print_endline final_str
  ;;

  let%expect_test "print_win_for_x" =
    print_game win_for_x;
    [%expect
      {|
      X | O | X
      ---------
      O | O | X
      ---------
      O | X | X
      |}];
    return ()
  ;;

  let%expect_test "print_non_win" =
    print_game non_win;
    [%expect
      {|
      X |   |
      ---------
      O |   |
      ---------
      O |   | X
      |}];
    return ()
  ;;

  (* Exercise 1 *)
  let available_moves (game : Game.t) : Game.Position.t list =
    let open Game in
    let len = Game_kind.board_length game.game_kind in
    let row = List.init len ~f:Fn.id in
    List.cartesian_product row row
    |> List.map ~f:(fun (row, column) -> { Position.row; column })
    |> List.filter_map ~f:(fun pos ->
      match Map.find game.board pos with None -> Some pos | _ -> None)
  ;;

  let all_pos_in_bounds game =
    let open Game in
    let all_positions = Map.keys game.board in
    List.for_all all_positions ~f:(fun pos ->
      Position.in_bounds pos ~game_kind:game.game_kind)
  ;;

  let get_neighbors (pos : Game.Position.t) game =
    let open Game in
    let win_len = Game_kind.board_length game.game_kind in
    (* TODO: abstract this away *)
    let dirs_above =
      List.init (win_len - 1) ~f:(fun num ->
        let num = num + 1 in
        num, 0)
    in
    let dirs_below =
      List.init (win_len - 1) ~f:(fun num ->
        let num = num + 1 in
        -1 * num, 0)
    in
    let dirs_right =
      List.init (win_len - 1) ~f:(fun num ->
        let num = num + 1 in
        0, num)
    in
    let dirs_left =
      List.init (win_len - 1) ~f:(fun num ->
        let num = num + 1 in
        0, -1 * num)
    in
    let dirs_up_right =
      List.init (win_len - 1) ~f:(fun num ->
        let num = num + 1 in
        num, num)
    in
    let dirs_up_left =
      List.init (win_len - 1) ~f:(fun num ->
        let num = num + 1 in
        num, -1 * num)
    in
    let dirs_down_right =
      List.init (win_len - 1) ~f:(fun num ->
        let num = num + 1 in
        -1 * num, num)
    in
    let dirs_down_left =
      List.init (win_len - 1) ~f:(fun num ->
        let num = num + 1 in
        -1 * num, -1 * num)
    in
    let list_of_dirs =
      [ dirs_above
      ; dirs_below
      ; dirs_right
      ; dirs_left
      ; dirs_up_right
      ; dirs_up_left
      ; dirs_down_right
      ; dirs_down_left
      ]
    in
    List.map list_of_dirs ~f:(fun dirs ->
      List.filter_map dirs ~f:(fun (r_dir, c_dir) ->
        let neighbor =
          { Position.row = pos.row + r_dir
          ; Position.column = pos.column + c_dir
          }
        in
        match Position.in_bounds neighbor ~game_kind:game.game_kind with
        | true -> Some neighbor
        | false -> None))
  ;;

  (* TODO: anytime this returns none, eliminate group*)

  let has_win_length_in_a_row
    (neighbors : Game.Position.t list list)
    (game : Game.t)
    (pos : Game.Position.t)
    =
    let open Game in
    let target_piece = Map.find_exn game.board pos in
    List.exists neighbors ~f:(fun group ->
      Int.equal (List.length group) (Game_kind.win_length game.game_kind - 1)
      && List.for_all group ~f:(fun neighbor ->
        Map.mem game.board neighbor
        && Piece.equal target_piece (Map.find_exn game.board neighbor)))
  ;;

  let is_game_over game =
    let open Game in
    let all_positions_placed = Map.keys game.board in
    let has_game_been_won =
      List.exists all_positions_placed ~f:(fun pos ->
        let neighbors = get_neighbors pos game in
        has_win_length_in_a_row neighbors game pos)
    in
    match has_game_been_won with
    | false ->
      (match available_moves game with [] -> true, None | _ -> false, None)
    | true ->
      ( true
      , Some
          (List.find_exn all_positions_placed ~f:(fun pos ->
             let neighbors = get_neighbors pos game in
             has_win_length_in_a_row neighbors game pos)) )
  ;;

  (* Exercise 2 *)
  let evaluate (game : Game.t) : Game.Evaluation.t =
    (* let open Game in *)
    let illegal_moves_detected = not (all_pos_in_bounds game) in
    match illegal_moves_detected with
    | true -> Illegal_move
    | false ->
      let is_game_over = is_game_over game in
      (match is_game_over with
       | true, Some winner_piece ->
         Game_over { winner = Some (Map.find_exn game.board winner_piece) }
       | true, None -> Game_over { winner = None }
       | _ -> Game_continues)
  ;;

  (* TODO: return is_game_over directly *)

  (* Exercise 3 *)
  let winning_moves ~(me : Game.Piece.t) (game : Game.t)
    : Game.Position.t list
    =
    let open Game in
    let unplaced_positions = available_moves game in
    List.filter_map unplaced_positions ~f:(fun position ->
      let game_after_move = place_piece game ~piece:me ~position in
      match evaluate game_after_move with
      | Game_over { winner = Some piece } ->
        (match Piece.equal piece me with
         | true -> Some position
         | false -> None)
      | _ -> None)
  ;;

  (* Exercise 4 *)
  let losing_moves ~(me : Game.Piece.t) (game : Game.t)
    : Game.Position.t list
    =
    let available_moves = available_moves game in
    let opponent_winning_moves =
      winning_moves ~me:(Game.Piece.flip me) game
    in
    match opponent_winning_moves with
    | [] -> []
    | _ ->
      List.filter available_moves ~f:(fun p ->
        not (List.mem opponent_winning_moves p ~equal:Game.Position.equal))
  ;;

  (* Exercise 5 *)
  let _available_moves_that_do_not_immediately_lose
    ~(me : Game.Piece.t)
    (game : Game.t)
    : Game.Position.t list
    =
    let possible_moves = available_moves game in
    List.filter possible_moves ~f:(fun position ->
      let game_after_move = place_piece game ~piece:me ~position in
      List.is_empty (losing_moves ~me game_after_move))
  ;;

  let count_unobstructed_pieces_of_pos
    (pos : Game.Position.t)
    (game : Game.t)
    =
    let open Game in
    let neighbors = get_neighbors pos game in
    let target_piece = Map.find_exn game.board pos in
    let count_of_unobstructed_pieces =
      List.fold neighbors ~init:0 ~f:(fun acc group ->
        let count_of_target_pieces =
          List.count group ~f:(fun piece_position ->
            let piece = Map.find game.board piece_position in
            match piece with
            | None -> false
            | Some p -> Piece.equal p target_piece)
        in
        let count_of_enemy_pieces =
          List.count group ~f:(fun piece_position ->
            let piece = Map.find game.board piece_position in
            match piece with
            | None -> false
            | Some p -> Piece.equal p (Piece.flip target_piece))
        in
        acc + (count_of_target_pieces - count_of_enemy_pieces))
    in
    count_of_unobstructed_pieces
  ;;

  let score_omok
    (game_eval : Game.Evaluation.t)
    ~(is_maxi_player : bool)
    ~(player_piece : Game.Piece.t)
    ~(game : Game.t)
    : int
    =
    (* is my board good? 400 wins - impossible wins; closer wins -> higher
       score (more pieces connected or just in the vicinity of the five
       should be favored) *)
    (* what is not worth descending do *)
    (* best conf : winning game *)

    (* specific strategies to upvote *)
    (* second best conf : three in a row (given that both sides are not
       occupied by enemy piece) && four in a row (given that both sides are
       not occupied by enemy piece) *)
    (* third : four in a row with both sides not occupied *)
    (* fourth : three in a row given that both sides are not occupied by
       enemy piece *)
    (* X-corner start; Not Middle -> lose *)
    let open Game in
    match is_maxi_player with
    | true ->
      (match game_eval with
       | Illegal_move -> 0
       | Game_over { winner = Some piece } ->
         (match Piece.equal piece player_piece with
          | true -> Int.max_value
          | false -> -1 * Int.max_value)
       | Game_over { winner = None } -> 0
       | Game_continues ->
         let positions_on_board = Map.keys game.board in
         let positions_of_player_piece =
           List.filter positions_on_board ~f:(fun pos ->
             Piece.equal (Map.find_exn game.board pos) player_piece)
         in
         let count_of_all_pieces_unobstructed =
           List.fold positions_of_player_piece ~init:0 ~f:(fun acc pos ->
            (* ptsrint_s [%sexp ] *)
             acc + count_unobstructed_pieces_of_pos pos game)
         in
         count_of_all_pieces_unobstructed * count_of_all_pieces_unobstructed)
    | false ->
      (match game_eval with
       | Illegal_move -> 0
       | Game_over { winner = Some piece } ->
         (match Piece.equal piece (Piece.flip player_piece) with
          | true -> Int.max_value
          | false -> -1 * Int.max_value)
       | Game_over { winner = None } -> 0
       | Game_continues ->
         (*-1 * count_highest_pieces_together game ~piece:(Piece.flip
           player_piece)*)
         let positions_on_board = Map.keys game.board in
         let positions_of_enemy_piece =
           List.filter positions_on_board ~f:(fun pos ->
             Piece.equal (Map.find_exn game.board pos) (Piece.flip player_piece))
         in
         let count_of_all_enemy_pieces_unobstructed =
           List.fold positions_of_enemy_piece ~init:0 ~f:(fun acc pos ->
             print_s [%sexp (pos, count_unobstructed_pieces_of_pos pos game : Game.Position.t * int)];
             acc + count_unobstructed_pieces_of_pos pos game)
         in
         (* print_s [%sexp (-1
         * (count_of_all_enemy_pieces_unobstructed
            * count_of_all_enemy_pieces_unobstructed) : int)]; *)
         -1
         * (count_of_all_enemy_pieces_unobstructed
            * count_of_all_enemy_pieces_unobstructed))
  ;;

  let score
    (game_eval : Game.Evaluation.t)
    ~(is_maxi_player : bool)
    ~(player_piece : Game.Piece.t)
    ~(game : Game.t)
    : int
    =
    let open Game in
    match game.game_kind with
    | Game_kind.Omok ->
      score_omok game_eval ~is_maxi_player ~player_piece ~game
    | Game_kind.Tic_tac_toe ->
      (match is_maxi_player with
       | true ->
         (match game_eval with
          | Illegal_move -> 0
          | Game_over { winner = Some piece } ->
            (match Piece.equal piece player_piece with
             | true -> Int.max_value
             | false -> -1 * Int.max_value)
          | Game_over { winner = None } -> 0
          | Game_continues ->
            (*count_highest_pieces_together game ~piece:player_piece*) 0)
       | false ->
         (match game_eval with
          | Illegal_move -> 0
          | Game_over { winner = Some piece } ->
            (match Piece.equal piece (Piece.flip player_piece) with
             | true -> Int.max_value
             | false -> -1 * Int.max_value)
          | Game_over { winner = None } -> 0
          | Game_continues ->
            (*-1 * count_highest_pieces_together game ~piece:(Piece.flip
              player_piece)*)
            0))
  ;;

  (* Exercise 6 *)
  let rec helper
    ~(player_piece : Game.Piece.t)
    ~(game : Game.t)
    ~(depth : int)
    ~(is_maxi_player : bool)
    ~(alpha : int)
    ~(beta : int)
    =
    let losing_moves = losing_moves ~me:player_piece game in
    let unplaced_moves = available_moves game in
    let unplaced_moves =
      List.filter unplaced_moves ~f:(fun p ->
        not (List.mem losing_moves p ~equal:Game.Position.equal))
    in
    let game_eval = evaluate game in
    (* print_endline "~~~~~~~~~~~~~~~~~~~~~~~~~"; *)
    (* print_s [%sexp (losing_moves : Game.Position.t list)]; *)
    (* print_s [%sexp (player_piece : Game.Piece.t)]; *)
    (* print_s [%sexp (is_maxi_player : bool)]; *)
    (* print_endline ""; *)
    (* print_game game; *)
    (* print_endline ""; *)
    (* print_s [%sexp (unplaced_moves : Game.Position.t list)]; *)
    (* print_endline "~~~~~~~~~~~~~~~~~~~~~~~~~"; *)
    match
      Int.( = ) depth 0
      || List.is_empty unplaced_moves
      || Game.Evaluation.is_over game_eval
      || Game.Evaluation.is_illegal game_eval
    with
    | true -> None, score game_eval ~is_maxi_player ~player_piece ~game
    | false ->
      let winning_moves = winning_moves ~me:player_piece game in
      (match winning_moves with
       | first :: _ ->
         ( Some first
         , (match is_maxi_player with
            | true -> Int.max_value
            | false -> Int.min_value) )
       | [] ->
         (* print_endline "--RECEIVED SCORES--"; print_s [%sexp (player_piece
            : Game.Piece.t)]; print_s [%sexp (is_maxi_player : bool)];
            print_s [%sexp (scores : int list)]; print_s [%sexp
            (Option.value_map (List.max_elt scores ~compare:Int.compare)
            ~default:(-1 * Int.max_value) ~f:Fn.id : int)]; print_endline
            "--RECEIVED SCORES--"; *)
         (match is_maxi_player with
          | true ->
            let positions_with_score =
              List.fold unplaced_moves ~init:[] ~f:(fun acc position ->
                let is_subtree_redundant =
                  List.exists acc ~f:(fun (_, s) -> s >= beta)
                in
                let max_alpha =
                  Option.value_map
                    (List.max_elt
                       acc
                       ~compare:(Comparable.lift Int.compare ~f:snd))
                    ~f:snd
                    ~default:alpha
                in
                match is_subtree_redundant with
                | true -> List.append acc [ position, 0 ]
                | false ->
                  let game_after_move =
                    place_piece ~position ~piece:player_piece game
                  in
                  let _, scr =
                    helper
                      ~player_piece:(Game.Piece.flip player_piece)
                      ~is_maxi_player:(not is_maxi_player)
                      ~game:game_after_move
                      ~depth:(depth - 1)
                      ~alpha:max_alpha
                      ~beta
                  in
                  List.append acc [ position, scr ])
            in
            print_scores positions_with_score;
            Option.value_map
              (List.max_elt
                 positions_with_score
                 ~compare:(Comparable.lift Int.compare ~f:snd))
              ~default:(None, Int.min_value)
              ~f:(fun (pos, score) -> Some pos, score)
          | false ->
            let positions_with_score =
              List.fold unplaced_moves ~init:[] ~f:(fun acc position ->
                let is_subtree_redundant =
                  List.exists acc ~f:(fun (_, s) -> s <= alpha)
                in
                let min_beta =
                  Option.value_map
                    (List.min_elt
                       acc
                       ~compare:(Comparable.lift Int.compare ~f:snd))
                    ~f:snd
                    ~default:beta
                in
                match is_subtree_redundant with
                | true -> List.append acc [ position, 0 ]
                | false ->
                  let game_after_move =
                    place_piece ~position ~piece:player_piece game
                  in
                  let _, s =
                    helper
                      ~player_piece:(Game.Piece.flip player_piece)
                      ~is_maxi_player:(not is_maxi_player)
                      ~game:game_after_move
                      ~depth:(depth - 1)
                      ~alpha
                      ~beta:min_beta
                  in
                  List.append acc [ position, s ])
            in
            Option.value_map
              (List.min_elt
                 positions_with_score
                 ~compare:(Comparable.lift Int.compare ~f:snd))
              ~default:(None, Int.max_value)
              ~f:(fun (pos, score) -> Some pos, score)))
  ;;

  let minmax ~(me : Game.Piece.t) (game : Game.t) =
    let open Game in
    let pos, _ =
      match game.game_kind with
      | Game_kind.Omok ->
        helper
          ~player_piece:me
          ~is_maxi_player:true
          ~game
          ~depth:1
          ~beta:Int.max_value
          ~alpha:Int.min_value
      | Game_kind.Tic_tac_toe ->
        helper
          ~player_piece:me
          ~is_maxi_player:true
          ~game
          ~depth:Int.max_value
          ~beta:Int.max_value
          ~alpha:Int.min_value
    in
    let default = { Position.row = 0; column = 0 } in
    Option.value_map pos ~default ~f:Fn.id
  ;;

  let exercise_one =
    Command.async
      ~summary:"Exercise 1: Where can I move?"
      (let%map_open.Command () = return () in
       fun () ->
         let moves = available_moves win_for_x in
         print_s [%sexp (moves : Game.Position.t list)];
         let moves = available_moves non_win in
         print_s [%sexp (moves : Game.Position.t list)];
         return ())
  ;;

  let exercise_two =
    Command.async
      ~summary:"Exercise 2: Is the game over?"
      (let%map_open.Command () = return () in
       fun () ->
         let evaluation = evaluate win_for_x in
         print_s [%sexp (evaluation : Game.Evaluation.t)];
         let evaluation = evaluate non_win in
         print_s [%sexp (evaluation : Game.Evaluation.t)];
         return ())
  ;;

  let piece_flag =
    let open Command.Param in
    flag
      "piece"
      (required (Arg_type.create Game.Piece.of_string))
      ~doc:
        ("PIECE "
         ^ (Game.Piece.all
            |> List.map ~f:Game.Piece.to_string
            |> String.concat ~sep:", "))
  ;;

  let exercise_three =
    Command.async
      ~summary:"Exercise 3: Is there a winning move?"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let winning_moves = winning_moves ~me:piece non_win in
         print_s [%sexp (winning_moves : Game.Position.t list)];
         return ())
  ;;

  let exercise_four =
    Command.async
      ~summary:"Exercise 4: Is there a losing move?"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let losing_moves = losing_moves ~me:piece non_win in
         print_s [%sexp (losing_moves : Game.Position.t list)];
         return ())
  ;;

  let minmax_t =
    Command.async
      ~summary:"MinMax Test"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         (* print_game minmax_test; *)
         let res = minmax ~me:piece minmax_test in
         print_endline "FINAL";
         print_s [%sexp (res : Game.Position.t)];
         return ())
  ;;

  let test_omok =
    Command.async
      ~summary:"Omok Score Function Test"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         print_game omok_game;
         let res = minmax ~me:piece omok_game in
         print_endline "FINAL";
         print_s [%sexp (res : Game.Position.t)];
         return ())
  ;;

  let test_score =
    Command.async
      ~summary:"Test Score"
      (let%map_open.Command () = return ()
       and _piece = piece_flag in
       fun () ->
         print_game omok_game;
         let eval = evaluate omok_game in
         let res = score eval ~is_maxi_player:true ~player_piece:(Game.Piece.of_string "O") ~game:omok_game in
         print_endline "FINAL";
         print_s [%sexp (res : int)];
         return ())

  let command =
    Command.group
      ~summary:"Exercises"
      [ "one", exercise_one
      ; "two", exercise_two
      ; "three", exercise_three
      ; "four", exercise_four
      ; "minmax", minmax_t
      ; "omok", test_omok
      ; "scr", test_score
      ]
  ;;
end

module Server = struct
  let handle_take_turn (_client : unit) (query : Rpcs.Take_turn.Query.t) =
    print_s
      [%message "Received take turn query" (query : Rpcs.Take_turn.Query.t)];
    let response =
      { Rpcs.Take_turn.Response.piece = query.you_play
      ; Rpcs.Take_turn.Response.position =
          Exercises.minmax ~me:query.you_play query.game
      }
    in
    print_s [%message "Response" (response : Rpcs.Take_turn.Response.t)];
    return response
  ;;

  let handle_game_over (_client : unit) (query : Rpcs.Game_over.Query.t) =
    print_s
      [%message "Received game over query" (query : Rpcs.Game_over.Query.t)];
    let response = () in
    return response
  ;;
end

let implementations =
  Rpc.Implementations.create_exn
    ~on_unknown_rpc:`Close_connection
    ~implementations:
      [ Rpc.Rpc.implement Rpcs.Take_turn.rpc Server.handle_take_turn
      ; Rpc.Rpc.implement Rpcs.Game_over.rpc Server.handle_game_over
      ]
;;

let command_play =
  Command.async
    ~summary:"Play"
    (let%map_open.Command () = return ()
     (* and _controller = flag "-controller" (required host_and_port) ~doc:"_
        host_and_port of controller" *)
     and port = flag "-port" (required int) ~doc:"_ port to listen on" in
     fun () ->
       (* We should start listing on the supplied [port], ready to handle
          incoming queries for [Take_turn] and [Game_over]. We should also
          connect to the controller and send a [Start_game] to initiate the
          game. *)
       let%bind server =
         Rpc.Connection.serve
           ~implementations
           ~initial_connection_state:(fun _client_identity _client_addr ->
             (* This constructs the "client" values which are passed to the
                implementation function above. We're just using unit for
                now. *)
             ())
           ~where_to_listen:(Tcp.Where_to_listen.of_port port)
           ()
       in
       Tcp.Server.close_finished server)
;;

let command =
  Command.group
    ~summary:"Game Strategies"
    [ "play", command_play; "exercises", Exercises.command ]
;;
