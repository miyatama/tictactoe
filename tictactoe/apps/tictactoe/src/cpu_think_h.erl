-module(cpu_think_h).

-export([init/2,
  allowed_methods/2,
  content_types_accepted/2,
  content_types_provided/2,
  think_logic/2]).

-define(OUTPUT_DEBUG(S), io:fwrite("[DEBUG] cpu_think_h: " ++ S ++ "~n")).
-define(OUTPUT_DEBUG(S, Args), io:fwrite("[DEBUG] cpu_think_h: " ++ S ++ "~n", Args)).
-define(OUTPUT_ERROR(S, Args), io:fwrite("[ERROR] cpu_think_h: " ++ S ++ "~n", Args)).

% side = main | opponent
% main is cpu
% opponent is virtual user
% rf(player_record).
% rd(player_record, {side}).
-record(player_record, {side}).

% pos -> {X:integer(), Y:integer()}, state -> main | opponent | null
% rf(game_state_record).
% rd(game_state_record, {position, state}).
-record(game_state_record,{position, state}).
-define(MAX_PLYDEPTH, 99).
-define(WIN_SCORE, 100).
-define(LOSE_SCORE, -100).
-define(DRAW_SCORE, 0).

init(Req, Opts) ->
  ?OUTPUT_DEBUG("init/2"),
  {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
  ?OUTPUT_DEBUG("allowed_methods/2"),
  {
    [
      <<"POST">>
    ],
    Req,
    State 
  }.

content_types_accepted(Req, State) ->
  ?OUTPUT_DEBUG("content_types_accepted/2"),
  {
    [
      {{<<"application">>, <<"json">>, '*'}, think_logic}
    ],
    Req,
   State 
  }.

content_types_provided(Req, State) ->
  ?OUTPUT_DEBUG("content_types_provided/2"),
  {
    [
      {{<<"application">>, <<"json">>, '*'}, think_logic}
    ],
    Req,
   State 
  }.

% curl -m 120 -X POST -d '{"troutes": [0,0,0,0,0,0,0,0,0]}' -H 'Content-Type: application/json' -H 'Accept: application/json' http://localhost/cpu_think
% curl -X POST -d '{"troutes": [1,0,0,0,0,0,0,0,0]}' -H 'Content-Type: application/json' -H 'Accept: application/json' http://localhost/cpu_think
% curl -X POST -d '{"troutes": [1,1,2,0,0,2,1,0,0]}' -H 'Content-Type: application/json' -H 'Accept: application/json' http://localhost/cpu_think
think_logic(Req, State) ->
  ?OUTPUT_DEBUG("think_logic/2"),
  Response = case cowboy_req:method(Req) of
    <<"POST">> -> 
      post_think_logic(Req, State);
    Method -> 
      ?OUTPUT_ERROR(
        "think_logic unknown method: ~w",
        [Method]),
      Resp2 = cowboy_req:reply(
        500,
        jsone:encode(<<"\"error_reason\": \"unknown method\"">>)),
      {true, Resp2, State}
  end,
  ?OUTPUT_DEBUG(
      "think_logic - response: ~w",
      [Response]),
  Response.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% private function                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
post_think_logic(Req, State) ->
  ?OUTPUT_DEBUG("post_think_logic/2"),
  GameState = generate_game_state(Req),
  ?OUTPUT_DEBUG("post_think_logic/2 - game state: ~w", [GameState]),
  MainPlayer = generate_main_player(),
  {Move, _} = best_move(GameState, MainPlayer),
  {X, Y, Succeed} = case Move of
      null ->
          {0, 0, false};
      SucceedMove ->
          {MoveX, MoveY} = SucceedMove,
          {MoveX, MoveY, true}
  end,
  ResultJson = #{
      <<"succeed">> => Succeed,
      <<"x">> => X,
      <<"y">> => Y
  },
  ?OUTPUT_DEBUG("post_think_logic/2 - think result: ~w", [ResultJson]),
  Resp = cowboy_req:set_resp_body(
    jsone:encode(ResultJson),
    Req
  ),
  Resp2 = cowboy_req:reply(
    200,
    Resp),
  {true, Resp2, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% minimax logic                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

best_move(State, MainPlayer) ->
    PlyDepth = ?MAX_PLYDEPTH,
    minimax(State, PlyDepth, MainPlayer, null, null).

% minimax(GameState, PlyDepth, MainPlayer) -> {Move, Score}.
-spec minimax(list(), integer(), map(), map(), integer()) -> {map(), integer()}.
minimax(GameState, PlyDepth, MainPlayer, Move, _)  ->
    case is_leaf_scene(GameState, PlyDepth) of
        true ->
            NewScore = evaluate_score(GameState, MainPlayer),
            {Move, NewScore};
        false ->
            EmptyPositions = get_null_positions(GameState),
            minimax(
                GameState, 
                PlyDepth, 
                MainPlayer, 
                EmptyPositions , 
                null, 
                get_side_min_score(MainPlayer#player_record.side))
    end.

-spec minimax(list(), integer(), map(), list(), map(), integer()) -> {map(), integer()}.
minimax(_, _, _, [], Move, Score) -> 
    {Move, Score};
minimax(GameState, PlyDepth, MainPlayer, EmptyPositions, Move, Score) ->
    [EmptyPosition | EmptyPositionRetain] = EmptyPositions,
    GameStateAfterPlayer = set_game_state(GameState, MainPlayer, EmptyPosition),
    {_, OpponentScore} = minimax(
        GameStateAfterPlayer, 
        PlyDepth - 1, 
        get_opponent_player(MainPlayer), 
        Move, 
        Score),
    NeedExchangeMove = need_exchange_move(
        get_opponent_player(MainPlayer),
        Move, 
        Score, 
        OpponentScore),
    {NewMove, NewScore} = case NeedExchangeMove of
        true -> 
            {EmptyPosition, OpponentScore};
        _ -> {Move, Score}
    end,
    minimax(GameState, PlyDepth, MainPlayer, EmptyPositionRetain, NewMove, NewScore).

% evaluate_score().
% return player score
-spec evaluate_score(list(), map()) -> integer().
evaluate_score(GameState, Player) -> 
    Side = Player#player_record.side,
    % many empty state is short circuit
    case is_win_game(GameState, Side) of
        win -> ?WIN_SCORE + length(get_null_positions(GameState));
        lose -> ?LOSE_SCORE - length(get_null_positions(GameState));
        _ -> calculate_draw_score(GameState, Side)
    end. 

-spec is_win_game(list(), main | opponent) -> win | lose | draw.
is_win_game(GameState, PlayerSide) ->
    Positions = get_lines(),
    PlayerWinState = is_win_game(GameState, PlayerSide, Positions),
    OpponentWinState = is_win_game(GameState, get_opponent(PlayerSide), Positions),
    case {PlayerWinState, OpponentWinState} of
        {win, win} -> draw;
        {win, _} -> win;
        {_, win} -> lose;
        _ -> draw
    end.

-spec is_win_game(list(), main | opponent, list()) -> win | no_win.
is_win_game(_, _, []) -> draw;
is_win_game(GameState, PlayerSide, Positions) when is_list(Positions) ->
    [Position|Retain] = Positions,
    PlayerWinGameResult = is_player_win_game(GameState, PlayerSide, Position),
    case PlayerWinGameResult of
        win -> win;
        _ -> is_win_game(GameState, PlayerSide, Retain)
    end.

-spec is_player_win_game(list(), main | opponent, list()) -> win | not_win.
is_player_win_game(GameState, PlayerSide, Position)  ->
    PositionState = get_position_state(GameState, Position),
    PlayerCount = count_player_side(PositionState, PlayerSide),
    OpponentCount = count_player_side(PositionState, get_opponent(PlayerSide)),
    case {PlayerCount, OpponentCount} of
        % もう勝ってる
        {3, 0} -> win;
        % 分からん
        _ -> not_win
    end.

% get_position_state()
% return a state list at specified positions. 
-spec get_position_state(list(), list()) -> list().
get_position_state(_, []) -> [];
get_position_state(GameState, Positions) ->
    [Position | Retain] = Positions,
    State = lists:keyfind(Position,
                        #game_state_record.position,
                        GameState),
    [State#game_state_record.state] ++ 
    get_position_state(GameState, Retain).

get_lines() ->
    [
        % vertical line
        [{1,1},{1,2},{1,3}],
        [{2,1},{2,2},{2,3}],
        [{3,1},{3,2},{3,3}],
        % horizontal line
        [{1,1},{2,1},{3,1}],
        [{1,2},{2,2},{3,2}],
        [{1,3},{2,3},{3,3}],
        % diagonal line
        [{1,1},{2,2},{3,3}],
        [{3,1},{2,2},{1,3}]
    ].

-spec calculate_draw_score(list(), main | opponent) -> integer().
calculate_draw_score(GameState, Side) -> 
    PlayerValidLine = calculate_valid_line(GameState, get_lines(), Side),
    OpponentValidLine = calculate_valid_line(GameState, get_lines(), get_opponent(Side)),
    PlayerValidLine - OpponentValidLine.

-spec calculate_valid_line(list(), list(), main | opponent) -> integer().
calculate_valid_line(_, [], _) -> 0;
calculate_valid_line(GameState, Lines, PlayerSide) ->
    [Line|LineRetain] = Lines,
    ValidPositionCount = count_valid_position(GameState, Line, PlayerSide),
    LineState = case  ValidPositionCount of
        3 -> 1;
        _ -> 0
    end,
    LineState + calculate_valid_line(GameState, LineRetain, PlayerSide).

-spec count_valid_position(list(), list(), main | opponent) -> integer().
count_valid_position(GameState, Line, PlayerSide) ->
    PositionState = get_position_state(GameState, Line),
    ValidPosition = [X || X <- PositionState, X /= get_opponent(PlayerSide)],
    length(ValidPosition).

-spec count_player_side(list(), main | opponent) -> integer().
count_player_side(StateList, PlayerSide) ->
    length([X || X <- StateList, X == PlayerSide]).

-spec need_exchange_move(map(), map() | null, integer(), integer()) -> true | false.
need_exchange_move(_, null, _, _) -> true;
need_exchange_move(Player, _, Score1, Score2) ->
    case {is_main_player(Player), compare_score(Score1,Score2)} of
        {true, lower_than} -> true;    
        {false, greater_than} -> true;
        _ -> false
    end.

compare_score(Score1, Score1) -> equal_to;
compare_score(Score1, Score2) when Score1 > Score2 -> greater_than;
compare_score(_, _) -> lower_than.

is_leaf_scene(_, PlyDepth) when PlyDepth == 0 -> true;
is_leaf_scene(GameState, PlyDepth) ->
    is_leaf_scene(GameState, PlyDepth, is_win_game(GameState, main)).
is_leaf_scene(_, _, win) -> true;
is_leaf_scene(_, _, lose) -> true;
is_leaf_scene(GameState, _, _) ->
    case get_null_positions(GameState) of
        [] -> true;
        _ -> false 
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GameState
-spec generate_game_state(map()) -> list().
generate_game_state(Req) ->
    {ok, Body, Req1} = cowboy_req:read_urlencoded_body(Req),
    case get_body(Body, Req1) of
        {ok, Input, _Req} ->
            % "{\"troutes\": [0,0,0,0,0,0,0,0,0]}"
            Json = jsone:decode(
                Input, 
                [{keys, attempt_atom}]),
            ?OUTPUT_DEBUG(
                "generate_game_state - request.input: ~w",
                [Json]),
            TroutStates = maps:get(troutes,Json),
            generate_game_state(TroutStates, 1);
        _ ->
             []
    end.
generate_game_state([], _) -> [];
generate_game_state(TroutStates, Index) ->
    [TroutState|TroutStateRetain] = TroutStates,
    State = case TroutState of
        1 -> opponent;
        2 -> main;
        _ -> null
    end,
    X = trunc(math:floor((Index - 1) / 3) + 1),
    Y = trunc(math:floor((Index - 1) rem 3) + 1),
    [#game_state_record{position={X, Y}, state=State}] ++
    generate_game_state(TroutStateRetain, Index + 1).

get_body(Body, Req) ->
    case Body of 
        [{Input, true}] ->
            {ok, Input, Req};
        _ ->
            error
    end.

get_null_positions([]) -> [];
get_null_positions(GameState) ->
    [Position || #game_state_record{position=Position, state=State} <- GameState, State == null].

set_game_state(GameState, Player, Position) ->
    PositionState = lists:keyfind(Position,
                        #game_state_record.position,
                        GameState),
    case PositionState#game_state_record.state of
        null ->
            [
            PositionState#game_state_record{state = Player#player_record.side} 
            | lists:keydelete(Position,
                            #game_state_record.position,
                            GameState)];
        _ ->
            ?OUTPUT_ERROR("set_game_state - ~w", [replace_not_null]),
            [
            PositionState#game_state_record{state = Player#player_record.side} 
            | lists:keydelete(Position,
                            #game_state_record.position,
                            GameState)]
    end.

get_side_min_score(main) ->    ?LOSE_SCORE;
get_side_min_score(opponent) -> ?WIN_SCORE.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Player
generate_main_player() -> #player_record{side=main}.

-spec get_opponent(main | opponent) -> main | opponent.
get_opponent(main) -> opponent;
get_opponent(opponent) -> main.

get_opponent_player(Player) ->
    Side = get_opponent(Player#player_record.side),
    #player_record{side=Side}.

is_main_player(Player) ->
    Player#player_record.side == main.
