-module(aggregator).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(MAX_BUFF_SIZE, 1000*4).
-define(MAX_AGG_PARTS, 1000*2).

start_link() ->
    gen_server:start_link({local, aggregator}, ?MODULE, [], []).

init(_Args) ->
    ets:new(buffer,[ordered_set, public, named_table]),
    {ok, []}.



% handle_cast(_Msg, State) ->
%     {noreply, State}.


% Debugging



handle_cast({Type, {Id, Part}}, State) ->
    delete_if_full(),

    Res = insert_or_merge({Id, [{Type, Part}]}),
    NewState = create_state(State, Res),
    io:format("Aggregated:: ~p~n", [length(NewState)]),
    % io:format("BufferSize:: ~p~n", [proplists:get_value(size, ets:info(buffer))]),
    {noreply, NewState}.


% Logic

create_state(OldState, []) -> % State is a placeholder for aggregated parts
    OldState;

create_state(OldState, NewState) ->
    [NewState|OldState].

delete_if_full() ->
    delete_if_full(proplists:get_value(size, ets:info(buffer))).

delete_if_full(Size) when Size >= ?MAX_BUFF_SIZE ->
    delete_oldest_from_buff();

delete_if_full(_Size) ->
    ok.

% --Buffer operations

% unique id is linked with time -> the bigger the id the more young the part is
delete_oldest_from_buff() ->
    delete_from_buff(ets:first(buffer)).  

insert_or_merge({Id, Part}) ->
    Res = insert_in_buff({Id, Part}), % try to insert in buff
    insert_or_merge({Id, Part}, Res).
    

insert_or_merge(_Part, true) -> % if true -> part inserted
    [];

insert_or_merge({Id, NewPart}, false) -> % if false -> part merged with an existing one
    [{_,OldPart}]  = ets:lookup(buffer, Id),
    MergedPart = merge_parts(OldPart, NewPart),
    update_part_in_buff({Id, MergedPart}),
    manage_completed_part({Id, MergedPart}, check_if_complete(MergedPart)).
    


insert_in_buff({Id, Part}) ->
    ets:insert_new(buffer, {Id, Part}).

update_part_in_buff({Id, NewPart}) ->
    ets:insert(buffer, {Id, NewPart}).

delete_from_buff(Id) ->
    ets:delete(buffer, Id).

% --Parts operations

merge_parts(OldPart, NewPart) ->
    OldPart ++ NewPart.


check_if_complete(Part) ->
    length(Part) >= 3.

manage_completed_part({Id, Part}, true) ->
    delete_from_buff(Id),
    Part;

manage_completed_part(_, false) ->
    [].




% Useless fun

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
