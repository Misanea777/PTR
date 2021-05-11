-module(topicker).

-compile(export_all).

% Just a topic api 

get_all_topics() ->
    ets:match_object(topics, {topic, '$1'}).

subscribe_to_topic(Topic, Consumer) ->
    case is_present(Topic) of
        true ->
            ets:insert(subscriptions, {Topic, Consumer}),
            ok;
        false ->
            error
    end.

subscribe_to_topics(Topics = [H|T], Consumer) ->
    subscribe_to_topic(H, Consumer),
    subscribe_to_topics(T, Consumer);


subscribe_to_topics(Topics = [], Consumer) ->
    ok.

unsuscribe_from_topic(Topic, Consumer) ->
    ets:match_delete(subscriptions, {Topic, Consumer}).

unsuscribe_from_topics(Topics = [H|T], Consumer) ->
    unsuscribe_from_topic(H, Consumer),
    unsuscribe_from_topic(T, Consumer);


unsuscribe_from_topics(Topics = [], Consumer) ->
    ok.

get_subscribtions(Topic) ->
    ets:match_object(subscriptions, {Topic, '$1'}).


get_subscribtions_from_topics(Topics) ->
    get_subscribtions_from_topics(Topics, []).

get_subscribtions_from_topics(_Topics = [H|T], Acc) ->
    get_subscribtions_from_topics(T, [get_subscribtions(H)|Acc]);

get_subscribtions_from_topics([], Acc) ->
    lists:flatten(Acc).

create_topic(Topic) ->
    ets:insert(topics, {topic, Topic}).

create_topics(Topics = [H|T]) ->
    create_topic(H),
    create_topics(T);

create_topics(Topics = []) ->
    ok.

delete_topic(Topic) ->
    ets:match_delete(topics, {topic, Topic}).

delete_topics(Topics = [H|T]) ->
    delete_topic(H),
    delete_topics(T);

delete_topics(Topics = []) ->
    ok.



is_present([]) ->
    false;

is_present([_H|_T]) ->
    true;


is_present(Topic) ->
    is_present(ets:match_object(topics, {topic, Topic})).


