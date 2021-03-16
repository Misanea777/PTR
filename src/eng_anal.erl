-module(eng_anal).
-export([analyze/1]).

analyze(Tweet) ->
    
    Followers = ej:get({"message", "tweet", "user", "followers_count"}, Tweet),
    Favorites = ej:get({"message", "tweet", "favorite_count"}, Tweet),
    Retweets = ej:get({"message", "tweet", "retweet_count"}, Tweet),

    Retweet_status = ej:get({"message", "tweet", "retweeted_status"}, Tweet),

    get_score(Followers, Favorites, Retweets).
    
    

get_score(0, _Favorites, _Retweets) ->
    0;

get_score(Followers, Favorites, Retweets) ->
    (Favorites + Retweets) / Followers.




