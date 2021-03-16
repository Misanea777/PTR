-module(eng_anal).
-export([analyze/1]).

analyze(Tweet) ->
    
    Followers = ej:get({"user", "followers_count"}, Tweet),
    Favorites = ej:get({"favorite_count"}, Tweet),
    Retweets = ej:get({"retweet_count"}, Tweet),

    get_score(Followers, Favorites, Retweets).
    
    

get_score(0, _Favorites, _Retweets) ->
    0;

get_score(Followers, Favorites, Retweets) ->
    (Favorites + Retweets) / Followers.




