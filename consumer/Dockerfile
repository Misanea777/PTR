# FROM erlang:latest
# WORKDIR /app/src
# COPY rebar.config .
# RUN rebar3 compile
# COPY . .
# EXPOSE 4020
# CMD ["rebar3", "shell"]   


FROM erlang:latest

WORKDIR /app

COPY rebar.config .

RUN rebar3 compile

