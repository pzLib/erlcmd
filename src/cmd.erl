%%%-------------------------------------------------------------------
%%% @author Peng Zheng (彭峥)
%%% @copyright (C) 2019, https://github.com/pzLib/erlcmd.git
%%% @doc
%%% A simple tool function like os:cmd return with exit status code of process.
%%% @end
%%% Created : 2019-07-25
%%%-------------------------------------------------------------------

-module(cmd).

-export([
    run/2
]).


-spec run(
    Cmd :: string(),
    Timeout :: non_neg_integer()
) ->
    {ok, ExitStatus :: number(), Output :: string()}
    | {error, timeout}.
run(Cmd, Timeout) when is_integer(Timeout), Timeout > 0, is_list(Cmd) ->
    [Filename | Args] = string:split(Cmd, " ", all),
    Exe = os:find_executable(Filename),
    Port = open_port({spawn_executable, Exe}, [exit_status, use_stdio, stderr_to_stdout, {args, Args}]),
    wait_for(Port, Timeout, "").


wait_for(Port, Timeout, Output) ->
    receive
        {Port, {data, Data}} -> wait_for(Port, Timeout, [Output | Data]);
        {Port, {exit_status, Status}} -> {ok, Status, unicode:characters_to_list(Output)}
    after Timeout ->
        {os_pid, PID} = erlang:port_info(Port, os_pid),
        % kill process if timeout
        os:cmd("kill " ++ integer_to_list(PID)),
        receive
            % ignored message: {Port, {exit_status, _}}
            _ -> ok
        end,
        {error, timeout}
    end.

