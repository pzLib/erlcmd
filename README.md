# erlcmd

Erlcmd includes a simple tool function `cmd:run`. It likes `os:cmd` but returns exit status code and stdio output.

## How to use

Download src/cmd.erl to your local disk then run:

```erlang
Exe = os:find_executable("ls"),
CmdLine = Exe ++ " -a ./src",
{ok, ExitStatus, Output} = cmd:run(CmdLine, 1000),
io:format("exit status code: ~p~noutput: ~n~s", [ExitStatus, Output]).
``` 

Or like this:

```erlang
Exe = os:find_executable("ls"),
{ok, ExitStatus, Output} = cmd:run(Exe, ["-a", "./src"], 5000), 
io:format("exit status code: ~p~noutput: ~n~s", [ExitStatus, Output]).
``` 

## rebar.config

Add the following script into your rebar.config if you wanna manage your project by rebar.

```erlang
{deps, [
    {erlcmd, "*", {git, "https://github.com/pzlib/erlcmd.git", {tag, "0.1"}}}
]}.
```
