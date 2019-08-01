# erlcmd

A simple tool function like os:cmd return with exit status code of process.

## How to use

Download src/cmd.erl to your local disk then run:

```erlang
{ok, ExitStatus, Output} = cmd:run("ls -a ./src", 5000), 
io:format("exit status code: ~p~noutput: ~n~s~n", [ExitStatus, Output]).
``` 

Or like this:

```erlang
{ok, ExitStatus, Output} = cmd:run("ls", ["-a", "./src"], 5000), 
io:format("exit status code: ~p~noutput: ~n~s~n", [ExitStatus, Output]).
``` 

## rebar.config

Add the following script into your rebar.config if you wanna manage your project by rebar.

```erlang
{deps, [
    {erlcmd, "*", {git, "https://github.com/pzlib/erlcmd.git", {tag, "0.1"}}}
]}.
```
