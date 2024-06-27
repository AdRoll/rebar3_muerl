rebar3_muerl
=====

A rebar plugin to apply mutation testing to your codebase

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {rebar3_muerl, {git, "https://host/user/rebar3_muerl.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 rebar3_muerl
    ===> Fetching rebar3_muerl
    ===> Compiling rebar3_muerl
    <Plugin Output>
