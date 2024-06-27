-module(rebar3_muerl_prv).

-export([init/1, do/1, format_error/1]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider =
        providers:create([{name, muerl},
                          {module, ?MODULE},
                          {bare, true},
                          {deps, [app_discovery]},
                          {example, "rebar3 muerl"},
                          {opts, opts()},
                          {short_desc, "A rebar plugin to apply mutation testing to your codebase"},
                          {desc, ""}]),
    {ok, rebar_state:add_provider(State, Provider)}.

opts() ->
    [{mode, $m, "mode", {atom, list_mutations}, "Target mode (list_mutations / mutate)"},
     {dir, $d, "dir", string, "Directory on which to search for files to run muerl with"},
     {files,
      $f,
      "files",
      string,
      "Force muerl to be run on a specific file (or a series of comma-separated files)"},
     {parallel_processing,
      undefined,
      "parallel_processing",
      {boolean, true},
      "if the files should be processed in parallel"},
     {pretty_print_list,
      undefined,
      "pretty_print_list",
      {boolean, false},
      "if the list of mutations should be pretty-printed"}].

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Opts0 = handle_opts(State),
    rebar_api:debug("Opts:~n\t~p", [Opts0]),
    Files = get_files_from_opts(Opts0),
    Opts = remove_rebar_only_opts(Opts0),
    rebar_api:debug("muerl will run on the following files:~n\t~p", [Files]),
    try muerl:run(Files, Opts) of
        Map when is_map(Map) ->
            print_result(Map, Opts, length(Files)),
            {ok, State}
    catch
        Kind:Error:Stack ->
            rebar_api:warn("~p executing on files: ~p\nStack: ~p", [Kind, Error, Stack]),
            {error, format_error(Error)}
    end.

print_result(#{results := Mutations}, #{mode := list_mutations}, NumOfFiles) ->
    rebar_api:info("Found a total of ~p mutations across ~p files",
                   [length(Mutations), NumOfFiles]);
print_result(_Result, #{mode := mutate}, _NumOfFiles) ->
    rebar_api:warn("Not implemented yet, sorry!").

-spec get_files_from_opts(muerl:options()) -> [file:filename()].
get_files_from_opts(#{files := Files}) ->
    Files1 = re:split(Files, ", *", [{return, list}]),
    lists:map(fun rebar_file_utils:absolute_path/1, Files1);
get_files_from_opts(#{dir := Dir}) ->
    AbsPath = rebar_file_utils:absolute_path(Dir),
    [filename:join(AbsPath, F)
     || F <- filelib:wildcard("**/*.erl", AbsPath), not is_hidden(F)];
get_files_from_opts(_Opts) ->
    [F || F <- filelib:wildcard("**/*.erl"), not is_hidden(F)].

-spec handle_opts(rebar_state:t()) -> map().
handle_opts(State) ->
    {CliOpts, _} = rebar_state:command_parsed_args(State),
    ConfigOpts = rebar_state:get(State, muerl, []),
    rebar_api:debug("CliOpts:~n\t~p~nConfigOpts:~n\t~p", [CliOpts, ConfigOpts]),
    maps:from_list(
        rebar_utils:tup_umerge(CliOpts, ConfigOpts)).

-spec remove_rebar_only_opts(map()) -> muerl:options().
remove_rebar_only_opts(Opts0) ->
    Opts1 = maps:remove(dir, Opts0),
    maps:remove(dir, Opts1).

is_hidden(Filename) ->
    lists:any(fun is_hidden_name/1, filename:split(Filename)).

is_hidden_name(".") ->
    false;
is_hidden_name("..") ->
    false;
is_hidden_name("." ++ _) ->
    true;
is_hidden_name("_" ++ _) ->
    true;
is_hidden_name(_) ->
    false.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
