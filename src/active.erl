-module(active).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, build/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

build() ->
    gen_server:cast(?SERVER, build).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    erlfsmon:subscribe(),
    rebar_log:init(rebar_config:new()),

    erlang:process_flag(priority, low),

    {ok, fresh}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(build, State) ->
    run_rebar(compile, rebar_conf([])),
    {noreply, {last, user_build}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({_Pid, {erlfsmon,file_event}, {Path, Flags}}, State) ->
    CurDir = filename:absname(""),
    Cur = filename:split(CurDir),
    P = filename:split(Path),

    Result = case lists:prefix(Cur, P) of
        true ->
            Components = P -- Cur,
            %error_logger:info_msg("event: ~p ~p", [Components, Flags]),
            path_event(Components, Flags);
        false ->
            ok
    end,

    {noreply, {last, event, Path, Flags, Result}};
handle_info({load_ebin, Atom}, State) ->
    do_load_ebin(Atom),
    {noreply, {last, do_load_ebin, Atom}};
handle_info(Info, State) ->
    {noreply, {last, unk, Info}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

path_event(C, [E|_Events]) when
        E =:= created
        orelse E =:= modified
        orelse E =:= renamed ->
    path_modified_event(C);
path_event(C, [_E|Events]) ->
    path_event(C, Events);
path_event(_, []) ->
    done.

path_modified_event([P, Name, "src", EName|_Px] = _Path) when P =:= "apps" orelse P =:= "deps" ->
    run_rebar(compile, rebar_conf([{apps, Name}]));

path_modified_event(["src"|_Px] = _Path) ->
    run_rebar(compile, rebar_conf([{apps, toplevel_app()}]));

path_modified_event([P, _Name, "ebin", EName|_Px] = _Path) when P =:= "apps" orelse P =:= "deps" ->
    load_ebin(EName);

path_modified_event(["ebin", EName|_Px] = _Path) ->
    load_ebin(EName);

path_modified_event(P) ->
    error_logger:warning_msg("path_modified_event: ignoring: ~p", [P]),
    dont_care.

toplevel_app() -> lists:last(filename:split(filename:absname(""))).

rebar_default_conf() ->
    C = rebar_config:new(),
    C1 = rebar_config:base_config(C),

    %% Keep track of how many operations we do, so we can detect bad commands
    C2 = rebar_config:set_xconf(C1, operations, 0),

    %% Initialize vsn cache
    C3 = rebar_config:set_xconf(C2, vsn_cache, dict:new()),

    %%% Determine the location of the rebar executable; important for pulling
    %%% resources out of the escript
    %ScriptName = filename:absname(escript:script_name()),
    %BaseConfig1 = rebar_config:set_xconf(BaseConfig, escript, ScriptName),
    %?DEBUG("Rebar location: ~p\n", [ScriptName]),

    %% Note the top-level directory for reference
    AbsCwd = filename:absname(rebar_utils:get_cwd()),
    C4 = rebar_config:set_xconf(C3, base_dir, AbsCwd),
    C4.

rebar_conf([{Key, Value}|Args], Conf) ->
    rebar_conf(Args, rebar_config:set_global(Conf, Key, Value));
rebar_conf([], Conf) ->
    Conf.

rebar_conf(Args) -> rebar_conf(Args, rebar_default_conf()).

run_rebar(Commands, Conf) when is_list(Commands) -> 
    R = (catch rebar_core:process_commands(Commands, Conf)),
    R;
run_rebar(Command, Conf) ->
    run_rebar([Command], Conf).

%%
%% TODO: discover any compile callbacks in rebar and stop using filesystem events for beam loads
%%
load_ebin(EName) ->
    Tokens = string:tokens(EName, "."),
    case Tokens of
        [Name, "beam"] ->
            do_load_ebin(list_to_atom(Name));
        [Name, "bea#"] ->
            case monitor_handles_renames() of
                false ->
                    erlang:send_after(1000, ?SERVER, {load_ebin, list_to_atom(Name)}),
                    delayed;
                true ->
                    ignored
            end;
        %[Name, Smth] -> ok;
        _ ->
            error_logger:warning_msg("load_ebin: unknown ebin file: ~p", [EName]),
            ok
    end.

do_load_ebin(Module) ->
    {Module, Binary, Filename} = code:get_object_code(Module),
    code:load_binary(Module, Filename, Binary),
    error_logger:info_msg("active: do_load_ebin: loaded: ~p", [Module]),
    reloaded.

monitor_handles_renames([renamed|_]) -> true;
monitor_handles_renames([_|Events]) -> monitor_handles_renames(Events);
monitor_handles_renames([]) -> false.

monitor_handles_renames() ->
    case get(monitor_handles_renames) of
        undefined ->
            R = monitor_handles_renames(erlfsmon:known_events()),
            put(monitor_handles_renames, R),
            R;
        V -> V
    end.
