-module(geoip_server).
-behaviour(gen_server).

%% API
-export([
        start_link/1,
        start_link/2,
        start_link/3,
        get_country_name_by_ip/1,
        get_country_code_by_ip/1,
        get_country_code3_by_ip/1
    ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
        geoip :: port()
    }).


-spec start_link(string()) -> {ok, pid()} | ignore | {error, term()}.
start_link(GeoIPData) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [GeoIPData], []).
start_link(GeoIPData, Type) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [GeoIPData, Type], []).
start_link(GeoIPData, Type, ReturnType) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [GeoIPData, Type, 
						      ReturnType], []).


get_country_name_by_ip(Ip) ->
    gen_server:call(?SERVER, {get_country_name_by_ip, Ip}).
get_country_code_by_ip(Ip) ->
    gen_server:call(?SERVER, {get_country_code_by_ip, Ip}).
get_country_code3_by_ip(Ip) ->
    gen_server:call(?SERVER, {get_country_code3_by_ip, Ip}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Args) ->
    case erlang:apply(geoip, new, Args) of
        {ok, Pid} ->
            State = #state{
                geoip = Pid
            },
            {ok, State};
        {error, Error} ->
            {stop, Error}
    end.

handle_call({get_country_name_by_ip, Ip}, _From, State) ->
    Result = geoip:get_country_name_by_ip(State#state.geoip, Ip),
    {reply, Result, State};

handle_call({get_country_code_by_ip, Ip}, _From, State) ->
    Result = geoip:get_country_code_by_ip(State#state.geoip, Ip),
    {reply, Result, State};

handle_call({get_country_code3_by_ip, Ip}, _From, State) ->
    Result = geoip:get_country_code3_by_ip(State#state.geoip, Ip),
    {reply, Result, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    geoip:delete(State#state.geoip).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
