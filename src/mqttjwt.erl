-module(mqttjwt).

-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/emqx_hooks.hrl").
-include_lib("emqx/include/logger.hrl").

%% Module documentation here
% -import([jwt]).

-export([load/1, unload/0]).

%% Export only necessary functions
-export([on_client_subscribe/4, on_message_delivered/3]).

-export([extract_exp/2]).

%% Load the plugin
load(Env) ->
    hook('client.subscribe', {?MODULE, on_client_subscribe, [Env]}),
    hook('message.delivered', {?MODULE, on_message_delivered, [Env]}).

%% Unload the plugin
unload() ->
    unhook('client.subscribe', {?MODULE, on_client_subscribe, []}),
    unhook('message.delivered', {?MODULE, on_message_delivered, []}).

%% Handle client subscribe event
on_client_subscribe(#{clientid := ClientId}, _Properties, TopicFilters, _Env) ->
    % io:format("Client ~s will subscribe to topics: ~p~n", [ClientId, TopicFilters]),
    % emqx_logger:info("Client ~s will subscribe to topics: ~p~n", [ClientId, TopicFilters]),
    {ok, TopicFilters}.



%% Handle message delivered event
on_message_delivered(_ClientInfo = #{clientid := ClientId,username :=Username}, Message, _Env) ->
    extract_exp(Username,ClientId),
    {ok, Message}.
    

extract_exp(JWTToken, ClientId) ->
    case binary:split(JWTToken, <<".">>, [global]) of
        [_, Payload, _] ->
            case jose_base64url:decode(Payload) of
                {_, DecodedPayload} ->
                    case jsx:decode(DecodedPayload) of
                        PayloadMap ->
                            
                            Exp = maps:get(<<"exp">>, PayloadMap),
                            CurrentTime = erlang:system_time(second),
                        
                            case Exp > CurrentTime of
                                true ->
                                    % io:format("Token is still valid.~n"),
                                    {ok, "Token is still valid."};
                                false ->
                                    % io:format("Token has expired.~n"),
                                    case emqx_mgmt:kickout_client(ClientId) of
                                        ok ->
                                            {ok, "Token has expired."};
                                        _ ->
                                            ok
                                    end
                            end;
                        Error ->
                            % io:format("Error decoding Payload: ~p~n", [Error]),
                            {error, Error}
                    end;
                Error ->
                    % io:format("Error decoding JWT payload: ~p~n", [Error]),
                    {error, Error}
            end;
        _ ->
            % io:format("Invalid JWT token format.~n"),
            {error, "Invalid JWT token format."}
    end.

%% Add a hook
hook(HookPoint, MFA) ->
    case emqx_hooks:add(HookPoint, MFA, _Property = ?HP_HIGHEST) of
        ok -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% Remove a hook
unhook(HookPoint, MFA) ->
    emqx_hooks:del(HookPoint, MFA).
