-module(mqttjwt_app).

-behaviour(application).

-emqx_plugin(?MODULE).

-export([ start/2
        , stop/1
        ]).

start(_StartType, _StartArgs) ->
    {ok, Sup} = mqttjwt_sup:start_link(),
    mqttjwt:load(application:get_all_env()),

    emqx_ctl:register_command(mqttjwt, {mqttjwt_cli, cmd}),
    {ok, Sup}.

stop(_State) ->
    emqx_ctl:unregister_command(mqttjwt),
    mqttjwt:unload().
