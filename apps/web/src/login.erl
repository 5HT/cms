-module(login).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("n2o_bootstrap/include/wf.hrl").
-include_lib("kvs/include/users.hrl").
-include("records.hrl").
-define(LOGIN,[facebook,google,twitter]).

main() ->
  avz:callbacks(?LOGIN),
  [#dtl{file = "dev", ext="dtl", 
        bindings=[{title,<<"Login">>},{body, body()},{css,?LOGIN_CSS},{less,?LESS},{js,?LOGIN_BOOTSTRAP}]} ].

body() ->
    index:header() ++ [#section{class=[section], body=[
        #panel{class=[container], body=[
            #panel{class=[modal, fade, in, "modal-login"], body=[
                #panel{class=["modal-dialog"], body=[
                    #panel{class=["modal-content"], body=[
                        #panel{class=["modal-header"], body=[
                            #h3{class=["modal-title"], body=[<<"Log in to your account">>]}]},

                        #panel{class=["modal-body"], role="form", body=[
                            #panel{id=messages, body=[]},
                            #h3{class=["text-center"], body= <<"Sign in with">>},
                            #panel{class=["btn-toolbar", "text-center"], body=[avz:buttons(?LOGIN)]},
                            #h3{class=["text-center"], body= <<"or">>},
                            #panel{class=["form-horizontal"], body=[
                                #panel{class=["form-group"], body=[
                                    #label{class=["control-label","col-sm-3"], for=user, body= <<"Email">>},
                                    #panel{class=["col-sm-7"], body=[
                                        #panel{class=["input-group"], body=[
                                        #span{class=["input-group-addon"], body=[#i{class=[fa,"fa-user", "fa-fw"]}]},
                                        #textbox{id=user, class=["form-control"], placeholder= <<"e-mail">>}]}]}]},

                                #panel{class=["form-group"], body=[
                                    #label{class=["control-label", "col-sm-3"], for=pass, body= <<"Password">>},
                                    #panel{class=["col-sm-7"], body=[
                                        #panel{class=["input-group"], body=[
                                        #span{class=["input-group-addon"],body=[#i{class=[fa,"fa-lock","fa-fw"]}]},
                                        #password{id=pass, class=["form-control"]}]}]}]}]}]},

                        #panel{class=["modal-footer"], body=[avz:buttons([email]) ]} ]} ]} ]} ]} ]}
    ] ++ index:footer() ++ avz:sdk(?LOGIN).

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event(login) -> avz:login(email, [{<<"email">>, list_to_binary(wf:q(user))}, {<<"password">>, wf:q(pass)}]);
event({login, #user{}=User}) ->
    {ok, U} = kvs:get(user, User#user.email),
    case kvs_acl:check_access(User#user.email, {feature,login}) of
    disable ->
        wf:update(messages, index:error(<<"Your account has been blocked.">>));
    _ ->
        Av = case string:str(wf:to_list(U#user.avatar), "static/"++U#user.email) of
            0 -> User#user.avatar; _ -> U#user.avatar end,
        msg:notify([kvs_user, login, user, User#user.email, update_status], {ok}),
        avz:login_user(User#user{avatar=Av}) end;
event({register, #user{}=User}) ->
    msg:notify([kvs_user, user, create], [User#user{feeds=?USR_CHUNK}]);
event(X) -> avz:event(X).

api_event(X,Y,Z) -> avz:api_event(X,Y,Z).

process_delivery([user, created], [#user{}=U]) -> event({login, U});
process_delivery([user, created], [{error,E}]) -> wf:update(messages, index:error(E));
process_delivery(_,_) -> skip.
