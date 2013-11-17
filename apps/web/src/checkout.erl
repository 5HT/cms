-module(checkout).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("n2o_bootstrap/include/wf.hrl").
-include_lib("kvs/include/payments.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/feeds.hrl").
-include("paypal.hrl").
-include("records.hrl").
-include("states.hrl").

main()-> #dtl{file="prod", bindings=[{title,<<"Checkout">>},
                                     {body, body()},{css,?CHECKOUT_CSS},{less,?LESS},{js,?CHECKOUT_BOOTSTRAP}]}.

body() ->
    error_logger:info_msg("Token~p", [wf:qs(<<"token">>)]),
    Token = wf:qs(<<"token">>),
    PayerId = wf:qs(<<"PayerID">>),

    index:header() ++ [
    #section{class=[section], body=[
        #panel{class=[container], body=[
            #h4{class=["row", "page-header-sm"], body=[#small{id=alert, body= <<"">>}]},
            case Token of undefined ->
                error_logger:info_msg("[checkout]No token defined"),
                wf:update(alert, cart:alert("No token defined")),
                [];
            _ when PayerId /= undefined ->
                case paypal:get_express_details([{"TOKEN", Token}]) of {error, E}-> 
                    wf:update(alert, cart:alert(E)),[];
                Details ->
                    wf:info("The order details ~p", [Details]),
                    #panel{class=["well", "pricing-table"], body=[
                        #h4{class=["text-warning", "text-center"], body= [
                            <<"Receipt: ">>, 
                            proplists:get_value("CORRELATIONID", Details),
                            <<" time: ">>,
                            proplists:get_value("TIMESTAMP", Details) ]},
                        #hr{},
                        #panel{class=[], body=[
                            #h4{body= <<"Buyer:">>},
                            #panel{body= proplists:get_value("EMAIL", Details)},
                            #panel{body= proplists:get_value("FIRSTNAME", Details)},
                            #panel{body= proplists:get_value("LASTNAME", Details)}
                        ]},
                        #hr{},
                        lists:flatmap(fun({K,_})->
                            case string:str(K,"L_PAYMENTREQUEST_0_NUMBER") of 0 -> [];
                            Pos -> Index = string:substr(K, Pos+length("L_PAYMENTREQUEST_0_NUMBER")),
                                Title = proplists:get_value("L_PAYMENTREQUEST_0_NAME"++Index, Details),
                                Price = proplists:get_value("L_PAYMENTREQUEST_0_AMT"++Index, Details),
                                [#panel{body=[#b{body=Title}, #span{class=["pull-right"], body=[
                                    #span{class=["icon-usd"]}, Price ]}]}, #hr{}]
                            end
                        end, Details),
                        #panel{body=[
                            #b{body= <<"Estimated total: ">>},
                            #span{class=["pull-right"],
                                body=[#i{class=["icon-usd"]}, proplists:get_value("AMT", Details)]} ]},
                        #hr{},
                        #panel{class=["btn-toolbar", "text-center"], body=[
                            #link{class=[btn, "btn-warning"], body= <<"pay now">>, postback={buy,
                                [{"PAYMENTREQUEST_0_PAYMENTACTION", "Sale"},
                                {"PAYERID", PayerId},
                                {"TOKEN",   Token},
                                {"PAYMENTREQUEST_0_AMT", proplists:get_value("PAYMENTREQUEST_0_AMT", Details)}],
                                            Details}
                            },
                            #link{class=[btn], body= <<"cancel">>, postback={cancel, Token}}]}
                    ]}
                end;
            _ ->
                error_logger:info_msg("[checkout]PayerId not defined, but token specified~p", [Token]),
                [] end
     ]} ]}] ++ index:footer().

% Events

api_event(Name,Tag,Term) -> error_logger:info_msg("Name ~p, Tag ~p, Term ~p",[Name,Tag,Term]).

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({buy, Params, Order}) ->
    wf:info("BUY ~p", [Params]),
    User = wf:user(),
    case paypal:do_express_checkout(Params) of {error, E} ->
        wf:update(alert, cart:alert(E)), ok;
    Details ->
        T1 = wf:to_list(proplists:get_value("TOKEN", Params)),
        T2 = wf:to_list(proplists:get_value("TOKEN", Details)),
        if T1 == T2 ->
            case paypal:get_express_details([{"TOKEN", T2}]) of {error, E} ->
                error_logger:info_msg("[checkout]Error geting the details:~p", [E]),
                wf:update(alert, cart:alert(E)),
                {error,E};
            Status ->
                CheckoutStatus = proplists:get_value("CHECKOUTSTATUS", Status),
                if CheckoutStatus == "PaymentActionCompleted" ->
%                    CorrellationId = proplists:get_value("CORRELATIONID", Status),
                    Feed = lists:keyfind(cart,1,User#user.feeds),
                    [case string:str(K,"L_PAYMENTREQUEST_0_NUMBER") of 0 -> ok;
                        _ -> case kvs:get(payment, V) of {error,_} -> ok;
                            {ok,#payment{product_id=Pid}} ->
                                case Feed of false -> ok;
                                {_,Fid} ->
                                    msg:notify( [kvs_feed, User#user.email, entry, delete],
                                                [#entry{id={Pid,Fid}, entry_id=Pid, feed_id=Fid}]) end,
                                msg:notify([kvs_payment, user, User#user.email, set_state],
                                                 {V, done, paypal}) end end || {K,V} <- Order],
                    wf:redirect("/profile");
                true ->
                    wf:info("[checkout] Fail status: ~p", [Status]),
                    wf:update(alert, cart:alert(Status)) end end;
        true ->
            wf:info("[checkout] Fail, tokens doesn't match."),
            wf:update(alert, cart:alert("tokens doesn't match")) end end;

event({cancel, Token}) -> wf:redirect("/cart?token="++wf:to_list(Token));
event({counter,C}) -> wf:update(onlinenumber,wf:to_list(C));
event(_) -> ok.

process_delivery(_R, _M) -> skip.


