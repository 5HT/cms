-module(cart).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("n2o_bootstrap/include/wf.hrl").
-include_lib("kvs/include/payments.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include("records.hrl").
-include("states.hrl").
-include("paypal.hrl").

main() -> #dtl{file="prod", bindings=[{title,<<"shopping cart">>},
                                      {body, body()},{css,?CART_CSS},{less,?LESS},{js, ?CART_BOOTSTRAP}]}.

body()->
    case wf:user() of undefined -> wf:redirect("/login");
    User ->
    State = case lists:keyfind(cart, 1, element(#iterator.feeds, User)) of false -> undefined;
        {_, Cid} -> case wf:cache({Cid, ?CTX#context.module}) of undefined ->
            CS = ?CART_STATE(Cid), wf:cache({Cid, ?CTX#context.module},CS), CS; Cs-> Cs end end,

    WishState = case lists:keyfind(wishlist, 1, element(#iterator.feeds, User)) of
        false -> undefined;
        {_, Wid} -> case wf:cache({Wid, ?CTX#context.module}) of 
            undefined -> Ws = ?CART_STATE(Wid)#feed_state{view=store},
                         wf:cache({Wid, ?CTX#context.module}, Ws), Ws; WS-> WS end end,

    index:header() ++ [
    #section{class=[section], body=[
        #panel{class=[container], body=[
            #h4{class=["row", "page-header-sm"], body=[
                #link{class=?BTN_INFO, body= <<"continune shopping">>, url="/store"},
                #small{id=alert, body=case wf:qs(<<"token">>) of undefined -> <<"">>; Tk ->
                    case paypal:get_express_details([{"TOKEN", Tk}]) of {error,E} ->
                        index:alert_inline("payment " ++ proplists:get_value(?PP_TRANSACTION, E) 
                            ++" "++ proplists:get_value(?PP_ACK, E)
                            ++ " "++ proplists:get_value(?PP_ERROR_MSG,E));
                    Details ->
                        CorrelationId = proplists:get_value(?PP_TRANSACTION, Details),
                        CheckoutStatus = proplists:get_value(?PP_STATUS, Details),
                        index:alert_inline("payment " ++ CorrelationId ++ " status:" ++CheckoutStatus) end end}]},

            #panel{class=["row"], body=[
                #panel{class=["col-sm-9"], body=[
                    #feed_ui{title= <<"shopping cart">>,
                            icon="fa fa-shopping-cart fa-lg",
                            state=State},

                    #panel{class=["hero-unit", "clearfix"], body= <<"">>},
                        #feed_ui{title= <<"whish list">>,
                            icon="fa fa-list fa-lg",
                            state=WishState,
                            header=[]}]},

                #panel{id=?USR_ORDER(State#feed_state.container_id), class=["col-sm-3"], body=order_summary(State)} ]}]}]}
    ] ++ index:footer() end.

order_summary(S)-> order_summary(S,false).

order_summary(#feed_state{visible_key=Visible}=S, Escape) ->
    case wf:cache(Visible) of [] ->
        case kvs:get(S#feed_state.container, S#feed_state.container_id) of {error,_} -> ok;
            {ok, Feed} -> Entries = kvs:entries(Feed, S#feed_state.entry_type, S#feed_state.page_size),
                wf:cache(Visible, [element(S#feed_state.entry_id, E)|| E<-Entries]) end; _-> ok end,

    {Items, Total} = lists:mapfoldl(fun({Id,_}, In)->
        case kvs:get(product,Id) of {error,_} -> {[], In};
            {ok, #product{price=Price, title=Title}} -> {
                [#panel{body=[#b{body=if Escape -> wf:js_escape(Title);true -> Title end},
                    #span{class=["pull-right"], body=[
                    #span{class=["fa fa-usd"]},
                    float_to_list(Price/100, [{decimals,2}]) ]}]}
                ], In+Price} end end,
        0, [Pid || Pid <- ordsets:from_list(case wf:cache(Visible) of undefined->[];I->I end)]),

    #panel{class=[well, "pricing-table", "affix-top"],
           style="width:230px",
           data_fields=[{<<"data-spy">>, <<"affix">>}],
           body=[
        #h4{class=["text-warning", "text-center"], body= <<"Order Summary">>},
        #hr{},
        Items,
        if Total > 0 -> #hr{}; true -> [] end,
        #panel{body=[
            #b{body= <<"Estimated total: ">>},
            #span{class=["pull-right"],
                  body=[#i{class=["fa fa-usd"]}, float_to_list(Total/100, [{decimals,2}])]} ]},
        #hr{},
        #link{class=[btn, "btn-default", "btn-block", if Total == 0 -> disabled; true -> "" end],
            postback={checkout, Visible},
            body=[#image{image="https://www.paypal.com/en_US/i/btn/btn_xpressCheckout.gif"}]} ,
        #hr{},
        #panel{class=[alert,"alert-block", "alert-warning"], body=[
            #p{body= <<"WARNING!">>},
            #p{body= <<"PayPal chekout is in the sandbox mode! ">>},
            #p{body = <<"You can buy anything using following account:">>},
            #panel{body= <<"Login:">>},
            #strong{body = <<"buyer@igratch.com">>},
            #panel{body= <<"Password:">>},
            #strong{body= <<"buyerigratch">>}
        ]}
    ]};
order_summary(undefined,_)->[].

%% Render elements

render_element(#div_entry{entry=#entry{}=E, state=#feed_state{view=cart}=State}) ->
    wf:render(case kvs:get(product, E#entry.entry_id) of
    {ok, P} ->
        Id = wf:to_list(erlang:phash2(element(State#feed_state.entry_id, E))),
        error_logger:info_msg("Id: ~p", [Id]),
        [#panel{id=?EN_MEDIA(Id), class=["col-sm-4", "media-pic"], style="margin:0;",
            body=#entry_media{media=input:media(P#product.cover), mode=store}},

        #panel{class=["col-sm-5", "article-text"], body=[
            #h3{body=#span{id=?EN_TITLE(Id), class=[title], body=
                #link{style="color:#9b9c9e;", body=P#product.title, url=?URL_PRODUCT(P#product.id)}}},

            #p{id=?EN_DESC(Id), body=index:shorten(P#product.brief)} ]},

        #panel{class=["col-sm-3", "text-center"], body=[
            #h3{style="",
                body=[#span{class=["fa fa-usd"]}, float_to_list(P#product.price/100, [{decimals, 2}]) ]},
                #link{class=?BTN_INFO, body= <<"to wishlist">>, postback={to_wishlist, P, State}} ]}];

    {error,_} -> <<"item not available">> end);
render_element(E)-> store:render_element(E).

% Events

event(init) -> wf:reg(?MAIN_CH),[];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);

event({to_wishlist, #product{}=P, #feed_state{}=S})->
    case kvs:get(entry, {P#product.id, S#feed_state.container_id}) of {error,_}-> ok;
    {ok, E} ->
        User = wf:user(),
        Is = #input_state{
            collect_msg = false,
            show_recipients = false,
            entry_type = wishlist,
            entry_id = P#product.id,
            title = P#product.title,
            description = P#product.brief,
            medias=[input:media(P#product.cover)]},

            input:event({post, wishlist, Is}),

            msg:notify([kvs_feed, User#user.email, entry, delete], [E]) end;

event({add_cart, #product{}=P}=M) ->
    store:event(M),
    User = wf:user(),
    case lists:keyfind(wishlist, 1, User#user.feeds) of false -> ok;
    {_,Fid} ->
        case kvs:get(entry, {P#product.id, Fid}) of {error,_}-> ok;
        {ok, E} -> msg:notify([kvs_feed, User#user.email, entry, delete], [E]) end end;

event({checkout, Visible}) ->
    User = wf:user(),
    {Req, {Total,_}} = lists:mapfoldl(fun({Id,_}, {T, In})->
        case kvs:get(product,Id) of {error,_} -> {[], {T, In}};
            {ok, #product{price=Price}=P} ->
                Index = integer_to_list(In),
                PmId = kvs_payment:payment_id(),
                Pm = #payment{id=PmId,
                        user_id = User#user.email,
                        product_id=P#product.id,
                        product = P,
                        payment_type = paypal},
                msg:notify([kvs_payment, user, User#user.email, add], {Pm}),

                {{PmId, paypal:product_request(Index,PmId,P)}, {T+Price,In+1}} end end,
        {0,0}, [I || I <- ordsets:from_list(wf:cache(Visible))]),

    {Ids, Req1} = lists:unzip(Req),
    case paypal:set_express_checkout(lists:flatten(?PP_PAYMENTREQUEST(Total)++Req1)) of
        {error,E} ->
            [begin
                 msg:notify([kvs_payment, user, User#user.email, set_state], {I, failed, {E}}),
                 msg:notify([kvs_payment, user, User#user.email, set_external_id],
                            {I, proplists:get_value("CORRELATIONID", Req1)})
            end || I <- Ids],
            wf:update(alert, index:alert_inline(wf:js_escape(wf:to_list(E))));_-> ok end;

event(_) -> ok.

process_delivery([entry, {_,Fid}, _]=R, [#entry{}|_]=M)->
    User = wf:user(),
    feed_ui:process_delivery(R,M),
    case feed_ui:feed_state(Fid) of false -> ok;
    State ->
        wf:update(?USR_ORDER(State#feed_state.container_id), order_summary(State, true)),
        case lists:keyfind(cart, 1, User#user.feeds) of false -> ok;
        {_,CFid} when Fid == CFid ->
            case kvs:get(feed,Fid) of
                {ok, #feed{entries_count=C}} when C == 0 -> wf:update(?USR_CART(User#user.id), "");
                {ok, #feed{entries_count=C}} -> wf:update(?USR_CART(User#user.id), integer_to_list(C));
                _ -> ok end;
        _ -> ok end end;

process_delivery(R,M) -> feed_ui:process_delivery(R,M).
