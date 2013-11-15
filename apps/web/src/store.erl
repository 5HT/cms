-module(store).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("n2o_bootstrap/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include("records.hrl").
-include("states.hrl").

main()->#dtl{file="prod", bindings=[{title,<<"Store">>},{body,body()},
                                    {css,?STORE_CSS},{less,?LESS},{js, ?STORE_BOOTSTRAP}]}.

body()->
    wf:wire(#api{name=tabshow}),
    wf:wire(index:on_shown("'tab'")),

    Groups = lists:flatmap(fun(#group{scope=Scope, feeds=Feeds, name=Name})->
        case lists:keyfind(products,1, Feeds) of
        {_,Fid} when Scope==public ->
            case wf:cache({Fid,?CTX#context.module}) of 
                 undefined -> wf:cache({Fid,?CTX#context.module}, ?STORE_FEED(Fid)), [{Name, Fid}];
                         _ -> [{Name,Fid}] end; _ -> [] end end, kvs:all(group)),

    All = case wf:cache({?FEED(product),?CTX#context.module}) 
               of undefined -> FS = ?PRODUCTS_FEED, wf:cache({?FEED(product),?CTX#context.module},FS), FS;
                          F -> F end,

    [#panel{class=[page], body=[
        index:header(),
        #panel{class=["page-wrapper"], body=[
            #section{class=[section], body=[
                #panel{class=[container], body=[
                        #h1{class=["page-header"],body=[
                            #link{url="#all", body= <<"Categories">>, data_fields=?DATA_TAB},
                            #small{body= string:join([wf:to_list(wf:render(
                                #link{url="#"++wf:to_list(Fid),body=Name, data_fields=?DATA_TAB})) || {Name,Fid} <- Groups], " / ")}
                        ]},

                    #panel{class=["tab-content"], body=[
                        #panel{id=all, class=["tab-pane", active, products, "items-list"], body=[
                            #feed_ui{icon=[fa, "fa-home ", "fa-lg ", "text-warning"], icon_url="#all", state=All}
                        ]},
                        [#panel{id=wf:to_list(Fid), class=["tab-pane"]}|| {_,Fid} <- Groups]]}
                ]} ]},

            #section{class=[section, alt, "pattern-45-degree-fabric"], body=[
                #panel{class=[container], body=[
                    #panel{class=[jumbotron, "text-center"], body=[
                        #h1{body= <<"Got a question?">>},
                        #p{body= <<"want to work with us to move your bussines to the next level? Well, dont be afraid">>},
                        #button{class=[btn, "btn-lg", "btn-info"], body= <<"contact us">>} ]} ]} ]}

        ]}]}] ++ index:footer().

%% Render store elements
render_element(#div_entry{entry=#entry{}=E, state=#feed_state{view=store}=State}) ->
    case kvs:get(product, E#entry.entry_id) of
        {error, _} -> wf:render(#panel{body= <<"error displaying item">>});
        {ok, P} -> store_element(wf:to_list(erlang:phash2(element(State#feed_state.entry_id, P))), P) end;

render_element(#div_entry{entry=#product{}=P, state=#feed_state{view=store}}) ->
    store_element(wf:to_list(erlang:phash2(element(#product.id, P))), P);

render_element(#entry_media{media=[#media{}=Media|_]}) ->
    I = feed_ui:image(Media, "370x250"),
    element_image:render_element(I);

render_element(E)-> error_logger:info_msg("[store] render -> feed_ui"),feed_ui:render_element(E).

store_element(Id, P) ->
    Media = input:media(P#product.cover),

    {FromId, From} = case kvs:get(user, P#product.owner) of
        {ok, U} -> {P#product.owner, U#user.display_name};
        {error, _} -> {P#product.owner,P#product.owner} end,

    wf:render([
        #link{body=
            #figure{class=[product, "thumbnail-figure"], body=[
                #panel{id=?EN_MEDIA(Id), style= <<"width:370px;height:250px;">>, body=#entry_media{media=Media, mode=store, module=store}},
                #figcaption{class=["product-caption"], body=[
                    #panel{class=["product-title", "thumbnail-title" ], body=[
                        #h3{id=?EN_TITLE(Id), body=#span{body=P#product.title}},
                        #p{id=?EN_DESC(Id), body=index:shorten(P#product.brief)} ]},
                    #span{class=[badges],body=[
                        #p{body=[#link{url="#",body=[
                            #span{class=[?EN_CM_COUNT(Id)], body=
                                integer_to_list(kvs_feed:comments_count(product, P#product.id))},
                            #i{class=[fa, "fa-comment-alt", "fa-2x"]} ]} ]} ]}
                ]},

                #panel{class=["well","pricing-table", "product-price", span3, "text-center"], body=[
                    #h2{class=["pricing-table-price", "product-price-price"], body=[
                        #span{class=[fa,"fa-usd"]}, float_to_list(P#product.price/100, [{decimals, 2}]) 
                    ]},
                    #button{class=[btn, "btn-large"], body= <<"add to cart">>, postback={add_cart, P}}
                ]}
            ]}
        }
    ]).


% Events

api_event(tabshow,Args,_) ->
    [Id|_] = string:tokens(Args,"\"#"),
    case Id of "all" -> []; _ -> wf:update(Id, index:feed(list_to_integer(Id))) end,
    wf:wire("Holder.run();").

event(init) -> wf:reg(?MAIN_CH),[];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({add_cart, #product{}=P}) ->
    case wf:user() of undefined -> wf:redirect("/login");
    _->
        Is = #input_state{
            collect_msg = false,
            show_recipients = false,
            entry_type = cart,
            entry_id = P#product.id,
            title = P#product.title,
            description = P#product.brief,
            medias=[input:media(P#product.cover)]},

        input:event({post, cart, Is}) end;

event(Event) -> error_logger:info_msg("[store]Page event: ~p", [Event]), ok.

process_delivery(R,M) ->
    User = wf:user(),
    case lists:keyfind(cart, 1, User#user.feeds) of false -> ok;
    {_, Id} -> case kvs:get(feed,Id) of {error,_}-> ok;
        {ok, #feed{entries_count=C}} when C==0 -> ok;
        {ok, #feed{entries_count=C}}-> wf:update(?USR_CART(User#user.id), integer_to_list(C)) end end,
    feed_ui:process_delivery(R,M).

short_date(undefined) -> short_date(now());
short_date(Date) ->
    {{Y, M, D}, {_,_,_}} = calendar:now_to_datetime(Date),
    io_lib:format("~s ~p, ~p", [?MONTH(M), D, Y]).
