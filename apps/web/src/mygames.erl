-module(mygames).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("n2o_bootstrap/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("kvs/include/membership.hrl").
-include("records.hrl").
-include("states.hrl").

main()-> #dtl{file="prod", bindings=[{body,body()},{css,?MYGAMES_CSS},{less,?LESS},{js,?MYGAMES_BOOTSTRAP}]}.

navbar() -> profile:navbar().

body()->
    User = wf:user(),
    Nav = {User, mygames, []},
    Feeds = case User of undefined -> []; _-> element(#iterator.feeds, User) end,

    dashboard:page(Nav,
        case lists:keyfind(products, 1, Feeds) of false -> [];
        {_, Id} ->
            FeedState = case wf:cache({Id,?CTX#context.module}) of undefined ->
                FS = ?MYGAMES_FEED(Id), wf:cache({Id,?CTX#context.module}, FS), FS; FD -> FD end,

            InputState = case wf:cache({?FD_INPUT(Id),?CTX#context.module}) of undefined ->
                IS = ?MYGAMES_INPUT(Id), wf:cache({?FD_INPUT(Id),?CTX#context.module}, IS), IS; IS -> IS end,

            #feed_ui{title= <<"products">>,
                     state=FeedState,
                     header=[ #input{state=InputState} ]} end).

%% Render feed items

render_element(#div_entry{entry=#entry{id={Eid,_}}=E, state=#feed_state{view=product}=State}) ->
    Id = element(State#feed_state.entry_id, E),
    case kvs:get(product, Eid) of {error,_}-> wf:render(["no product"]);
    {ok, P} ->
        Fid = State#feed_state.container_id,
        UiId = wf:to_list(erlang:phash2(element(State#feed_state.entry_id, E))),
        From = case kvs:get(user, E#entry.from) of 
                    {ok, User} -> User#user.display_name;
                    {error, _} -> E#entry.from end,
        InputState = (wf:cache({?FD_INPUT(Fid),?CTX#context.module}))#input_state{update=true},

        wf:render([#panel{class=["col-sm-3", "article-meta"], body=[
            #h4{class=[blue], body= <<"">>},
            #p{body=#link{body=From, url= "/profile?id="++wf:to_list(E#entry.from)}},
            #p{body=[#span{body= index:to_date(E#entry.created)} ]},
            #p{body=[
                #span{class=[?EN_CM_COUNT(UiId)], body=integer_to_list(kvs_feed:comments_count(product, Eid))},
                #i{class=["fa fa-comment-alt", "fa-lg"]},

                #i{class=["fa fa-usd", "fa-large"]},
                #b{body=float_to_list(P#product.price/100, [{decimals, 2}])}
            ]} ]},

            #panel{id=?EN_MEDIA(UiId), class=["col-sm-4","media-pic"],
                   body=#entry_media{media=E#entry.media, mode=reviews}},

            #panel{class=["col-sm-4", "article-text"], body=[
                #h3{body=#span{id=?EN_TITLE(UiId), class=[title], body=
                    #link{style="color:#9b9c9e;", url=?URL_PRODUCT(Id), body=E#entry.title}}},
                #p{id=?EN_DESC(UiId), body=E#entry.description}
            ]},
            #panel{class=["col-sm-1"], body=[
                #link{body= <<"edit">>, class=[btn, "btn-default","btn-block"], delegate=input,
                      postback={edit, P, InputState}},
                #link{body= <<"more">>, class=[btn, "btn-default","btn-block"], url=?URL_PRODUCT(Id)} ]} ]) end;
render_element(E)-> feed_ui:render_element(E).

%% Events

event(init) -> wf:reg(?MAIN_CH), [];
event({counter,C}) -> wf:update(onlinenumber,wf:to_list(C));
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event(_) -> ok.

process_delivery(R,M) ->
    wf:update(sidenav, dashboard:sidenav({wf:user(), mygames, []})),
    feed_ui:process_delivery(R,M).
