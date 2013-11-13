-module(myreviews).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("n2o_bootstrap/include/wf.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("kvs/include/users.hrl").
-include("records.hrl").
-include("states.hrl").

title() -> <<"My reviews">>.

main()-> #dtl{file="prod", bindings=[{title, title()},{body, body()},
                                     {css,?MYREVIEW_CSS},{less,?LESS},{js, ?MYREVIEW_BOOTSTRAP}]}.
-record(editor, {?ELEMENT_BASE(myreviews)}).

body()->
    User = wf:user(),
    Nav = {User, myreviews, []},
    Feeds = case User of undefined -> []; _-> element(#iterator.feeds, User) end,

    dashboard:page(Nav,
        case lists:keyfind(feed, 1, Feeds) of false -> [];
        {_,Id}->
            FeedState = case wf:cache({Id,?CTX#context.module}) of undefined ->
                Fs = ?MYREVIEWS_FEED(Id), wf:cache({Id,?CTX#context.module}, Fs),Fs; FS -> FS end,
            InputState = case wf:cache({?FD_INPUT(Id),?CTX#context.module}) of undefined -> 
                Is = ?MYREVIEWS_INPUT(Id), wf:cache({?FD_INPUT(Id),?CTX#context.module}, Is), Is; IS-> IS end,

            #feed_ui{title= title(),
                icon="fa fa-list",
                state=FeedState,
                header=[#input{state=InputState} ]} end).

%% Render review elements

render_element(#div_entry{entry=#entry{entry_id=Eid}=E, state=#feed_state{view=review}=State})->
    Id = element(State#feed_state.entry_id, E),
    Fid = State#feed_state.container_id,
    UiId = wf:to_list(erlang:phash2(Id)),
    FromId = E#entry.from,
    From = case kvs:get(user, E#entry.from) of
        {ok, User} -> User#user.display_name;
        {error, _} -> E#entry.from end,

    InputState = (wf:cache({?FD_INPUT(Fid),?CTX#context.module}))#input_state{update=true},

    wf:render([#panel{class=["col-sm-3", "article-meta"], body=[
        #h3{body= <<"">>},
        #p{class=[username], body= #link{body=From, url=?URL_PROFILE(FromId)}},
        #panel{body= index:to_date(E#entry.created)},
        #p{body=[
            #link{url="#",body=[#span{class=[?EN_CM_COUNT(UiId)],
                body= integer_to_list(kvs_feed:comments_count(entry, Id))},
                #i{class=["icon-comment-alt", "icon-2x"]} ]} ]}]},

        #panel{id=?EN_MEDIA(UiId), class=["col-sm-4", "media-pic"],
            body=#entry_media{media=E#entry.media, mode=reviews}},

        #panel{class=["col-sm-4", "article-text"], body=[
            #h3{body=#span{id=?EN_TITLE(UiId), class=[title], body=
                #link{style="color:#9b9c9e;", body=E#entry.title, url=?URL_REVIEW(Eid)}}},
            #p{id=?EN_DESC(UiId), body=index:shorten(E#entry.description)}
        ]},

        #panel{id=?EN_TOOL(UiId), class=["col-sm-1"], body=[
            #link{body= <<"edit">>, class=[btn, "btn-default", "btn-block"], delegate=input, postback={edit, E, InputState}},
            #link{body= <<"more">>, class=[btn, "btn-default", "btn-block"], url=?URL_REVIEW(Eid)} ]}

    ]);

render_element(#editor{}=R)->
    wf:wire(""),
    wf_tags:emit_tag(<<"div">>,[
    {<<"id">>, R#editor.id},
    {<<"style">>, R#editor.style},
    {<<"class">>, R#editor.class} | R#editor.data_fields]);

render_element(E)-> feed_ui:render_element(E).


event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event(_) -> ok.

process_delivery(R,M) ->
    wf:update(sidenav, dashboard:sidenav({wf:user(), myreviews, []})),
    feed_ui:process_delivery(R,M).
