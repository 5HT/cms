-module(reviews).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("n2o_bootstrap/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include("records.hrl").
-include("states.hrl").

main()-> #dtl{file="prod", bindings=[{title,<<"reviews">>}, {body, body()},
                                    {css, ?REVIEWS_CSS},{less, ?LESS},{js, ?REVIEWS_BOOTSTRAP}]}.

body()->
    wf:wire(#api{name=tabshow}),
    wf:wire(index:on_shown("'tab'")),

    Groups = lists:flatmap(fun(#group{scope=Scope, feeds=Feeds, name=Name})->
        case lists:keyfind(feed,1, Feeds) of
        {_,Fid} when Scope==public ->
            case wf:cache({Fid,?CTX#context.module}) of 
                 undefined -> wf:cache({Fid,?CTX#context.module}, ?REVIEWS_FEED(Fid)), [{Name, Fid}];
                         _ -> [{Name,Fid}] end; _ -> [] end end, kvs:all(group)),

    All = case wf:cache({?FEED(entry),?CTX#context.module}) of undefined ->
        FS = ?ENTRIES_FEED, wf:cache({?FEED(entry),?CTX#context.module},FS), FS; F->F end,

    index:header() ++ [
    #section{class=[section], body=[
        #panel{class=[container], body=[
            #h4{class=["col-sm-12", "page-header-sm"], body=[
                #link{url="#all", body=[#i{class=[fa, "fa-home", "fa-lg", "text-warning"]}],
                                           data_fields=?DATA_TAB},
                #small{body= string:join([wf:to_list(wf:render(
                    #link{url="#"++wf:to_list(Fid),body=[#i{class=[fa, "fa-asterisk"]}, Name],
                          data_fields=?DATA_TAB})) || {Name,Fid} <- Groups], " / ")} ]},

            #panel{class=[row], body=[
                #panel{class=["col-sm-9", "tab-content"], body=[
                    #panel{id=all, class=["tab-pane", active], body=[
                        #feed_ui{icon=[fa, "fa-tags ", "fa-lg ", "text-warning"], state=All} ]},

                    [#panel{id=wf:to_list(Fid), class=["tab-pane"]}|| {_,Fid} <- Groups] ]},
                #panel{class=["col-sm-3"], body=[<<"">>]} ]} ]}]} ] ++ index:footer().

%% Render review elements

render_element(#div_entry{entry=#entry{entry_id=Eid}=E, state=#feed_state{view=review}=State})->
    Id = element(State#feed_state.entry_id, E),
    UiId = wf:to_list(erlang:phash2(element(State#feed_state.entry_id, E))),
    {FromId, From} = case kvs:get(user, E#entry.from) of 
                          {ok, User} -> {E#entry.from, User#user.display_name};
                          {error, _} -> {E#entry.from,E#entry.from} end,
    wf:render([#panel{class=["col-sm-3", "article-meta"], body=[
        #h3{class=[blue], body= <<"">>},
        #p{class=[username], body= #link{body=From, url= "/profile?id="++wf:to_list(FromId)}},
        #panel{body= index:to_date(E#entry.created)},
        #p{body=[
            #link{url=?URL_REVIEW(Eid),body=[#span{class=[?EN_CM_COUNT(UiId)],
                body= integer_to_list(kvs_feed:comments_count(entry, Id))},
                #i{class=[fa,"fa-comment-alt", "fa-2x"]} ]} ]}]},

        #panel{id=?EN_MEDIA(UiId), class=["col-sm-4", "media-pic"],
            body=#entry_media{media=E#entry.media, mode=reviews}},

        #panel{class=["col-sm-5", "article-text"], body=[
            #h3{body=#span{id=?EN_TITLE(UiId), class=[title], body=
                #link{style="color:#9b9c9e;", body=E#entry.title, url=?URL_REVIEW(Eid)}}},

            #p{id=?EN_DESC(UiId), body=index:shorten(E#entry.description)},
            #panel{id=?EN_TOOL(UiId), class=[more], body=[
                #link{body=[<<"read more">>], url=?URL_REVIEW(Eid)} ]}]}]);

render_element(E)-> feed_ui:render_element(E).

% Events 

api_event(tabshow,Args,_) ->
    [Id|_] = string:tokens(Args,"\"#"),
    case Id of "all" -> []; _ -> wf:update(Id, index:feed(list_to_integer(Id))) end,
    wf:wire("Holder.run();").

event(init) -> wf:reg(?MAIN_CH),[];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event(_) -> ok.

process_delivery(R,M) -> feed_ui:process_delivery(R,M).
