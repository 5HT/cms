-module(direct).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("n2o_bootstrap/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/feeds.hrl").
-include("records.hrl").
-include("states.hrl").

main()-> case wf:user() of undefined -> wf:redirect("/");
    _-> #dtl{file="prod", bindings=[{title,<<"notifications">>},
                                    {body, body()},{css,?DIRECT_CSS},{less,?LESS},{js, ?DIRECT_BOOTSTRAP}]} end.
body()->
    wf:wire(#api{name=tabshow}),
    wf:wire(index:on_shown("'pill'")),

    Nav = {wf:user(), direct, subnav()},
    dashboard:page(Nav,
        #panel{class=["col-sm-9", "tab-content"], body=[
            #panel{id=direct, class=["tab-pane", active], body=feed(direct, false)},
            [#panel{id=Id, class=["tab-pane"]} || Id <- [sent,archive]]] }).

subnav()-> [{sent, "sent"}, {archive, "archive"}].

feed(Feed, Escape)->
    User = wf:user(),
    case lists:keyfind(Feed, 1, element(#iterator.feeds, User)) of false ->
        index:error("No feed "++wf:to_list(Feed));
    {_, Id} ->
        State = case wf:cache({Id,?CTX#context.module}) of undefined ->
            Fs = ?DIRECT_STATE(Id), wf:cache({Id,?CTX#context.module}, Fs), Fs; FS->FS end,

        InFid = case lists:keyfind(sent,1,element(#iterator.feeds,User)) of false -> Id; {_,Fid} -> Fid end,
        InputState = case wf:cache({?FD_INPUT(InFid),?CTX#context.module}) of undefined ->
            Is = ?DIRECT_INPUT(InFid), wf:cache({?FD_INPUT(InFid),?CTX#context.module}, Is), Is; IS->IS end,

        #feed_ui{title=title(Feed),
                 icon=icon(Feed),
                 state=State#feed_state{js_escape=Escape},
                 header=[case Feed of direct ->
                    #input{icon="", state=InputState}; _-> #tr{class=["feed-table-header"]} end],
                 selection_ctl=case Feed of direct -> [
                    #link{class=[btn, "btn-default"], body=#i{class=["fa fa-archive"]},
                    data_fields=?DATA_TOOLTIP, title= <<"archive">>, postback={archive, State}}];_-> [] end } end.

title(sent)-> <<"Sent Messages ">>;
title(direct)-> <<"Notifications ">>;
title(archive)-> <<"Archive ">>;
title(_) -> <<"">>.

icon(sent)-> "fa fa-envelope-o";
icon(direct)-> "fa fa-envelope";
icon(archive) -> "fa fa-archive";
icon(_)-> "".

% Render direct messages

render_element(#div_entry{entry=#entry{}=E, state=#feed_state{view=direct}=State})->
    Id = wf:to_list(erlang:phash2(element(State#feed_state.entry_id, E))),
    User = wf:user(),
    From = case kvs:get(user, E#entry.from) of {ok, U} -> U#user.display_name; {error, _} -> E#entry.from end,
    IsAdmin = case User of undefined -> false; 
        Us when Us#user.email==User#user.email -> true; 
        _-> kvs_acl:check_access(User#user.email, {feature, admin})==allow end,

    wf:render([
        #p{body=[#small{body=["[", index:to_date(E#entry.created), "] "]},
            #link{body= if From == User#user.email -> <<"you">>; true -> From end, url= "/profile?id="++E#entry.from},
            <<" ">>,
            E#entry.title,
            case E#entry.type of {feature, _}-> #b{body=io_lib:format(" ~p", [E#entry.type])}; _-> [] end ]},
        #p{body= E#entry.description},
        #panel{id=?EN_TOOL(Id), body= case E#entry.type of {feature, _} when IsAdmin ->
            #panel{class=["btn-toolbar"], body=[
                #link{class=[btn, "btn-success"], body= <<"allow">>,
                    postback={allow, E#entry.from, E#entry.entry_id, E#entry.type}, delegate=admin},
                #link{class=[btn, "btn-info"], body= <<"reject">>, 
                    postback={cancel, E#entry.from, E#entry.entry_id, E#entry.type}, delegate=admin} ]};
        _ -> [] end }]);
render_element(E)->feed_ui:render_element(E).

% Events

control_event(_, _) -> ok.
api_event(tabshow,Args,_) ->
    [Id|_] = string:tokens(Args,"\"#"),
    wf:info("Show tab ~p", [Id]),
    case list_to_atom(Id) of direct -> ok;
    _-> wf:update(list_to_atom(Id), feed(list_to_atom(Id), true)) end;
api_event(_,_,_) -> ok.

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({archive, #feed_state{selected_key=Selected, visible_key=Visible}}) ->
    Selection = sets:from_list(wf:cache(Selected)),
    User = wf:user(),
    case lists:keyfind(archive, 1, User#user.feeds) of false -> ok;
    {_,Fid} ->
        [case kvs:get(entry, Id) of {error,_} -> ok;
        {ok, E} ->
            msg:notify( [kvs_feed, user, User#user.email, entry, Eid, add],
                        [E#entry{id={Eid, Fid}, feed_id=Fid}]),

            msg:notify( [kvs_feed, User#user.email, entry, delete], [E])
        end || {Eid,_}=Id <- wf:cache(Visible), sets:is_element(wf:to_list(erlang:phash2(Id)), Selection)] end;
event(_) -> ok.

process_delivery(R,M) ->
    wf:update(sidenav, dashboard:sidenav({wf:user(), direct, subnav()})),
    feed_ui:process_delivery(R,M).
