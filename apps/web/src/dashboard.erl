-module(dashboard).
-compile(export_all).
-compile({parse_transform, shen}).
-include_lib("n2o/include/wf.hrl").
-include_lib("n2o_bootstrap/include/wf.hrl").
-include_lib("kvs/include/user.hrl").
-include_lib("kvs/include/feeds.hrl").
-include("records.hrl").

-define(PAGES, [
    {profile,       <<"profile">>,          undefined},
    {myreviews,     <<"reviews">>,          feed},
    {mygames,       <<"games">>,            products},
    {direct,        <<"notifications">>,    direct},
    {admin,         <<"admin">>,            undefined}
    ]).

-jsmacro([off_canvas/0]).

off_canvas()->
    jq(fun()-> J = jq("[data-toggle=\"offcanvas\"]"), J:click(fun()->
        J1 =jq(".row-offcanvas"), J1:toggleClass("active") end) end).

sidenav({What, Active, Tabs})->
    Who = wf:user(),
    #list{class=["list-group", "affix-top"], data_fields=?DATA_AFFIX, body=[
    begin
        SubTabs = if Active == Page -> [
            if Tabs /= [] -> #li{class=["list-group-item",divider]}; true-> [] end,
            [#li{class=["list-group-item"], body=[#link{url= "#"++wf:to_list(Id),
                data_fields=?DATA_PILL, body= Label}]} || {Id, Label} <- Tabs]]; true -> [] end,

        Class = if Active == Page -> [active]; true -> [] end,

        PageFeed = lists:keyfind(Feed, 1, if What==undefined->[];true -> What#user.feeds end),
        Badge = if PageFeed /= false -> {_, Fid} = PageFeed,
                    case kvs:get(feed, Fid) of {error,_}-> [];
                    {ok, F}-> #span{class=[label, "label-info"],
                                    body=integer_to_list(F#feed.entries_count)} end; true -> [] end,

        [#li{class=["list-group-item",Class], body=#link{
                url=if SubTabs/=[]->"#"; true -> "/" end ++atom_to_list(Page),
                data_fields=if SubTabs/=[]->?DATA_PILL; true-> [] end,  body=[Title, Badge]}}, SubTabs]
    end || {Page, Title, Feed} <- lists:filter(fun({P,_,_})-> 
            if  What==undefined -> P == Active;
                Who == What -> case P of
                    admin ->        kvs_acl:check_access(What#user.email, {feature, admin})==allow;
                    myreviews ->    kvs_acl:check_access(What#user.email, {feature, reviewer})==allow;
                    mygames ->      kvs_acl:check_access(What#user.email, {feature, developer})==allow;
                _-> true end; true -> P==Active end end, ?PAGES) ]}.

section(Body, Icon)-> section(wf:temp_id(),Body,Icon, "").
section(Id, Body, Icon) -> section(Id, Body,Icon, "").
section(Id, Body, Icon, Class) ->
  #section{class=["row", "dashboard-section", Class], body = [
    #panel{class=["col-xs-1"], body=#h3{body=[#i{class=[fa, Icon]}]}},
    #panel{id=Id, class=["col-xs-11", "dashboard-unit"], body=Body} ]}.

page({_,Type,_}=Nav, Body)->
    wf:wire(off_canvas()),
    [#panel{class=[page], body=[
        index:header(),
        #panel{class=["page-wrapper"], body=[
            #section{class=[container], body=[
                #panel{class=[row, "row-offcanvas", "row-offcanvas-left"], body=[
                    #panel{id=sidenav, class=["col-xs-6", "col-sm-3", "sidebar-offcanvas"], role=navigation,
                        body=[sidenav(Nav)]},
                    #panel{class=["col-xs-12","col-sm-9", "dashboard-feed"], body=Body} ]} ]} ]} ]}];
page(_,_)->[].
