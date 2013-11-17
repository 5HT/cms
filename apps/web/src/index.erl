-module(index).
-compile(export_all).
-compile({parse_transform, shen}).
-include_lib("n2o/include/wf.hrl").
-include_lib("n2o_bootstrap/include/wf.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/products.hrl").
-include("records.hrl").
-include("states.hrl").

-jsmacro([on_shown/1]).

on_shown(Type) ->
    X = jq("a[data-toggle=\""+Type+"\"]"),
    X:on("shown.bs.tab", fun(E) -> T = jq(E:at("target")), tabshow(T:attr("href")) end).

main() -> #dtl{file = "prod", ext="dtl", bindings=[ {title, <<"esprit">>},
                                                    {body, body()},
                                                    {css,?INDEX_CSS},{less,?LESS},{js,?INDEX_BOOTSTRAP}]}.
body() ->
    wf:wire(#api{name=tabshow}),
    wf:wire("$('a[data-toggle=\"tab\"]').on('shown.bs.tab', function(e){"
        "id=$(e.target).attr('href');"
        "if(id!='#all')$('a[href=\"#all\"').removeClass('text-warning');"
        "else $(e.target).parent().parent().find('.text-warning').removeClass('text-warning');"
        "$(e.target).addClass('text-warning').siblings().removeClass('text-warning');"
        "tabshow(id);});"),

    Groups = lists:flatmap(fun(#group{scope=Scope, feeds=Feeds, name=Name})->
        case lists:keyfind(feed,1, Feeds) of
        {_,Fid} when Scope==public ->
            case wf:cache({Fid,?CTX#context.module}) of 
                 undefined -> wf:cache({Fid,?CTX#context.module}, ?REVIEWS_FEED(Fid)), [{Name, Fid}];
                         _ -> [{Name,Fid}] end; _ -> [] end end, kvs:all(group)),

    All = case wf:cache({?FEED(entry),?CTX#context.module}) of undefined ->
        FS = ?ENTRIES_FEED, wf:cache({?FEED(entry),?CTX#context.module},FS), FS; F->F end,
    Discus = case wf:cache({?FEED(comment),?CTX#context.module}) of undefined ->
        AS= ?ACTIVE_FEED, wf:cache({?FEED(comment),?CTX#context.module}, AS),AS; A->A end,

    [
    #panel{class=[page], body=[
        header(),
        #panel{class=["page-wrapper"],body=[
            #section{class=[container, featured], body=#panel{id=carousel, class=[container], body=featured()}},
            #section{class=[container], body=[
                #h3{body=[
                    string:join([ [#link{url="#"++wf:to_list(F), body=N,data_fields=?DATA_TAB}] || {N,F} <- Groups ]," "),
                    " or ",
                    #link{url="#all",body= <<"Everything">>,data_fields=?DATA_TAB}, " ",
                    #link{url="#mine",body= <<"Mine">>,data_fields=?DATA_TAB} ]},
                #panel{class=["col-md-8", "tab-content"], body=[
                    #panel{id=all, class=["tab-pane", active], body=[
                        #feed_ui{title= <<"">>, icon=[fa, "fa-tags "], state=All#feed_state{delegate=index}}]},
                    [#panel{id=wf:to_list(Fid), class=["tab-pane"]}|| {_,Fid} <- Groups]]},
                #aside{class=["col-md-4"], body=[
                    #feed_ui{title= <<"Gossip">>, class="comments-flat", state=Discus}]}]}]}]}] ++ footer().

feed(Fid) ->
   #feed_ui{icon=[fa, "fa-tags ", "fa-large "],
            state=(wf:cache({Fid,?CTX#context.module}))#feed_state{js_escape=true,delegate=index}}.

featured() ->
  #carousel{class=["product-carousel"], items=case kvs:get(group, "featured") of
    {error, not_found} -> [];
    {ok, G} ->
      Ps = lists:flatten([ case kvs:get(product, Who) of 
                                {ok, P}->P; 
                                {error,_}-> [] end || #group_subscription{who=Who} <-
                                         kvs_group:members(G#group.name)]),
      [begin
        {Cover, Class} = case P#product.cover of
          undefined -> {<<"">>, ""};
          C -> 
            Ext = filename:extension(C),
            Name = filename:basename(C, Ext),
            Dir = filename:dirname(C),
            {filename:join([Dir, "thumbnail", Name++"_1170x350"++Ext]),""}
        end,
        [
          #panel{id=P#product.id, class=["slide"], body=[
            #h1{body=P#product.title},
            #image{class=[Class], image=Cover}
          ]},
          #button{class=[btn, "btn-large", "btn-inverse", "btn-info", "btn-buy", win, buy],
            body= [<<"Buy for ">>, #span{body= "$"++ float_to_list(P#product.price/100, [{decimals, 2}]) }],
            postback={add_cart, P}}
        ]
      end || P <- Ps]
  end, caption= #panel{class=[row],body=[
%        box(50, 12.99, "btn-warning", "fa-windows"), box(50, 12.99, "btn-success", "fa-windows"),
%        box(50, 12.99, "btn-violet", "fa-windows"), box(50, 12.99, "btn-info", "fa-windows") 
    ]}} .

box(Discount, Price, ColorClass, IconClass)->
  #panel{class=["col-sm-3", box], body=#button{class=[btn, "btn-large", ColorClass], body=[
    #p{style="margin-left:-10px;margin-right:-10px;", body= <<"Lorem: Ipsum dolor sit amet">>},
    #p{class=[accent], body= list_to_binary(integer_to_list(Discount)++"% OFF")},
    #p{class=[row], body=[
      #span{class=[IconClass, "pull-left"]}, #span{class=["pull-right"], body=[#span{class=["fa-usd"]},
        list_to_binary(io_lib:format("~.2f", [Price]))]} 
    ]} ]}}.

header() ->
    User = case wf:user() of undefined -> #user{}; U -> U end ,
    CartFeed = case lists:keyfind(cart, 1, User#user.feeds) of false -> {error, not_found};
        {_,Fid}-> kvs:get(feed,Fid) end,
    Avatar = case User#user.avatar of undefined -> <<"/holder.js/35x35">>;
        Av -> iolist_to_binary([Av++"?sz=35&width=35&height=35&s=35"]) end,

    {Admin, Rev, Dev} = case User of undefined -> {false,false,false};
    _ -> {  kvs_acl:check_access(User#user.email, {feature, admin}) == allow,
            kvs_acl:check_access(User#user.email, {feature, reviewer}) == allow,
            kvs_acl:check_access(User#user.email, {feature, developer}) == allow} end,

    [#header{class=[navbar, "navbar-default","navbar-fixed-top"], role=navigation, body=[
        #panel{class=["container"], body=[
            #panel{class=["navbar-header"], body=[
                #button{class=[btn, "navbar-toggle"], data_fields=?DATA_COLLAPSE,
                    body=[#i{class=["icon-bar"]}||_<-lists:seq(1,3)]},

                lists:flatmap(fun(F) -> case F of {navbar,0} -> [(?CTX#context.module):navbar()]; _-> [] end end,
                    (?CTX#context.module):module_info(exports)),
                #link{url="/index", class=["navbar-brand"], body= <<"countach">>} ]},

            #panel{class=["navbar-collapse", collapse], body=[
                #list{class=[nav, "navbar-nav"], body=[
                    #li{body=#link{url= <<"/store">>, body= <<"Store">> }},
                    #li{body=#link{url= <<"/index">>, body= <<"Reviews">> }},
                    #li{body=#link{url= <<"/chat">>, body= <<"Messages">> }}]},

                #list{class=[nav, "navbar-nav", "navbar-right"], body=case User#user.email of undefined ->
                    #li{body=#link{url= <<"/login">>, body= <<"Sign In">> }};
                _ -> [
                    #li{body=[#link{url= <<"/cart">>, body=[
                        #span{id=onlinenumber},
                        #span{class=["fa-stack", "fa-lg"], body=[
                            #span{id=?USR_CART(User#user.id), class=["cart-number", "fa-stack-2x"], body= case CartFeed of
                                                {error,_} -> <<"?">>;
                                                {ok, #feed{entries_count=C}} when C==0 -> <<"0">>;
                                                {ok, #feed{entries_count=C}} -> integer_to_list(C) end}
                        ]},

                        #span{class=["fa-stack", "fa-lg"],title= <<"shopping cart">>, body=[
                            #i{class=[fa,"fa-square-o","fa-stack-2x"]},
                            #i{class=[fa,"fa-shopping-cart","fa-stack-1x"]}]}]} ]},

                    #li{class=[dropdown], body=[
                        #link{class=["dropdown-toggle"], data_fields=?DATA_DROPDOWN, body=[
                            #image{class=["img-4x", "img-thumbnail", "img-circle"], image=Avatar} ]}
                        ,

                        #list{class=["dropdown-menu"], role=menu, body=[
                            #li{body=#link{url="/profile",  body=[#i{class=[fa,"fa-user", "fa-lg", "fa-fw"]},
                                           <<"&nbsp;Profile">>]}},
                            if Rev -> #li{body=#link{url="/myreviews", body=[
                                #i{class=[fa,"fa-list", "fa-lg", "fa-fw"]}, <<"&nbsp;Reviews">>]}};true->[] end,
                            if Dev -> #li{body=#link{url="/mygames",  body=[
                                #i{class=[fa,"fa-gamepad", "fa-lg", "fa-fw"]}, <<"&nbsp;Games">>]}};true->[] end,
                            #li{body=#link{url="/direct",body=[
                                #i{class=[fa,"fa-envelope", "fa-lg", "fa-fw"]}, <<"&nbsp;Notifications">>]}},
                            #li{body=#link{url="/cart",  body=[
                                #i{class=[fa,"fa-shopping-cart", "fa-lg", "fa-fw"]}, <<"&nbsp;Shopping Cart">>]}},
                            if Admin -> #li{body=#link{url="/admin", body=[
                                #i{class=[fa,"fa-cog", "fa-lg", "fa-fw"]}, <<"&nbsp;Admin">>]}};true->[] end,
                            #li{body=#link{id=logoutbtn, postback=logout, delegate=login,
                                body=[#i{class=[fa, "fa-power-off", "fa-lg", "fa-fw"]}, <<"&nbsp;Logout">>
                                ]}}]}]}] end }]}]}]},
    #panel{id=?PAGE_ALERT({?CTX#context.module, User#user.email}), class=["page-alert"]}].

footer() -> [ #dtl{file = "tl", ext="dtl", bindings=[]} ].

success(Msg)-> alert(Msg, "alert-success").
error(Msg)  -> alert(Msg,"alert-danger").
info(Msg)   -> alert(Msg,"alert-info").
warn(Msg)   -> alert(Msg,"alert-warning").
alert(Msg, Class)-> #panel{class=[Class], body=[
    #panel{class=[container, alert, "alert-block", "alert-dismissable"], body=[
        #button{class=[close], data_fields=[{<<"data-dismiss">>,<<"alert">>}], body= <<"&times;">>},
        #panel{class=[<<"text-center">>],body=Msg} ]} ]}.

alert_inline(Msg) ->
    #span{class=[alert, fade, in, "alert-danger"],
        style ="margin-left:10px;",body=[
        #span{body= Msg},
        #link{class=[close], url="#", 
              data_fields=[{<<"data-dismiss">>,<<"alert">>}],
              style="float:none;top:0;",
              body= <<"&times;">>}]}.

api_event(tabshow,Args,_) ->
    [Id|_] = string:tokens(Args,"\"#"),
    case Id of "all" -> []; 
    _ -> wf:update(Id, feed(list_to_integer(Id))) end,
    wf:wire("Holder.run();");
api_event(Name,Tag,Term) -> error_logger:info_msg("Name ~p, Tag ~p, Term ~p",[Name,Tag,Term]).

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({counter,C}) -> wf:update(onlinenumber,wf:to_list(C));
event({add_cart, P}) ->
    store:event({add_cart, P}),
    wf:redirect("/cart");
event(_) -> ok.

process_delivery([_Id, join,  G], {}) when G=="featured"-> wf:update(carousel, featured());
process_delivery([_Id, leave, G], {}) when G=="featured"-> wf:update(carousel, featured());
process_delivery([user,logout], #user{}=U) ->
    case wf:user() of User when User#user.email == U#user.email-> wf:logout();_ -> ok end;

process_delivery(R,M) -> feed_ui:process_delivery(R,M).

shorten(undefined) -> <<"">>;
shorten(Input) when is_list(Input) -> shorten(list_to_binary(Input));
shorten(Input) when is_binary(Input) ->
    R = [{"<img[^>]*>", ""}, {"<p></p>", ""},
        {"<br[\\s+]/>", ""}, {"^\\s*", ""}, {"\n+$", ""}],

    lists:foldl(fun({Pt, Re}, Subj) ->
        re:replace(Subj, Pt, Re, [global, {return, binary}]) end, Input, R).

render_element(#div_entry{entry=#entry{entry_id=Eid}=E, state=#feed_state{view=review}=State}) ->
    Id = element(State#feed_state.entry_id, E),
    UiId = wf:to_list(erlang:phash2(element(State#feed_state.entry_id, E))),
    {FromId, From,Avatar} = case kvs:get(user, E#entry.from) of 
                          {ok, User} -> {E#entry.from, User#user.display_name,User#user.avatar};
                          {error, _} -> {E#entry.from,E#entry.from,""} end,
    wf:render([
        #panel{class=["av-col"],
            body = case Avatar of
                [] -> [];
                _ -> #image{image=Avatar,width="100%",class=["img-thumbnail"]} end},
        #panel{class=["title-col"], body=[
            #panel{body= [
                #span{body=#link{body=#b{body=From}, url= "/profile?id="++wf:to_list(FromId)}}, " ",
                #span{body=index:to_date(E#entry.created)} ]},
            #h3{class=[title], body=[ 
                #span{id=?EN_TITLE(UiId), body=#link{body=E#entry.title, url=?URL_REVIEW(Eid)}},
                #span{class=[label,"label-info","msg-label"],body=[integer_to_list(kvs_feed:comments_count(entry, Id))]}
                    ]},
            #span{style="padding:3px;background-color:steelblue;color:white;",body="Erlang"} ]} ]).

to_date(undefined) -> to_date(now());
to_date(Date)->
  {{Y, M, D}, {H,Mi,_}} = calendar:now_to_datetime(Date),
  io_lib:format("~s ~p, ~p at ~p:~p", [?MONTH(M), D, Y, H,Mi]).
