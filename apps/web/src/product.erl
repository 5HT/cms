-module(product).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("n2o_bootstrap/include/wf.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/payments.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("kernel/include/file.hrl").
-include("records.hrl").
-include("states.hrl").

main() -> #dtl{file="prod", bindings=[{title,<<"product">>}, {body, body()},
                                      {css,?PRODUCT_CSS}, {less,?LESS}, {js, ?PRODUCT_BOOTSTRAP}]}.

-define(FILE_DIR, "static/"++ case wf:user() of undefined-> "anonymous"; User -> User#user.email end ++"/files").

body() ->
    Id = case wf:qs(<<"id">>) of undefined -> <<"no">>; I-> I end,
    index:header()++[
    #section{class=[section], body=#panel{class=[container], body=
    case kvs:get(product, binary_to_list(Id)) of
    {ok, P} ->
        wf:wire(#api{name=tabshow}),
        wf:wire(index:on_shown("'tab'")),

        wf:session(product, P),
        Owner = case wf:user() of undefined -> false; U -> P#product.owner == U#user.email end,
        Groups = lists:flatmap(fun(#group_subscription{where=G})->
            case kvs:get(group,G) of
                {error,_}-> [];
                {ok, #group{scope=Scope}} when Scope == private -> [];
                {ok, #group{id=Gid,name=Name,feeds=Feeds}} -> [{Gid,Name,lists:keyfind(products,1,Feeds)}] end end,
            kvs_group:participate(P#product.id)),
        UsrFeeds = [{Feed,Fid}||{Feed, Fid} <- P#product.feeds, Feed /= comments andalso Feed /= bundles],

        [#panel{class=[row, "page-header-sm"], style="padding:0", body=[
            #h4{class=["col-sm-9"], style="line-height:30px;margin-left:5px;", body= [
                #link{url= <<"/reviews">>, body= <<"Categories ">>, style="color:#999"},
                #small{body=[
                    [<<" | ">>, #link{url="/store?id="++Gid, body=[#span{class=["fa fa-asterisk"]},Name]}]
                 || {Gid, Name, _} <- Groups]} ]},

            #panel{class=["col-sm-3", "input-append", "pull-right"], style="margin:10px 0", body=[
                #textbox{class=[disabled], placeholder= <<"Search">>, disabled=true},
                #button{class=[btn, disabled], body= <<"Go!">>} ]} ]},

        #section{class=[section, alt], body=#panel{class=[container], body=[
            #product_hero{product=P},
            #list{class=[nav, "nav-tabs", "sky-nav", "entry-type-tabs"], body=[
                [#li{class= if Feed == reviews -> ["active"]; true -> [] end,
                    body=#link{data_fields=?DATA_TAB,
                        url= "#"++wf:to_list(Feed), body=wf:to_list(Feed)}} || {Feed, _} <- UsrFeeds],

                if Owner -> [
                    #li{body=#link{data_fields=?DATA_TAB, url="#files",   body= <<"files">> }},
                    #li{body=#link{data_fields=?DATA_TAB, url="#finance", body= <<"finance">> }}]; true -> [] end
            ]} ]}},

        #section{class=[section], body=
            #panel{class=[container], body=
                #panel{class=[row], body=[
                    #panel{class=["col-sm-9"], body=
                        #panel{class=["tab-content"], body=[
                            [if Feed == reviews ->
                                #panel{id=reviews, class=["tab-pane", active], body=feed(P,Fid,false)};
                             true ->
                                #panel{id=Feed, class=["tab-pane"], body=feed(P,Fid)} end 
                                || {Feed,_}=Fid <- UsrFeeds],
                            if Owner -> [
                                #panel{id=files,   class=["tab-pane"], body=files(P)},
                                #panel{id=finance, class=["tab-pane"], body=payments(P)}]; true -> [] end ]}},
                    #panel{class=["col-sm-3"], body=aside(P, Groups)} ]}}}
        ];
    {error, E} -> #panel{class=[alert, "alert-danger","alert-block"], body=[
        #button{class=[close], data_fields=[{<<"data-dismiss">>,<<"alert">>}], body= <<"&times;">>},
        #strong{body= atom_to_list(E)} ]} end }}]++index:footer().

feed(#product{} = P, {Tab, Id}) -> feed(#product{} = P, {Tab, Id}, true).
feed(#product{} = P, {Tab, Id},Escape) ->
    User = case wf:user() of undefined -> #user{}; U -> U end,

    State = ?BLOG_STATE(Id)#feed_state{enable_selection=User#user.email == P#product.owner},
    Is = ?BLOG_INPUT(Id)#input_state{
        entry_type=case Tab of reviews -> review; _-> Tab end,
        recipients=[{product, P#product.id, {Tab,Id}}],
        expand_btn= "Write "++atom_to_list(Tab)},

    wf:cache({Id,?CTX#context.module}, State),
    wf:cache({?FD_INPUT(Id),?CTX#context.module}, Is),

    #feed_ui{title=wf:to_list(Tab), icon="fa fa-circle", state=State#feed_state{js_escape=Escape}, header=[
        if User#user.email == P#product.owner orelse Tab == reviews -> #input{state = Is}; true -> [] end]}.

payments(#product{} = P)-> [
    #table{class=[table, "table-hover", payments],
      header=[#tr{cells=[#th{body= <<"Date">>}, 
                         #th{body= <<"Status">>},
                         #th{body= <<"Price">>},
                         #th{body= <<"User">>}]}],
      body=[[begin
        #tr{cells=[
          #td{body=[index:to_date(Py#payment.start_time)]},
          #td{class=[case Py#payment.state of done -> "text-success"; added-> "text-warning"; _-> "text-error" end],
                body= [atom_to_list(Py#payment.state)]},
          #td{body=[case Cur of "USD"-> #span{class=["fa fa-usd"]}; _ -> #span{class=["fa fa-money"]} end,
                wf:to_list(Price/100, [{decimals, 2}])]},
          #td{body=#link{body=User, url= ?URL_PROFILE(User)}}]}
      end || #payment{user_id=User, product=#product{price=Price, currency=Cur}} = Py
        <- kvs:all_by_index(payment, product_id, P#product.id) ]]}].

files(#product{} = P)->
    case lists:keyfind(bundles, 1, P#product.feeds) of false-> [];
    {_,Fid}->
        Fs = case wf:cache({Fid,?CTX#context.module}) of undefined ->
            State = ?FILE_STATE(Fid), wf:cache({Fid,?CTX#context.module}, State), State; S -> S end,

        Is = case wf:cache({?FD_INPUT(Fid),?CTX#context.module}) of 
                  undefined -> In = ?FILE_INPUT(Fid)#input_state{upload_dir= ?FILE_DIR,
                                         recipients=[{product,P#product.id,{bundles, Fid}}]},
                                wf:cache({?FD_INPUT(Fid),?CTX#context.module}, In), In;
                         S1 -> S1 end,

        #feed_ui{title="product files", state=Fs, header=[#input{state=Is}]} end.

aside(#product{} = P, Groups)->
    ActiveFeed = case lists:keyfind(comments, 1, P#product.feeds) of false -> [];
    {_,Fid} ->
        AS = case wf:cache({Fid,?CTX#context.module}) of undefined ->
            Fs = ?FD_STATE(Fid)#feed_state{
                flat_mode=true,
                view=comment,
                entry_type=comment,
                entry_id=#comment.comment_id},
            wf:cache({Fid,?CTX#context.module}, Fs), Fs;
        AF -> AF end,
        #feed_ui{title= <<"Product discussion">>, 
                icon="fa fa-comments-o", 
                class="comments-flat",
                state=AS} end,

    GroupFeeds = [begin
        {_,Gfid} = Fd,
        GS = case wf:cache({Gfid,?CTX#context.module}) of undefined ->
            Gfs = ?FD_STATE(Gfid)#feed_state{view=group, flat_mode = true,
                                             entry_id = #entry.entry_id, delegate=product},
            wf:cache({Gfid,?CTX#context.module}, Gfs), Gfs; GF -> GF end,

            #feed_ui{title= "More " ++ wf:to_list(Name),
                icon = "fa fa-list",
                class= "entries-flat",
                state=GS}
    end || {_,Name,Fd} <- Groups, Fd /= false],

    #aside{class=[sidebar], body=[
       [#panel{class=["sidebar-widget"], body=F} || F <- GroupFeeds],
      #panel{class=["sidebar-widget"], body=ActiveFeed} ]}.

%controls(#entry{type=Type} =  E) -> [
%  #link{body=[case Type of product -> <<"view ">>; _-> <<"read more ">> end, 
%    #i{class=["fa fa-double-angle-right", "fa-lg"]}], posback={read, Type, E#entry.id}} ].


% Render elements of the page feeds

render_element(#product_hero{product=P}) ->
    Bought = lists:any(fun(#payment{product_id=PrId}) -> P#product.id==PrId end,
        case wf:user() of undefined -> []; #user{email=Email}-> kvs_payment:payments(Email) end),
    Hero = #panel{class=[row], body=[
    #panel{class=["col-sm-6"], body=[
    #panel{class=["hero-unit", "product-hero"], body=[
        #h2{body=P#product.title},
        #p{body=P#product.brief},
        #panel{body=#span{class=["game-rating"], body=[#span{class=["star"]} || _ <- lists:seq(1,5)]}},
        #panel{class=["btn-toolbar", "text-center"], body=[
            if Bought ->
                case lists:keyfind(bundles, 1, P#product.feeds) of false -> [];
                {_, Fid} ->
                    case kvs:entries(kvs:get(feed, Fid), entry, 1) of [] -> [];
                    [#entry{media=[#media{url=Url}]}] ->
                        error_logger:info_msg("URL: ~p", [Url]),
                        #link{class=[btn, "btn-large", "btn-success"],
                                body=[#i{class=["fa fa-windows", "fa-lg"]},<<" download">>],
                                url=Url,
                                download=[filename:basename(Url)]} end end;
            true ->
                #button{class=[btn, "btn-large", "btn-inverse", "btn-info", "btn-buy", win],
                    body= [<<"buy for ">>, #span{body= "$"++ wf:to_list(P#product.price/100, [{decimals, 2}]) }],
                    postback={add_cart, P}} end
        ]}
      ]}
    ]},
    #panel{class=["col-sm-6", "text-center"], body=[
      #image{image=P#product.cover}
    ]}
  ]},
  element_panel:render_element(Hero);

render_element(#div_entry{entry=E, state=#feed_state{view=files}=S})->
    Uid = wf:to_list(erlang:phash2(element(S#feed_state.entry_id, E))),
    Panel =#panel{class=["file-entry"],body=[
        #p{id=?EN_DESC(Uid), body=E#entry.description},
        [#entry_media{media=M, mode=input} ||M <- E#entry.media]
    ]},
    element_panel:render_element(Panel);

render_element(#div_entry{entry=#entry{}=E, state=#feed_state{view=group, flat_mode=true}=State}) ->
    case kvs:get(product, E#entry.entry_id) of {error, _} -> wf:render(#panel{body= <<"error displaying item">>});
    {ok, P} ->
        Id = wf:to_list(erlang:phash2(element(State#feed_state.entry_id, P))),
        Media = input:media(P#product.cover),

        wf:render([
            #panel{id=?EN_MEDIA(Id), class=["col-sm-5", "media-pic"], body=#entry_media{media=Media, mode=store}},

            #panel{class=["col-sm-6", "article-text"], style="height:5em",  body=[
                #h4{body=#span{id=?EN_TITLE(Id), class=[title], body=
                #link{style="color:#9b9c9e;", body=P#product.title, url=?URL_PRODUCT(P#product.id)}}},

            #p{id=?EN_DESC(Id), body=index:shorten(P#product.brief)} ]},

            #panel{class=["col-sm-12"], body=[
                #link{body=#i{class=["fa fa-windows"]}},
                #link{url="#",body=[
                    #span{class=[?EN_CM_COUNT(Id)], body=
                        integer_to_list(kvs_feed:comments_count(product, P#product.id))},
                    #i{class=["fa fa-comment-alt"]} ]} ]} ]) end;


% Render blog view

render_element(#div_entry{entry=#entry{}=E, state=#feed_state{view=blog}=State})->
    {Eid,_} = Id = element(State#feed_state.entry_id, E),
    UiId = wf:to_list(erlang:phash2(element(State#feed_state.entry_id, E))),
    From = case kvs:get(user, E#entry.from) of {ok, User} -> User#user.display_name; {error,_} -> E#entry.from end,

    Entry = #panel{class=["blog-post"], body=[
        #header{class=["blog-header"], body=[
            #h3{body=[#span{id=?EN_TITLE(UiId), body=E#entry.title, data_fields=[{<<"data-html">>, true}]}, 
            #small{body=[<<" by ">>, #link{body=From}, index:to_date(E#entry.created)]}]}]},

        #figure{class=["thumbnail-figure"], body=[
            #carousel{items=[#entry_media{media=Media, mode=blog} || Media <- E#entry.media]},
            if length(E#entry.media) > 1 ->
                #figcaption{class=["thumbnail-title"], 
                            body=[#h4{body=#span{body=E#entry.title}}]}; true -> [] end ]},

        #panel{id=?EN_DESC(UiId), body=index:shorten(E#entry.description), data_fields=[{<<"data-html">>, true}]},

        #footer{class=["blog-footer", row], body=[
            #link{body=[ #i{class=["fa fa-comments-o", "fa-lg"]},
                #span{class=[?EN_CM_COUNT(UiId)], body=integer_to_list(kvs_feed:comments_count(entry, Id))}],
                url=?URL_REVIEW(Id)},
            #link{class=["pull-right"], body= [<<"read more ">>, #i{class=["fa fa-double-angle-right", "fa-lg"]}],
                url=?URL_REVIEW(Eid)} ]} ]},
    element_panel:render_element(Entry);
render_element(E)-> feed_ui:render_element(E).

% Events

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({add_cart, P}) ->
    store:event({add_cart, P}),
    wf:redirect("/cart");
event(_) -> ok.

api_event(tabshow,Args,_) ->
    [Id|_] = string:tokens(Args,"\"#"),
    P = wf:session(product),
    case lists:keyfind(list_to_atom(Id), 1, P#product.feeds) of false -> ok;
        {reviews,_} -> ok;
        Tab -> wf:update(list_to_atom(Id), feed(P, Tab)) end,
    wf:wire("Holder.run();");
api_event(_,_,_) -> ok.

control_event(_, {query_file, Root, Dir, File, MimeType, _PostWrite, Target})->
    FileName = filename:join([Root,Dir,binary_to_list(File)]),

    {exist, case file:read_file_info(FileName) of {error, _} -> 0;
    {ok, FileInfo} ->
        Url = filename:join([Dir,binary_to_list(File)]),
        Media = #media{id=element_upload:hash(FileName), url = Url, type ={attachment,MimeType}},
        wf:cache(medias, [Media]),
        wf:update(Target, input:media_preview(Target, [Media])),
        wf:wire("Holder.run();"),
        FileInfo#file_info.size end}.

process_delivery(R,M) -> feed_ui:process_delivery(R,M).
