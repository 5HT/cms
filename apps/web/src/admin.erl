-module(admin).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("n2o_bootstrap/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/payments.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/acls.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include("records.hrl").
-include("states.hrl").

main()-> #dtl{file="prod",
              bindings=[{title,<<"Admin">>},{body,body()},{css,?ADMIN_CSS},{less,?LESS},{js,?ADMIN_BOOTSTRAP}]}.

body() ->
    User = case wf:user() of undefined -> #user{}; U -> U end,
    wf:wire(#api{name=tabshow}),
    wf:wire(index:on_shown("'pill'")),

    Nav = {User, admin, subnav()},
    IsAdmin = kvs_acl:check_access(User#user.email, {feature, admin})==allow,

    dashboard:page(Nav, [
        #panel{class=["col-sm-9", "tab-content"], style="min-height:400px;",
            body = if IsAdmin -> [
                #panel{id=categories, class=["tab-pane", active], body=tab(categories)},
                [#panel{id=Id, class=["tab-pane"]} ||
                    Id <- [acl, users, products, reviews, user_payment, product_payment]]
            ];true -> [] end} ]).

subnav() -> [
    {categories, "categories"},
    {acl, "acl"},
    {users, "users"},
    {products, "products"},
    {reviews, "reviews"},
    {user_payment, "user payments"},
    {product_payment, "product payments"},
    {dev_payment, "dev payments"}
  ].

tab(categories) ->
    GroupsFeed = ?FEED(group),
    InputState = case wf:cache({?FD_INPUT(GroupsFeed),?CTX#context.module}) of undefined -> 
        Is = ?GROUPS_INPUT, wf:cache({?FD_INPUT(GroupsFeed),?CTX#context.module}, ?GROUPS_INPUT), Is; IS -> IS end,
    FeedState = case wf:cache({GroupsFeed,?CTX#context.module}) of undefined ->
        Fs = ?GROUPS_FEED, wf:cache({GroupsFeed,?CTX#context.module}, Fs), Fs; FS -> FS end,

    [#input{state=InputState, icon="fa fa-tags"},

    #feed_ui{title= <<"Categories ">>,
        icon="fa fa-list", state=FeedState,
        header=[#tr{class=["feed-table-header"], cells=[
            #th{body= <<"">>},
            #th{body= <<"id">>},
            #th{body= <<"name">>},
            #th{body= <<"description">>},
            #th{body= <<"scope">>} ]} ]}];

tab(acl)->
    {AclEn, Acl} = lists:mapfoldl(fun(#acl{id={R,N}=Aid}, Ain) ->
        State = case wf:cache({Aid,?CTX#context.module}) of undefined ->
            S = ?ACL_FEED(Aid), wf:cache({Aid,?CTX#context.module}, S), S; Fs -> Fs end,

        B = #panel{id=wf:to_list(R)++wf:to_list(N), class=["tab-pane"], body=[
            #feed_ui{title=wf:to_list(Aid)++" entries",
                icon="fa fa-list",
                state=State#feed_state{js_escape=true},
                header=[#tr{class=["feed-table-header"], cells=[
                    #th{body= <<"id">>},
                    #th{body= <<"accessor">>},
                    #th{body= <<"action">>}]} ]}]},
        Ao = [#tr{cells=[
            #td{body=#link{url="#"++wf:to_list(R)++wf:to_list(N), body=wf:to_list(Aid),
                data_fields=[{<<"data-toggle">>, <<"tab">>}]}},
            #td{body=wf:to_list(Aid)}]}|Ain],
        {B , Ao} end, [], kvs:all(acl)),

    [
    #h3{body= <<"ACL">>},
    #table{class=[table, "table-hover"], header=[#tr{class=["feed-table-header"],
        cells=[#th{body= <<"id">>}, #th{body= <<"resourse">>}]}], body=[Acl]},
    #panel{class=["tab-content"], body=[AclEn]} ];

tab(users)->
    UsrFeed = ?FEED(user),
    State = case wf:cache({UsrFeed,?CTX#context.module}) of undefined ->
        S = ?USERS_FEED, wf:cache({UsrFeed,?CTX#context.module}, S), S; FS -> FS end,

    #feed_ui{title= <<"Users ">>, icon="fa fa-user", state=State#feed_state{js_escape=true},
        header=[#tr{class=["feed-table-header"], cells=[
            #th{body= <<"email">>},
            #th{body= <<"roles">>},
            #th{body= <<"last login">>}]} ]};

tab(products)->
    State = ?PRODUCTS_VIEW_FEED,
    wf:cache({?FEED(product),?CTX#context.module}, State),

    #feed_ui{title= <<"Products">>, icon="fa fa-gamepad", state=State#feed_state{js_escape=true}, header=[
        #tr{class=["feed-table-header"], cells=[#th{body= <<"">>},
                                                #th{body= <<"title">>},
                                                #th{body= <<"description">>}]}]};

tab(reviews)->
    State = ?REVIEWS_VIEW_FEED,
    wf:cache({?FEED(entry),?CTX#context.module}, State),
    #feed_ui{title= <<"Reviews">>, icon="fa fa-list", state=State#feed_state{js_escape=true}, header=[
        #tr{class=["feed-table-header"],
            cells=[#th{},#th{body= <<"from">>},#th{body= <<"title">>},#th{body= <<"description">>}]}]};

tab(product_payment) ->
    #panel{class=[row],body=[
       #table{class=[table,"table-hover", "col-sm-4"], header=
            #tr{class=["feed-table-header"], cells=[#th{body= <<"product">>}]},
            body=[[#tr{cells=[
                    #td{body=#link{body=Title, postback={show_payment, prd, Id}}}
            ]} || #product{id=Id,title=Title} <- kvs:all(product)] ]},

        #panel{class=["col-sm-8"], body=#panel{id=prd}} ]};

tab(user_payment) ->
    #panel{class=[row],body=[
       #table{class=[table,"table-hover", "col-sm-4"], header=[
           #tr{class=["feed-table-header"], cells=[#th{body= <<"user">>},#th{body= <<"pays">>}]}],
            body=[[
                #tr{cells=[
                    #td{body=#link{body=User, postback={show_payment, usr, User}}},
                    #td{body=integer_to_list(Count)}
                ]} || #user_payment{id=User, entries_count=Count} <- kvs:all(user_payment)] ]},
        #panel{class=["col-sm-8"], body=#panel{id=usr}} ]};

tab(_)-> [].

% Render 

render_element(#row_entry{entry=#group{name=Name, description=Desc, scope=Scope}=E, state=#feed_state{}=S}) ->
    wf:render([
        #td{body=wf:to_list(element(S#feed_state.entry_id, E))},
        #td{body=Name},
        #td{body=Desc},
        #td{body=atom_to_list(Scope)}]);

render_element(#row_entry{entry=#user{email=Email}=U}) -> wf:render([
    #td{body=#link{url=?URL_PROFILE(Email), body=Email}},
    #td{body=[profile:features(wf:user(), U, "fa fa-2x")]},
    #td{body=case kvs:get(user_status, Email) of 
        {ok,Status} -> feed_ui:to_date(Status#user_status.last_login); {error,_}-> "" end}]);

render_element(#row_entry{entry=#product{title=Title, brief=Description}}) -> wf:render([
    #td{body=Title},
    #td{body=Description}]);

render_element(#row_entry{entry=#entry{title=Title, description=Description,from=From}}) -> wf:render([
    #td{body=From},
    #td{style="word-break:break-all;", body=Title},
    #td{body=Description}]);

render_element(#row_entry{entry=#acl_entry{accessor={user, Accessor}, action=Action}=E, state=S})-> wf:render([
    #td{body= wf:to_list(element(S#feed_state.entry_id, E))},
    #td{body= Accessor},
    #td{body= atom_to_list(Action)}]);

render_element(#row_entry{entry=#payment{id=Id, product_id=Pid}=Py}) -> wf:render([
    #td{body= #small{body=Id}},
    #td{body= #small{body=Py#payment.external_id}},
    #td{body= [#link{url=?URL_PRODUCT(Pid), body= Py#payment.product#product.title }]},
    #td{body= [#i{class=["fa fa-usd"]}, float_to_list(Py#payment.product#product.price/100, [{decimals,2}])]},
    #td{class=[case Py#payment.state of done -> "text-success"; added-> "text-warning"; _-> "text-error" end],
        body= [atom_to_list(Py#payment.state)]}]);

render_element(E) -> feed_ui:render_element(E).

feature_reply(#user{}=Whom, Feature, Msg, Eid) ->
    case lists:keyfind(direct, 1, Whom#user.feeds) of false -> skip;
        {_,Fid}=Feed ->
            Is = ?DIRECT_INPUT(Fid)#input_state{
                collect_msg = false,
                show_recipients = false,
                title = "Re: Feature <b>"++ wf:to_list(Feature)++"</b> request",
                recipients = [{user, Whom#user.email, Feed}],
                description = Msg},
            input:event({post, direct, Is}),
            error_logger:info_msg("entry reply to: ~p", [Eid]),
            case Eid of undefined -> ok;
            _ -> User = wf:user(),
                case lists:keyfind(direct, 1, User#user.feeds) of false -> ok;
                {_,Did}->
                    error_logger:info_msg("Entry delete: ~p", [Eid]),
                    msg:notify( [kvs_feed, User#user.email, entry, delete],
                                [#entry{id={Eid, Did}, entry_id=Eid, feed_id=Did}]) end end end.

% Events

api_event(tabshow,Args,_) ->
    [Id|_] = string:tokens(Args,"\"#"),
    case list_to_atom(Id) of categories -> ok; 
    Tab -> wf:update(Tab, tab(Tab)) end;
api_event(_,_,_) -> ok.

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({disable, What})->
    kvs_acl:define_access({user, What#user.email}, {feature,login}, disable),
    msg:notify([kvs_user, login, user, What#user.email, update_status], {disabled}),
    msg:notify([kvs_user, user, logout], What);
event({unblock, What})->
    kvs_acl:define_access({user, What#user.email}, {feature,login}, allow),
    msg:notify([kvs_user, login, user, What#user.email, update_status], {ok});
event({allow, Whom, Eid, Feature}) ->
    case kvs:get(user, Whom) of {error, not_found} -> skip;
    {ok, U} ->
        kvs_acl:define_access({user, Whom}, Feature, allow),
        feature_reply(U, Feature, <<"Your request accepted!">>, Eid) end;
event({cancel, Whom, Eid, Feature}) ->
    error_logger:info_msg("Cancel ~p", [Eid]),
    case kvs:get(user, Whom) of {error, not_found} -> skip;
    {ok, U} -> feature_reply(U, Feature, <<"Your request is prohibited!">>, Eid) end;
event({revoke, Feature, Whom})->
    case kvs:get(user, Whom) of {error, not_found} -> skip;
    {ok, U} ->
        kvs_acl:define_access({user, U#user.email}, {feature, Feature}, disable),
        feature_reply(U, Feature, <<"Your role disabled!">>, undefined) end;
event({show_payment, usr, Id}) ->
    State = case wf:cache({Id,?CTX#context.module}) of undefined -> 
        S = ?USR_PAYMENTS_FEED(Id), wf:cache({Id,?CTX#context.module},S),S; Ss->Ss end,

    Header = #tr{class=["feed-table-header"], cells=[
        #th{body= <<"id">>}, #th{body= <<"paypal">>},
        #th{body= <<"product">>},#th{body= <<"amount">>},
        #th{body= <<"state">>}]},

    wf:update(usr, #feed_ui{title= <<"payments">>,
                             icon="fa fa-list",
                             state=State#feed_state{js_escape=true},
                             header=Header});
event({show_payment, prd, Id}) ->
    Header = #tr{class=["feed-table-header"], cells=[
        #th{body= <<"Date">>}, #th{body= <<"Status">>},#th{body= <<"Price">>},#th{body= <<"User">>}]},

    wf:update(prd, #table{class=[table, "table-hover", payments], header=Header, body=[[begin
        #tr{cells=[
          #td{body=[index:to_date(Py#payment.start_time)]},
          #td{class=[case Py#payment.state of done -> "text-success"; added-> "text-warning"; _-> "text-error" end],
                body= [atom_to_list(Py#payment.state)]},
          #td{body=[#span{class=["fa fa-usd"]}, float_to_list(Price/100, [{decimals, 2}])]},
          #td{body=#link{body=User, url= ?URL_PROFILE(User)}}]}
      end || #payment{user_id=User, product=#product{price=Price}} = Py
        <- kvs:all_by_index(payment, product_id, Id) ]]} );
event({counter,C}) -> wf:update(onlinenumber,wf:to_list(C));
event(_) -> ok.

process_delivery(R,M) ->
    feed_ui:process_delivery(R,M).
