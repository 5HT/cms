-module(profile).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("n2o_bootstrap/include/wf.hrl").
-include_lib("kvs/include/user.hrl").
-include_lib("kvs/include/payments.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/acls.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("kernel/include/file.hrl").
-include("records.hrl").
-include("states.hrl").

main() -> [#dtl{file = "prod",  ext="dtl", bindings=[
    {title,<<"Profile">>},{body,body()},{css,?PROFILE_CSS},{less,?LESS},{js, ?PROFILE_BOOTSTRAP}]}].

navbar() ->
    #button{class=[btn, "navbar-toggle","navbar-toggle-left"], data_fields=?DATA_OFFCANVAS,
        body=[#i{class=["icon-bar"]}||_<-lists:seq(1,3)]}.

body() ->
    Who = case wf:user() of undefined -> #user{}; U -> U end,
    What = case wf:qs(<<"id">>) of undefined -> Who;
        Val -> case kvs:get(user, binary_to_list(Val)) of {error, not_found} -> #user{}; {ok, Usr1} -> Usr1 end end,
    Nav = {What, profile, []},

    dashboard:page(Nav, if What#user.email == undefined ->
        wf:update(?PAGE_ALERT({?CTX#context.module, Who#user.email}),
            index:alert(["No user "++wf:to_list(wf:qs(<<"id">>))], "alert-info"));
        true -> [
            dashboard:section(profile, profile_info(Who, What, "fa-2x"), "fa-user"),
            if Who == What -> payments(What);
            true -> [
                case lists:keyfind(direct, 1,What#user.feeds) of false -> [];
                {_,Fid} ->
                    DirectInput = case wf:cache({?FD_INPUT(Fid),?CTX#context.module}) of undefined ->
                        Is = ?DIRECT_INPUT(Fid)#input_state{
                            show_recipients = false,
                            expand_btn= <<"Write message">> ,
                            recipients=[{user, What#user.email, {direct, Fid}}]},
                        wf:cache({?FD_INPUT(Fid),?CTX#context.module}, Is), Is; IS->IS end,
                    #input{state=DirectInput} end,

                case lists:keyfind(feed, 1, element(#iterator.feeds, What)) of false -> [];
                {_, Fid} ->
                    FeedState = case wf:cache({Fid,?CTX#context.module}) of undefined -> 
                        Fs = ?REVIEWS_FEED(Fid), wf:cache({Fid,?CTX#context.module}, Fs), Fs; FS->FS end,
                    #feed_ui{title= <<"Recent activity">>, icon=[fa,"fa-list"], state=FeedState} end ] end ] end).


profile_info(#user{}=Who, #user{}=What, Size) ->
    Status = case What#user.status of ok -> #b{class=["text-success"], body= <<"Active">>};
        S -> #b{class=["text-error"], body= wf:to_list(S)} end,
    Mailto = #link{url= if What#user.email==undefined -> [];
        true-> iolist_to_binary(["mailto:", What#user.email]) end, body=#strong{body= What#user.email}},
    [
    #h3{body=[<<"Profile ">>,  if Who==What ->
        #span{id=profile_ctl, body=[
            %#link{body=[#i{class=[fa,"fa-edit","fa-lg"]}], title="edit", postback={edit_profile}} 
        ]}; true -> [] end]},

    #panel{class=[row], body=[
        #panel{class=["col-sm-4", "dashboard-img-wrapper"], body=[
            #panel{id=profile_img,body=[
                #image{class=["img-circle", "img-thumbnail"],
                    image = case What#user.avatar of undefined ->  "/holder.js/180x180";
                    Av -> re:replace(Av, <<"_normal">>, <<"">>, [{return, list}])
                        ++"?sz=180&width=180&height=180&s=180" end, width= <<"180px">>, height= <<"180px">>}]},
            #panel{id=img_ctl, class=["dashboard-img-ctl"], body=[]} ]},

        #panel{class= ["col-sm-8", "profile-info-wrapper"], body=[
            #panel{class=["form-group"], body=[
                #label{body= <<"Name:">>},
                #b{id=displayname, body= What#user.display_name}]},
            #panel{class=["form-group"], body=[#label{body= <<"Mail:">>}, Mailto]},
            #panel{body=[#label{body= <<"Member since ">>},
                #strong{body= index:to_date(What#user.register_date)}]},
            Status,
            features(Who, What, Size) ]}]}];
profile_info(Who, What, Size) -> case kvs:get(user, What) of {ok, U}-> profile_info(Who,U,Size); _-> [] end.

features(Who, What, Size) ->
    IsBlocked = kvs_acl:check_access(What#user.email, {feature,login}) == disable,
    IsWriter =  kvs_acl:check_access(What#user.email, {feature,reviewer}) =:= allow,
    IsDev =     kvs_acl:check_access(What#user.email, {feature,developer}) =:= allow,
    IsAdmin =   kvs_acl:check_access(What#user.email, {feature,admin}) =:= allow,
    AmIAdmin= kvs_acl:check_access(Who#user.email,  {feature, admin}) == allow,

    User = if IsBlocked -> [
        #span{class = [fa, "fa-stack", Size], body=[
            #i{class=[fa, "fa-user", "fa-stack-1x"]},
            #i{class=[fa, "fa-ban", "fa-stack-2x", "text-danger"]}]},
        if AmIAdmin ->
            #link{class=[btn, "btn-default"], body= <<"unblock">>,
                postback={unblock, What}, delegate=admin};true -> [] end];
    true -> [
        #span{class=[fa, "fa-stack", Size], body=[
            #i{class=[fa,"fa-user", "fa-stack-1x", "text-warning"]}]},
        if AmIAdmin ->
            #link{class=[btn, "btn-default"], body= <<"block">>,
                postback={disable,What}, delegate=admin};true -> [] end ] end,

    Writer = if IsWriter -> [
        #span{class=[fa, "fa-stack", Size], body=[
            #i{class=[fa,"fa-pencil", "fa-stack-1x", "text-success"]}]},
        if AmIAdmin -> #link{class=[btn, "btn-default"],
            body= <<"revoke">>,
            postback={revoke, reviewer, What#user.email}}; true -> [] end];
    Who == What ->
        #link{class=[btn, "btn-default"], body= <<"request reviewer">>, postback={request, reviewer}};
    true -> [] end,

    Developer = if IsDev -> [
        #span{class=[fa, "fa-stack", Size], body=[
            #i{class=[fa,"fa-barcode", "fa-stack-1x", "text-info"]}
        ]},
        if AmIAdmin -> #link{class=[btn, "btn-default"],
            body= <<"revoke">>,
            postback={revoke, developer, What#user.email}}; true -> [] end ];
    Who == What ->
        #link{class=[btn, "btn-default"], body= <<"request developer">>, postback={request, developer}};
    true -> [] end,

    Admin = if IsAdmin -> [
        #span{class=[fa, "fa-stack", Size], body=[
            #i{class=[fa, "fa-wrench", "fa-stack-1x"]} ]}];true -> [] end,

    #p{body=[ User,Writer,Developer,Admin ]}.

payments(#user{email=Id}) ->
    State = case wf:cache({Id,?CTX#context.module}) of undefined ->
        S = ?USR_PAYMENTS_FEED(Id), wf:cache({Id,?CTX#context.module},S),S; Ss->Ss end,

    Header = #tr{class=["feed-table-header"], cells=[
        #th{body= <<"id">>},
        #th{body= <<"paypal">>},
        #th{body= <<"product">>},
        #th{body= <<"amount">>},
        #th{body= <<"state">>}]},

    #feed_ui{title= <<"Payments">>,
             icon=[fa, "fa-list"],
             state=State#feed_state{js_escape=true},
             header=Header}.

preview_medias(#media{} = M)-> [
    #image{image = case M#media.thumbnail_url of undefined -> <<"/holder.js/180x180">>;
        Th ->  Ext = filename:extension(Th),
            filename:join([filename:dirname(Th), filename:basename(Th, Ext)++"_180x180"++Ext]) end},
    #panel{id=apply_ctl, class=["btn-toolbar", "text-center"],body=[
        #link{class=[btn],body= <<"apply">>, postback={apply_image, M}},
        #link{class=[btn],body= <<"cancel">>, postback={close_image}} ]}].

% Event

api_event(attach_media, Args, _Tag)->
  Props = n2o_json:decode(Args),
  Target = binary_to_list(proplists:get_value(<<"preview">>, Props#struct.lst)),
  Id = proplists:get_value(<<"id">>, Props#struct.lst),
  File = binary_to_list(proplists:get_value(<<"file">>, Props#struct.lst)),
  Type = proplists:get_value(<<"type">>, Props#struct.lst),
  Thumb = binary_to_list(proplists:get_value(<<"thumb">>, Props#struct.lst)),
  Media = #media{id = Id,
    url = File,
    type = {attachment, Type},
    thumbnail_url = Thumb},
  wf:update(Target, preview_medias(Media));

api_event(Name,Tag,Term) -> error_logger:info_msg("dashboard Name ~p, Tag ~p, Term ~p",[Name,Tag,Term]).

control_event(_, {query_file, Root, Dir, File, MimeType, _PostWrite, Target})->
  Name = binary_to_list(File),
  Size = case file:read_file_info(filename:join([Root,Dir,Name])) of 
    {ok, FileInfo} ->
      Media = #media{
        id = element_upload:hash(filename:join([Root,Dir,Name])),
        url = filename:join([Root,Dir,Name]),
        type = {attachment, MimeType},
        thumbnail_url = filename:join([Dir,"thumbnail",Name])},
      wf:update(Target, preview_medias(Media)),
      FileInfo#file_info.size;
    {error, _} -> 0 end,
  {exist, Size}.

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({edit_profile}) ->
    Who = wf:user(),
    wf:update(profile_ctl, [#link{body=#i{class=["fa-reply", "fa-large", "text-warning"]},
        title= <<"close">>, postback={cancel}}]),
%    wf:update(displayname, #panel{class=["input-append"], body=[
%        #textbox{id=display_name,value=[Who#user.display_name]},
%        #button{class=[btn], body=#i{class=["fa-refresh", "fa-large"]},
%            title= <<"update">>, source=[display_name], postback={apply_name}} ]}),
    wf:update(img_ctl,
        #upload{id=profile_upload,
                preview=false,
                root=?ROOT,
                dir="static/"++ case Who of undefined-> "anonymous"; User -> User#user.email end,
                value="",
                delegate_query=?MODULE,
                post_write=attach_media,
                delegate_api=?MODULE,
                img_tool=gm,
                post_target=profile_img,
                size=?THUMB_SIZE});
event({cancel})->
    Who = wf:user(),
    wf:update(profile_ctl, [#link{body=#i{class=["fa-edit", "fa-large"]},
                                  title= <<"edit">>, postback={edit_profile}}]),
%    wf:update(displayname, Who#user.display_name),
    wf:update(img_ctl, []);
event({apply_name}) ->
    User = wf:user(),
    DisplayName = wf:q(display_name),
    case kvs:put(User#user{display_name=DisplayName}) of ok ->
        wf:user(User#user{display_name=DisplayName}),
        wf:update(?PAGE_ALERT({?CTX#context.module, User#user.email}),
            index:alert(<<"Display name updated.">>, "alert-success"));
    _-> 
    wf:update(?PAGE_ALERT({?CTX#context.module, User#user.email}),
            index:alert(<<"Failed to update display name. Please try again.">>, "alert-danger")) end;
event({apply_image, #media{thumbnail_url=Th}}) ->
    User = wf:user(),
    Ext = filename:extension(Th),
    Ava = filename:join([filename:dirname(Th), filename:basename(Th, Ext)++"_180x180"++Ext]),
    case kvs:put(User#user{avatar=Ava}) of ok ->
        wf:user(User#user{avatar=Ava}),
        wf:update(?PAGE_ALERT({?CTX#context.module, User#user.email}),
            index:alert(<<"Profile image updated.">>, "alert-success")),
        wf:remove(apply_ctl);
    _->
        wf:update(?PAGE_ALERT({?CTX#context.module, User#user.email}),
            index:alert(<<"Failed to update profile image. Please try again.">>, "alert-danger")) end;
event({close_image}) ->
    User =  wf:user(),
    wf:update(profile_img, #image{image = case User#user.avatar of undefined ->  "/holder.js/180x180";
        Av -> re:replace(Av, <<"_normal">>, <<"">>, [{return, list}])
            ++"?sz=180&width=180&height=180&s=180" end, width= <<"180px">>, height= <<"180px">>});
event({request, Feature}) ->
    User =wf:user(),
    case kvs:get(acl, {feature, admin}) of {error, not_found} -> 
        wf:update(?PAGE_ALERT({?CTX#context.module, User#user.email}),
            index:alert(<<"system has no administrators yet.">>, "alert-warning"));
    {ok,#acl{}=Acl} ->
        Recipients = lists:flatten([case kvs:get(user, Accessor) of {error,_} -> [];
            {ok, U} -> {Type, Accessor, lists:keyfind(direct, 1, U#user.feeds)} end
            || #acl_entry{accessor={Type,Accessor}, action=Action} <- 
                       kvs:entries(Acl, acl_entry, undefined), Action =:= allow]),

        case lists:keyfind(direct, 1, User#user.feeds) of false -> skip;
        {_,Fid} ->
            Is = ?DIRECT_INPUT(Fid)#input_state{
                entry_type = {feature, Feature},
                collect_msg = false,
                show_recipients = false,
                title = "Feature <b>"++ wf:to_list(Feature)++"</b> request",
                recipients = Recipients,
                description = wf:to_list(Feature) ++ " requested!"},

            input:event({post, {feature, Feature}, Is}),
            wf:update(?PAGE_ALERT({?CTX#context.module, User#user.email}),
                index:alert(wf:to_list(Feature) ++" requested", "alert-info"))
    end end;
event({counter,C}) -> wf:update(onlinenumber,wf:to_list(C));
event(Event) ->
    User = case wf:user() of undefined -> #user{}; U -> U end,
    IsAdmin = kvs_acl:check_access(User#user.email, {feature, admin})==allow,
    if IsAdmin -> admin:event(Event); true -> ok end.

process_delivery(R,M) ->
    wf:update(sidenav, dashboard:sidenav({wf:user(), profile, []})),
    feed_ui:process_delivery(R,M).
