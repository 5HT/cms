-module(chat).
-compile(export_all).
-compile({parse_transform, shen}).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/feeds.hrl").
-include("records.hrl").
-include("states.hrl").

main() ->
    #dtl{file = wf:cache(mode), ext="dtl",
         bindings=[{title,<<"Chat">>},{body,body()},{css,?CSS},{js,?DEFJS},{less,?LESS}]}.

body() ->
    {ok,Pid} = wf:async("main",fun() -> chat_loop() end),

    Feeds = [{"inbox","Inbox"},{"archive","Archive"},{"system","System"}],
    UsersFeed = ?FEED(user),
    Conversations = ?ROOMS_FEED,
    User = case wf:user() of undefined -> #user{}; X -> X end,
    History =  case lists:keyfind(sent, 1, element(#iterator.feeds, User)) of
        false -> #feed_state{};
        {A, Id} -> wf:info("Id ~p",[{A,Id}]),?HISTORY_FEED(Id) end,

    #panel{class=[page], body=[
        index:header(),
        #panel{class=["page-wrapper"],body=[
            #section{class=[container], body=[
                #h3{body=[string:join([ [#link{url="#"++F, body=N}] || {F,N} <- Feeds ]," ") ]},
                #panel{class=["col-md-4", "tab-content"], body=[ 
                    #panel{id=inbox,class=["tab-pane",active], body=#feed_ui{state=Conversations}} ]},
                #panel{id=history,class=["col-md-8",history], body=[
                    #feed_ui{state=History},
                    #htmlbox{id=message,class=["form-control"],style="width:100%;font-size:14pt;"},
                    #button{id=send,body="Send",postback={chat,Pid},source=[message]} ]} ]} ]} ]}.

% render roster

render_element(#div_entry{entry=#user{avatar=Avatar,
    id=From,register_date=Date}, state=State})->
    wf:render([
        #panel{class=["av-col"],
            body = case Avatar of
                [] -> [];
                _ -> #image{image=Avatar,width="100%",class=["img-thumbnail"]} end},
        #panel{class=["title-col"], body=[
            #span{body= [
                #span{body=#link{body=From}}, " ",
                #span{body=index:to_date(Date)} ]},
            #h3{class=[title], body=[ 
                #span{body=#link{style="font-size:18pt;",body=#b{body=From}}},
                #span{class=[label,"label-info","msg-label"],body=[]} ]} ]}
    ]);

% render messages

render_element(#div_entry{entry=#entry{from=From,to=To,
    created=Date,description=Dsc,title=Title}=E, state=State})->
    wf:render([ message(From,Dsc,Date) ]);
render_element(E)-> wf:info("Unknown: ~p",[E]).
%message(_,"",_) -> [];
%message(_,<<>>,_) -> [];
message(Who,What,When) ->
    #panel{body=[
        #span{body= [
            #span{body=#link{body=Who}}, " ",
            #span{body=index:to_date(When)} ]},
        #panel{class=[message],body=What} ]}.

% page events

event(init) ->
    Self = self(),
    wf:reg(room),
    wf:send(lobby,{top,5,Self}),
    Terms = wf:render(receive Top -> [ message(U,M,now()) || {U,M} <- Top] end),
    wf:insert_top(<<"history">>, wf_convert:js_escape(Terms)),
    wf:wire("$('#history').scrollTop = $('#history').scrollHeight;");
event({inc,Pid}) -> wf:info("Inc"), Pid ! inc;
event(chat) -> wf:redirect("chat");
event({counter,C}) -> wf:update(onlinenumber,wf:to_list(C));
event(hello) -> wf:redirect("login");
event(<<"PING">>) -> ok;
event({chat,Pid}) ->
    Username = case wf:user() of undefined -> "anonymous"; A -> A#user.display_name end,
    Message = wf:q(message),
    Terms = [ message("Systen","Message added",now()), #button{postback=hello} ],
    wf:wire("$('#message').focus(); $('#message').select(); "),
    wf:send(lobby,{add,{Username,Message}}),
    Pid ! {message, Username, Message}.

% async subscription to global room

chat_loop() ->
    receive
        {message, Username, Message} ->
            Terms = message(Username,Message,now()),
            wf:insert_top(<<"history">>, wf_convert:js_escape(wf:render(Terms))),
            wf:wire(#jq{target=history,property=scrollTop,right=#jq{target=history,property=scrollHeight}}),
            wf:flush(room);
        Unknown -> error_logger:info_msg("Unknown Looper Message ~p in Pid: ~p",[Unknown,self()])
    end, chat_loop().
