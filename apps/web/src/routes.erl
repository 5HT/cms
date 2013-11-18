-module (routes).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-export([init/2, finish/2]).

finish(State, Ctx) -> {ok, State, Ctx}.
init(State, Ctx) -> 
    Path = wf:path(Ctx#context.req),
    Module = route_prefix(Path),
    {ok, State, Ctx#context{path=Path,module=Module}}.

route_prefix(<<"/ws/",P/binary>>) -> route(P);
route_prefix(<<"/",P/binary>>) -> route(P);
route_prefix(P) -> route(P).

route(<<>>)              -> index;
route(<<"index">>)       -> index;
route(<<"admin">>)       -> admin;
route(<<"product">>)     -> product;
route(<<"login">>)       -> login;
route(<<"chat">>)        -> chat;
route(<<"store">>)       -> store;
route(<<"cart">>)        -> cart;
route(<<"checkout">>)    -> checkout;
route(<<"review">>)      -> review;
route(<<"reviews">>)     -> reviews;
route(<<"direct">>)      -> direct;
route(<<"profile">>)     -> profile;
route(<<"mygames">>)     -> mygames;
route(<<"myreviews">>)   -> myreviews;
route(<<"favicon.ico">>) -> static_file;
route(_) -> index.

