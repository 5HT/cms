-module(paypal).
-compile(export_all).
-include_lib("kvs/include/products.hrl").
-include("paypal.hrl").

set_express_checkout(Params) ->
    ExParams = Params++[
        {"cancelUrl",   ?PP_CANCEL_URL},
        {"returnUrl",   ?PP_RETURN_URL}],

    case nvp_request("SetExpressCheckout", ExParams) of {error, E} -> {error, E};
        Response ->
            Cmd = lists:concat([?PP_WEBSCR, "&cmd=_express-checkout&token=",proplists:get_value("TOKEN",Response)]),
            wf:redirect(Cmd) end.

get_express_details(Params)->
    case nvp_request("GetExpressCheckoutDetails", Params) of {error, E} -> {error, E}; Response -> Response end.

do_express_checkout(Params)->
    case nvp_request("DoExpressCheckoutPayment", Params) of {error, E} -> {error, E}; Response -> Response end.

nvp_request(Method, Params) ->
    ExtendedParams = Params ++ [
        {"USER",        ?PP_USERNAME},
        {"PWD",         ?PP_PASSWORD},
        {"SIGNATURE",   ?PP_SIGNATURE},
        {"METHOD",      Method},
        {"VERSION",     98}],

    Body = string:join([string:join([wf:to_list(K),wf:to_list(V)],"=")|| {K,V}<-ExtendedParams],"&"),

    case httpc:request(post,{?PP_NVP, [], "x-www-form-urlencode", Body},[],[]) of
    {error, E} -> wf:info("[paypal] Error ~p", [E]), {error, E};
    {ok, {StatusLine, _, ResponseBody}} ->
        case StatusLine of {"HTTP/1.1",200,"OK"} ->
            Details = [begin [K,V]=string:tokens(P,"="),{K,V} end||P<-string:tokens(http_uri:decode(ResponseBody),"&")],

            case proplists:get_value("ACK", Details) of "Success" -> Details;
            _ ->
                wf:info("[paypal] ~p ACK failed ~p", [Method, Details]),
                {error, Details} end;
       _ ->
            wf:info("[paypal] ~p Request error. Status: ~p [~p]", [Method, StatusLine, ResponseBody]),
           {error, StatusLine} end;
    {ok, {StatusCode,_}} ->
        wf:info("[paypal] ~p Request error. Status code: ~p", [Method, StatusCode]),
        {error, StatusCode};
    {ok, ReqId} ->
        wf:info("[paypal] ~p Request error ~p", [Method, ReqId]),
        {error, ReqId} end.

product_request(Index, Id, #product{title=Title,price=Price, brief=Brief}) -> [ 
    {"L_PAYMENTREQUEST_0_NAME"++Index, Title},
    {"L_PAYMENTREQUEST_0_NUMBER"++Index, Id},
    {"L_PAYMENTREQUEST_0_DESC"++Index, Brief},
    {"L_PAYMENTREQUEST_0_AMT"++Index, float_to_list(Price/100, [{decimals,2}])},
    {"L_PAYMENTREQUEST_n_QTY"++Index, 1},
    {"L_PAYMENTREQUEST_n_ITEMCATEGORY"++Index, "Digital"}].
