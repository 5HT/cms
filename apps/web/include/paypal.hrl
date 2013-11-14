-define(PP_USERNAME,    "stanp-facilitator_api1.clodot.com").
-define(PP_PASSWORD,    "1381757776").
-define(PP_SIGNATURE,   "A94NQXxLq.C9wiLuWrkiY.DrqDTVAyt3-GmOWOTrKTV8MkHBhw9KjRSM").
-define(PP_CANCEL_URL,  "http://localhost:8001/shopping_cart").
-define(PP_RETURN_URL,  "http://localhost:8001/checkout").
-define(PP_NVP,         "https://api-3t.sandbox.paypal.com/nvp").
-define(PP_WEBSCR,      "https://www.sandbox.paypal.com/cgi-bin/webscr").

-define(PP_TRANSACTION, "CORRELATIONID").
-define(PP_ACK,         "ACK").
-define(PP_ERROR_MSG,   "L_LONGMESSAGE0").
-define(PP_STATUS,      "CHECKOUTSTATUS").

-define(PP_PAYMENTREQUEST(Total), [
        {"PAYMENTREQUEST_0_ITEMAMT", float_to_list(Total/100, [{decimals,2}])},
        {"PAYMENTREQUEST_0_TAXAMT", 0},
        {"PAYMENTREQUEST_0_HANDLINGAMT", 0},
        {"PAYMENTREQUEST_0_SHIPPINGAMT", 0},
        {"PAYMENTREQUEST_0_SHIPDISCAMT", 0},
        {"PAYMENTREQUEST_0_INSURANCEAMT", 0},
        {"PAYMENTREQUEST_0_PAYMENTACTION", "SALE"},
        {"PAYMENTREQUEST_0_AMT", float_to_list(Total/100, [{decimals,2}])},
        {"PAYMENTREQUEST_0_CURRENCYCODE", "USD"},
        {"ALLOWNOTE", 1},
        {"LOGOIMG", "http://igratch.synrc.com/static/img/logo.png"},
        {"NOSHIPPING", 1}]).
