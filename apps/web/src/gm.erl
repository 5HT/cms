-module(gm).
-compile(export_all).

make_thumb(In,W,H,Out) ->
    case os:cmd(lists:concat([
        "gm convert -size ", integer_to_list(W*2), "x", integer_to_list(H*2),
        " \"",In,"\"",
        " -gravity center -background transparent",
        " -extent ",integer_to_list(W),"x",integer_to_list(H),
        " -thumbnail ", integer_to_list(W),"x",integer_to_list(H), % " -thumbnail ", integer_to_list(W*H), "@"
        " \"",Out,"\""])) of [] -> ok; E -> error_logger:info_msg("GM:~p", [E]), {error, E} end.
