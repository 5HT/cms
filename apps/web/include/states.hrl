-include_lib("feed_server/include/input.hrl").
-include_lib("feed_server/include/feed.hrl").

% My Games

-define(MYGAMES_FEED(Id), ?FD_STATE(Id)#feed_state{view=product,
                                                   entry_id=#entry.entry_id,
                                                   enable_selection=true,
                                                   delegate=mygames}).

-define(MYGAMES_INPUT(Id), #input_state{id=?FD_INPUT(Id),
                                        role=group,
                                        entry_type=product,
                                        show_upload=true,
                                        show_price=true,
                                        upload_dir = ?DIR(case wf:user() of undefined -> "anonymous"; #user{email=E}->E end),
                                        class= ["feed-table-header"],
                                        control_title = <<"Create">>,
                                        post_btn = <<"create">>,
                                        placeholder_rcp= <<"Categories">>,
                                        placeholder_ttl= <<"Game title">> }).
% My reviews
-define(MYREVIEWS_FEED(Id), ?FD_STATE(Id)#feed_state{view=review,
                                                    enable_selection=true,
                                                    del_by_index    =true,
                                                    delegate = myreviews}).

-define(MYREVIEWS_INPUT(Id), #input_state{id=?FD_INPUT(Id),
                                          role=product,
                                          entry_type=review,
                                          upload_dir = ?DIR(case wf:user() of undefined -> "anonymous"; #user{email=E}->E end),
                                          control_title= <<"Submit review">>,
                                          placeholder_rcp= <<"Games">>,
                                          class= ["feed-table-header"]}).

% Index 

-define(ACTIVE_FEED, ?FD_STATE(?FEED(comment))#feed_state{  flat_mode=true,
                                                            view=comment,
                                                            delegate=review,
                                                            entry_type=comment,
                                                            entry_id=#comment.comment_id}).

% Store

-define(STORE_FEED(Id), ?FD_STATE(Id)#feed_state{view=store, enable_selection=false, delegate=store}).

-define(PRODUCTS_FEED, ?STORE_FEED(?FEED(product))#feed_state{entry_type=product}).

% Reviews

-define(REVIEWS_FEED(Id), ?FD_STATE(Id)#feed_state{view=review, delegate=reviews}).

-define(ENTRIES_FEED, ?FD_STATE(?FEED(entry))#feed_state{view=review, delegate=reviews}).

% Review
-define(DETACHED_FEED(Id), ?FD_STATE(Id)#feed_state{view=detached, entry_id=#entry.entry_id, delegate=review}).

-define(COMMENTS_FEED(Id), ?FD_STATE(Id)#feed_state{view=comment,
                                                    entry_type=comment,
                                                    entry_id=#comment.comment_id,
                                                    show_title=true,
                                                    delegate=review}).

-define(COMMENTS_INPUT(Id, Recipients), #input_state{id=?FD_INPUT(Id),
                                                     recipients=Recipients,
                                                     entry_type = comment,
                                                     show_recipients=false,
                                                     show_title = false,
                                                     show_media = false,
                                                     control_title = <<"Add your comment">>,
                                                     class = ["comments-form"]}).

-define(REPLY_INPUT(Id, Recipients), #input_state{id=?FD_INPUT(Id),
                                                  recipients= Recipients,
                                                  entry_type = comment,
                                                  show_recipients = false,
                                                  show_title = false,
                                                  show_media = false,
                                                  collapsed = true,
                                                  post_collapse = true,
                                                  role=comment,
                                                  class= ["comment-reply"],
                                                  expand_btn= [<<"reply">>, #i{class=["icon-reply"]}],
                                                  expand_class=[]}).

% Cart

-define(CART_STATE(Id), ?FD_STATE(Id)#feed_state{view=cart,
                                                enable_selection=true,
                                                delegate=cart}).

% Product

-define(BLOG_STATE(Id), ?FD_STATE(Id)#feed_state{view=blog, html_tag=panel, delegate=product}).

-define(BLOG_INPUT(Id), #input_state{id=?FD_INPUT(Id),
                                    role=product,
                                    placeholder_ttl= <<"Title">>,
                                    class= ["feed-table-header"],
                                    show_recipients=false,
                                    collapsed=true}).

-define(FILE_INPUT(Id), #input_state{id=?FD_INPUT(Id),
                                     show_upload = true,
                                     show_recipients = false, show_title = false, show_body=true,
                                     upload_title= <<"Title">>,
                                     post_upload = attach_file,
                                     delegate_query = product,
                                     img_tool = undefined,
                                     entry_type=bundles,
                                     class=["feed-table-header"],
                                     control_title = <<"upload file">>}).

-define(FILE_STATE(Id), ?FD_STATE(Id)#feed_state{view=files,enable_selection=true,delegate=product}).

% Admin

-define(GROUPS_FEED, ?FD_STATE(?FEED(group))#feed_state{delegate=admin,
                                                        entry_type=group,
                                                        enable_selection=true,
                                                        enable_traverse=true,
                                                        html_tag=table}).

-define(GROUPS_INPUT, #input_state{ id=?FD_INPUT(?FEED(group)),
                                    entry_type=group,
                                    control_title= <<"Add category">>,
                                    placeholder_ttl= <<"name">>,
                                    show_recipients=false,
                                    show_scope=true,
                                    show_media=false}).

-define(ACL_FEED(Id), ?FD_STATE(Id)#feed_state{ delegate=admin,
                                                container=acl,
                                                entry_type=acl_entry,
                                                html_tag=table,
                                                enable_selection=false,
                                                enable_traverse=true}).

-define(USERS_FEED, ?FD_STATE(?FEED(user))#feed_state{delegate=admin,
                                                      entry_type=user,
                                                      entry_id=#user.username,
                                                      html_tag=table,
                                                      enable_traverse=true}).

-define(PRODUCTS_VIEW_FEED, ?FD_STATE(?FEED(product))#feed_state{delegate=admin,
                                                                 entry_type=product,
                                                                 enable_selection=true,
                                                                 enable_traverse=true,
                                                                 html_tag=table}).

-define(REVIEWS_VIEW_FEED, ?FD_STATE(?FEED(entry))#feed_state{delegate=admin,
                                                              entry_type=entry,
                                                              enable_selection=true,
                                                              enable_traverse=true,
                                                              del_by_index = true,
                                                              html_tag=table}).

-define(DIRECT_STATE(Id), ?FD_STATE(Id)#feed_state{ view=direct,
                                                    delegate = direct,
                                                    enable_selection=true,
                                                    enable_traverse=true}).

-define(DIRECT_INPUT(Id), #input_state{ id=?FD_INPUT(Id),
                                        role=user,
                                        entry_type=direct,
                                        collapsed = true,
                                        show_media = false,
                                        upload_dir = ?DIR(case wf:user() of undefined -> "anonymous"; #user{email=E}->E end),
                                        placeholder_rcp= <<"E-mail/User">>,
                                        placeholder_ttl= <<"Subject">>,
                                        class= ["feed-table-header"],
                                        expand_btn= <<"compose">>}).

-define(USR_PAYMENTS_FEED(Id), ?FD_STATE(Id)#feed_state{container = user_payment,
                                                        entry_type = payment,
                                                        delegate = admin,
                                                        enable_traverse = true,
                                                        html_tag = table }).
