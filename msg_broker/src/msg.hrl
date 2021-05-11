



% Options---------
-define(topics, "topics").
-define(exp, "expiration_date").
-define(presistent, "presistent").
-define(reg_name, "reg_name").
% ----------------




% Older version

% -type conn_pub_type() :: connect_pub_msg.
% -type dissconn_pub_type() :: disconnect_pub_msg.

% -type sub_type() :: subscribe_msg.
% -type unsub_type() :: unsubscribe_msg.

% -type control_type() :: conn_pub_type() | dissconn_pub_type() | sub_type() | unsub_type(). 

% -type data_type() :: data_type.

% -type msg_type() :: control_type() | data_type().

% -record(header, {
%     type :: msg_type(),
%     topics :: list(string()),
%     opt :: list(any())
% }).



% -record(msg, {
%     header :: #header{},
%     body :: any()
% }).


