-ifndef(__AVCT_HRL__).
-define(__AVCT_HRL__, true).

%% API function return value result codes.
-define(AVCT_SUCCESS,                0).       %% Function successful
-define(AVCT_NO_RESOURCES,           1).       %% Not enough resources
-define(AVCT_BAD_HANDLE,             2).       %% Bad handle
-define(AVCT_PID_IN_USE,             3).       %% PID already in use
-define(AVCT_NOT_OPEN,               4).       %% Connection not open
%% PSM for AVCT.
-define(AVCT_PSM,                    16#0017).
-define(AVCT_BR_PSM,                 16#001B).
%% Protocol revision numbers
-define(AVCT_REV_1_0,                16#0100).
-define(AVCT_REV_1_2,                16#0102).
-define(AVCT_REV_1_3,                16#0103).
-define(AVCT_REV_1_4,                16#0104).
%% the layer_specific settings
-define(AVCT_DATA_CTRL,              16#0001).    %% for the control channel
-define(AVCT_DATA_BROWSE,            16#0002).    %% for the browsing channel
-define(AVCT_DATA_PARTIAL,           16#0100).    %% Only have room for a partial message
-define(AVCT_MIN_CONTROL_MTU,        48).  %% Per the AVRC spec, minimum MTU for the control channel
-define(AVCT_MIN_BROWSE_MTU,         335). %% Per the AVRC spec, minimum MTU for the browsing channel

-define(AVCT_MSG_OFFSET,             15).
-define(AVCT_BROWSE_OFFSET,          17). %% the default offset for browsing channel
%% Connection role.
-define(AVCT_INT,                    0).       %% Initiator connection
-define(AVCT_ACP,                    1).       %% Acceptor connection
%% Control role.
-define(AVCT_TARGET,                 1).       %% target 
-define(AVCT_CONTROL,                2).       %% controller 
-define(AVCT_PASSIVE,                4).       %% If conflict, allow the other side to succeed 
%% Command/Response indicator.
-define(AVCT_CMD,                    0).       %% Command message
-define(AVCT_RSP,                    2).       %% Response message
-define(AVCT_REJ,                    3).       %% Message rejected
%% Control callback events.
-define(AVCT_CONNECT_CFM_EVT,        0).       %% Connection confirm
-define(AVCT_CONNECT_IND_EVT,        1).       %% Connection indication
-define(AVCT_DISCONNECT_CFM_EVT,     2).       %% Disconnect confirm
-define(AVCT_DISCONNECT_IND_EVT,     3).       %% Disconnect indication
-define(AVCT_CONG_IND_EVT,           4).       %% Congestion indication
-define(AVCT_UNCONG_IND_EVT,         5).       %% Uncongestion indication
-define(AVCT_BROWSE_CONN_CFM_EVT,    6).       %% Browse Connection confirm
-define(AVCT_BROWSE_CONN_IND_EVT,    7).       %% Browse Connection indication
-define(AVCT_BROWSE_DISCONN_CFM_EVT, 8).       %% Browse Disconnect confirm
-define(AVCT_BROWSE_DISCONN_IND_EVT, 9).       %% Browse Disconnect indication
-define(AVCT_BROWSE_CONG_IND_EVT,    10).      %% Congestion indication
-define(AVCT_BROWSE_UNCONG_IND_EVT,  11).      %% Uncongestion indication
%% General purpose failure result code for callback events.
-define(AVCT_RESULT_FAIL,            5).

-endif.
