%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2025, Tony Rogvall
%%% @doc
%%%    BLE State Record Definition
%%%    Shared between ble.erl and gatt_client.erl
%%% @end

-ifndef(__BLE_STATE_HRL__).
-define(__BLE_STATE_HRL__, true).

%% Type definitions
-type service_type() :: primary | secondary.
-type property() :: read|write|notify|indicate.
-type descriptor() :: term().  %% FIXME
-type handle() :: integer().
-type uuid() :: <<_:128>> | <<_:16>> | <<_:32>>.

-type characteristic() ::
	#{ handle => handle(),
	   uuid => binary(),
	   properties => [property()],
	   value_handle => integer(),
	   value => binary(),
	   descriptors => [descriptor()]
	 }.

-type service() ::
	#{ handle => handle(),
	   uuid => binary(),
	   type => service_type(),
	   characteristics => [handle()]
	 }.

-type connection() :: 
	#{
	  ref => reference(),           %% Connection reference (for API)
	  handle => handle(),           %% HCI connection handle
	  addr => binary() | undefined, %% Device address
	  objects => #{ handle() => service() | characteristic() },
	  uuids => #{ uuid() => handle() },
	  %% Notification callback
	  notify_callback => fun((handle(), binary()) -> ok) | undefined
	 }.

%% BLE State Record
-record(ble_state, 
	{
	 mode :: peripheral | central,
	 hci :: reference(),
	 name :: undefined | string(),
	 %% For peripheral mode: services we offer
	 services = [] :: [service()],
	 advertising = false :: boolean(),
	 connections = #{} :: #{ handle() => connection()},
	 conn_refs = #{} :: #{ reference() => handle() },
	 pending_conn = undefined :: undefined | {binary(), pid(), reference()},
	 %% Pending ATT request context
	 pending_att = undefined :: undefined | term(), 
	 size :: map(),
	 features :: binary()
	}).
-type ble_state() :: #ble_state{}.

-endif.
