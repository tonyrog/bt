-ifndef(SCO_HRL).
-define(SCO_HRL, true).

-type uint16() :: 0..65535.

-record(sco_conninfo, 
	{
	 hci_handle :: uint16(),
	 dev_class  :: <<_:3>>
	}).

-endif.


