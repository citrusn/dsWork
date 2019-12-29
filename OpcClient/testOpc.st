OpcClientLibrary closeDefault; clear.
o := OpcClientLibrary default.
o opcConnect: 'opcserversim.Instance.1'.
o opcDisconnect.

b := ByteArray new: 600.
s := StringField length: 600.
o opcServerBrowse: b asParameter.
b asString.
o opcItemBrowse: b asParameter.
b asString.

o opcAddGroup: 'd'.
o opcCopyItem: (DWORD fromInteger: 0) item: 'Name1'.
o opcCopyItem: (DWORD fromInteger: 1) item: 'Name1'.

o opcAddItem: (DWORD fromInteger: 1).
r := FLOATArray new: 1.
o opcRead: (DWORD fromInteger: 1) value: r asParameter.
r