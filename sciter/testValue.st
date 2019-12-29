SciterLibrary default: nil.
sc := nil.

sc := SciterLibrary default.
sc api.

v := ValueStructure new.
sc ValueInit: v.

sc ValueIntData: v
sc ValueStringDataSet: v data: 'smile'
sc ValueIntData: v
v 
v t: 3.
v u: 0.
v d: 3333.
