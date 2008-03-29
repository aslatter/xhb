newtype $it$ = Mk$it$ Xid
 deriving (Serializable, Deserializable, Eq, Ord, Show)

instance XidLike $it$ where
 fromXid = Mk$it$

$! do not export the constructor !$