module Pretty where

import Types

import Text.PrettyPrint.HughesPJ

-- |Minaml complete definition:
--
-- One of 'pretty' or 'toDoc'.
class Pretty a where
    toDoc :: a -> Doc
    pretty :: a -> String

    pretty = show . toDoc
    toDoc = text . pretty

-- Builtin types

instance Pretty String where
    pretty = show

instance Pretty Int where
    pretty = show

instance Pretty a => Pretty [a] where
    toDoc = brackets . hsep . punctuate (char ',') . map toDoc

instance (Pretty a, Pretty b) => Pretty (a,b) where
    toDoc (x,y) = parens $ hsep $ punctuate (char ',') $ [toDoc x, toDoc y]

-- Simple stuff

instance Pretty ExInfo where
    pretty = show

instance Pretty UnionElem where
    pretty = show

instance Pretty Binop where
    pretty Add  = "+"
    pretty Sub  = "-"
    pretty Mult = "*"
    pretty Div  = "/"
    pretty RShift = ">>"
    pretty And = "&"

instance Pretty EnumElem where
    toDoc (EnumElem name expr)
        = text name <> char ':' <+> toDoc expr


-- More complex stuff

instance Pretty Expression where
    toDoc (Value n) = toDoc n
    toDoc (Bit n) = text "2^" <> toDoc n
    toDoc (FieldRef ref) = char '$' <> text ref
    toDoc (Op binop exprL exprR)
        = parens $ hsep [toDoc exprL
                        ,toDoc binop
                        ,toDoc exprR
                        ]

instance Pretty StructElem where
    toDoc (Pad n) = braces $ toDoc n <+> text "bytes"
    toDoc (List nm typ len)
        = text nm <+> text "::" <+> brackets (text typ) <+> toDoc len
    toDoc (SField nm typ) = hsep [text nm
                                 ,text "::"
                                 ,text typ
                                 ]
    toDoc (ExprField nm typ expr)
          = parens (text nm <+> text "::" <+> text typ)
            <+> toDoc expr
    toDoc (ValueParam typ mname lname)
        = text "Valueparam" <+>
          text "::" <+>
          hsep (punctuate (char ',') [text typ
                                     ,text mname
                                     ,text lname
                                     ])

instance Pretty XDecl where
    toDoc (XStruct nm elems) =
        hang (text "Struct:" <+> text nm) 2 $ vcat $ map toDoc elems
    toDoc (XTypeDef nm typ) = hsep [text "TypeDef:"
                                    ,text nm
                                    ,text "as"
                                    ,text typ
                                    ]
    toDoc (XEvent nm n elems) =
        hang (text "Event:" <+> text nm <> char ',' <> toDoc n) 2 $
             vcat $ map toDoc elems
    toDoc (XRequest nm n elems mrep) = 
        (hang (text "Request:" <+> text nm <> char ',' <> toDoc n) 2 $
             vcat $ map toDoc elems)
         $$ case mrep of
             Nothing -> empty
             Just reply ->
                 hang (text "Reply:" <+> text nm <> char ',' <> toDoc n) 2 $
                      vcat $ map toDoc reply
    toDoc (XidType nm) = text "XID:" <+> text nm
    toDoc (XidUnion nm elems) = 
        hang (text "XID" <+> text "Union:" <+> text nm) 2 $
             vcat $ map toDoc elems
    toDoc (XEnum nm elems) =
        hang (text "Enum:" <+> text nm) 2 $ vcat $ map toDoc elems
    toDoc (XImport nm) = text "Import:" <+> text nm
    toDoc (XError nm n elems) = 
        hang (text "Error:" <+> text nm) 2 $ vcat $ map toDoc elems

instance Pretty XHeader where
    toDoc (XHeader nm _ decs) = text nm $$ (vcat $ map toDoc decs)
