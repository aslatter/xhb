$if(extensions)${-# LANGUAGE $extensions; separator=", "$ #-}$endif$

module $basename$.$moduleName$
  $if(exports)$($exports; separator=", "$)$endif$ where

$if(imports)$$imports:Import()$$endif$
$if(relImports)$$relImports:ImportRel()$$endif$
$if(xidDecls)$$xidDecls:XidDeclare()$$endif$

$structs:Struct()$
