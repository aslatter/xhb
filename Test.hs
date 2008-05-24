
import Proto
import Types

import Control.Monad


main = pretty "Test" test_xdecs

pretty :: String -> [XDecl] -> IO ()
pretty name decs = putStrLn $ prettyBuild name $ mapM_ xDecl decs

test_xdecs = [XidType "ATOM"
             ,XidType "WINDOW"
             ,XidType "FONT"
             ,XTypeDef "VISUALID" "CARD32"
             ,XTypeDef "TIMESTAMP" "CARD32"
             ,XTypeDef "KEYSYM" "CARD32"
             ,pointStruct
             ,rectStruct
             ,screenStruct
             ,XImport "xproto"
             ,XImport "xfiles"
             ,testEvent
             ,testEvent2
             ]


pointStruct = XStruct "POINT"
              [SField "x" "INT16"
              ,SField "y" "INT16"
              ]

rectStruct = XStruct "RECTANGLE"
             [SField "x" "INT16"
             ,SField "y" "INT16"
             ,SField "width" "CARD16"
             ,SField "height" "CARD16"
             ]

screenStruct = XStruct "SCREEN"
               [SField "root" "WINDOW"
               ,SField "default_colormap" "COLORMAP"
               ,SField "white_pixel" "CARD32"
               ,SField "black_pixel" "CARD32"
               ,SField "curren_input_masks" "CARD32"
               ,SField "width_in_pixels" "CARD16"
               ,SField "height_in_pixels" "CARD16"
               ,SField "width_in_millimeters" "CARD16"
               ,SField "height_in_millimeters" "CARD16"
               ,SField "min_installed_maps" "CARD16"
               ,SField "max_installed_maps" "CARD16"
               ,SField "root_visual" "VISUALID"
               ,SField "backing_stores" "BYTE"
               ,SField "save_unders" "BOOL"
               ,SField "root_depth" "CARD8"
               ,ListSize "allowed_depths_len" "CARD8" "allowed_depths"
               ,List "allowed_depths" "DEPTH" "allowed_depths_len"
               ]

testEvent = XEvent "MyEvent" 23
            [SField "screen" "SCREEN"
            ,SField "x" "INT16"
            ,SField "y" "INT16"
            ]

testEvent2 = XEvent "MyOtherEvent" 24
             [SField "rect" "RECTANGLE"
             ,SField "left" "POINT"
             ,SField "right" "POINT"
             ]