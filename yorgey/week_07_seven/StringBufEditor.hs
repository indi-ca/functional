module Main where

import StringBuffer
import Editor
--import JoinList

main = runEditor editor $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]




--getInitialBuffer :: JoinList (Size, Score) String
--getInitialBuffer = Single ((Size 1), (Score 1)) "bob"




