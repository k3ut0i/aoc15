import qualified Test1 as T1
import qualified Test5 as T5
import qualified Test6 as T6
import qualified Test7 as T7
import qualified Test9 as T9
import qualified Test11 as T11
import qualified Test16 as T16
import qualified Test19 as T19
main :: IO ()
main = T1.test >>
       T5.test >>
       T6.test >>
       T7.test >>
       T9.test >>
       T11.test >>
       T16.test >>
       T19.test
