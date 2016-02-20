import qualified Compilation
import qualified Demo
import qualified CoDesign
import qualified NumSimpl

main = do
    Compilation.testAll
    Demo.testAll
    CoDesign.testAll
    NumSimpl.main

