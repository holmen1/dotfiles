import Data.Default.Class

-- Define our own instance to test the class without external dependencies
data MyType = MyType Int String deriving Show

instance Default MyType where
    def = MyType 42 "default"

main :: IO ()
main = do
    let myDef = def :: MyType
    putStrLn $ "Default MyType: " ++ show myDef
    putStrLn "data-default-class linking successful!"