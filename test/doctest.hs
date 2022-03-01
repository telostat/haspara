import Test.DocTest ( doctest )


main :: IO ()
main = doctest ["-XOverloadedStrings", "-XTemplateHaskell", "-isrc", "src"]
