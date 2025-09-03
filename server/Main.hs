import System.Directory (getCurrentDirectory)

main :: IO ()
main = do
    putStrLn "Start!"
    getCurrentDirectory >>= putStrLn


-- TODO:
--  - Add code until we have warp serving a basic generated html page
--  - read data from local json file
--  - render out cat app using local data
--  - create client-side Main.hs that loads data from a header tag
--  - hydrate the app client-side
