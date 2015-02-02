{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
import Yesod

-- Foundation data type.
--
-- In a larger application this would contain
-- settings, static resources, db connection pool,
-- http connection manager, logging, etc.
data App = App

-- Use `ghc -ddump-splices` to see the generated code.
mkYesod "App" [parseRoutes|
/ HomeR GET
/page PageR GET
|]

instance Yesod App

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|
                            <h1>Hello World!
                            <a href=@{PageR}>Page
                         |]

getPageR :: Handler Html
getPageR = do
    defaultLayout $ do
        let myvar = "hello world"
        toWidgetHead [lucius| .header { color: #FF0000 } |]
        toWidget [julius| console.log("Hello world!") |]
        [whamlet|
            <h1 .header> New Page!
            <a href=@{HomeR}>Home
        |]


main :: IO ()
main = warp 3000 App
