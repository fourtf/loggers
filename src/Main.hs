{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Discord
import           Discord.Types
import qualified Discord.Requests              as R
import           Discord.Internal.Rest.Channel

import           Web.Spock
import           Web.Spock.Config

import           Control.Monad
import           Control.Monad.Trans
import           Control.Concurrent
import           Control.Concurrent.Chan
import           Data.IORef
import           Data.Monoid
import qualified Data.Text                     as T
import           Debug.Trace
import qualified Data.Text.IO                  as TIO

data MySession = EmptySession
newtype MyAppState = DummyAppState (IORef Int)

main :: IO ()
main = do
    errorChan <- newChan
    token     <- TIO.readFile "./secret_token"
    channelId <- Snowflake . read <$> readFile "./channel_id"

    -- discord conn
    forkIO $ logErrors errorChan token channelId

    -- http server
    ref      <- newIORef 0
    spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
    runSpock 8080 $ spock spockCfg (app errorChan)

app :: Chan T.Text -> SpockM () MySession MyAppState ()
app errorChan = do
    get root $ text "200"
    post ("error" <//> var) $ \appName -> do
        liftIO $ writeChan errorChan appName

        text "200"

logErrors :: Chan T.Text -> T.Text -> Snowflake -> IO ()
logErrors chan token channelId = forever $ do
    userFacingError <- runDiscord $ def
        { discordToken   = token
        , discordOnEvent = onDiscordEvent
        , discordOnStart = void . forkIO . (void . loop)
        }
    TIO.putStrLn userFacingError

  where
    loop dis = do
        msg <- readChan chan
        err <- restCall dis $ CreateMessage channelId msg
        print err
        loop dis

onDiscordEvent :: DiscordHandle -> Event -> IO ()
onDiscordEvent dis event = case event of
    MessageCreate m -> when (not (fromBot m) && isPing (messageText m)) $ do
        _ <- restCall
            dis
            (R.CreateReaction (messageChannel m, messageId m) "eyes")
        pure ()
    _ -> pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isPing :: T.Text -> Bool
isPing = ("ping" `T.isInfixOf`) . T.toLower
