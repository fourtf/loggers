{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Discord
import           Discord.Types
import qualified Discord.Requests              as R
import           Discord.Internal.Rest.Channel

import           Web.Spock
import           Web.Spock.Config

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Control.Concurrent
import           Control.Concurrent.Chan
import           Data.IORef
import           Data.Monoid
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.IO                  as T
import           System.Environment

data MySession = EmptySession
newtype MyAppState = DummyAppState (IORef Int)

data Error = Error { errType :: T.Text
    , errAppName :: T.Text
    , errMessage :: T.Text
    } deriving Show

main :: IO ()
main = do
    errorChan <- newChan
    token     <- T.pack <$> readVar "DISCORD_TOKEN" "./discord_token"
    channelId <- Snowflake . read <$> readVar "DISCORD_CHANNEL_ID"
                                              "./discord_channel_id"

    forkIO $ logErrors errorChan token channelId
    runSpockServer errorChan


runSpockServer :: Chan Error -> IO ()
runSpockServer errorChan = do
    ref      <- newIORef 0
    spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
    runSpock 8075 $ spock spockCfg (spockApp errorChan)

spockApp :: Chan Error -> SpockM () MySession MyAppState ()
spockApp errorChan = do
    get root $ text "200"
    post ("error" <//> var) $ \appName -> do
        body' <- T.decodeUtf8 <$> body
        liftIO $ writeChan errorChan $ Error { errType    = "error"
                                             , errAppName = appName
                                             , errMessage = body'
                                             }

        text "200"


logErrors :: Chan Error -> T.Text -> Snowflake -> IO ()
logErrors chan token channelId = forever $ do
    userFacingError <- runDiscord $ def
        { discordToken   = token
        , discordOnEvent = onDiscordEvent
        , discordOnStart = void . forkIO . (void . loop)
        }
    T.putStrLn userFacingError

  where
    loop dis = do
        error' <- readChan chan
        result <- restCall dis $ CreateMessage channelId $ formatError error'
        case result of
            Left err -> print err
            _        -> return ()
        loop dis


formatError :: Error -> T.Text
formatError error =
    errType error <> " from '" <> errAppName error <> "': " <> errMessage error

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


readVar :: String -> String -> IO String
readVar envName fileName = do
    putStrLn
        $  "Reading from env '"
        <> envName
        <> "' or file '"
        <> fileName
        <> "'"
    channelId <- lookupEnv "DISCORD_CHANNEL_ID"
    case channelId of
        (Just envVar) -> return envVar
        Nothing       -> readFile fileName
