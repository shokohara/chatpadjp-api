{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib where

--import Data.Text
import Data.Text.IO (writeFile)
import System.IO.Unsafe (unsafePerformIO)
import Data.Char
import Data.String.Here
import Control.Lens
import Test.WebDriver
import Test.WebDriver.Commands.Wait
import qualified Test.WebDriver.Common.Keys as K
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as B
import Text.XML.HXT.Core
import Text.HandsomeSoup
import Control.Exception
import Control.Monad
import Control.Concurrent (threadDelay)

myConfig :: WDConfig
myConfig = defaultConfig {wdCapabilities = defaultCaps {browser = chrome}}

someFunc :: IO ()
someFunc = putStrLn "someFunc"

main4 :: IO ()
main4 = runSession myConfig $ do
  openPage "http://chatpad.jp/"
  printCookies
  e <- findElem . ByXPath $ "//*[@id=\"body\"]/div[2]/div[1]/div/div[4]/a"
  click e
  u <- getCurrentURL
  liftIO $ putStrLn u
--  liftIO $ fmap (putStrLn) getCurrentURL
  cs <- cookies
  liftIO $ mapM_ print cs
  printMessages
  waitUntil 60 $ findElem (ByXPath [here|//*[@id="chatLog"]/div/div[2][contains(., "チャットを始めるよー！")]|])
  liftIO $ threadDelay 2000
  sendText $ "こんにちは" ++ (T.unpack K.enter)
  printMessages
  return $ do
    a <- return $ printMessages
--    if "チャット相手がチャットを終了" `elem` a
    return "x"
  return $ do
    a <- return $ printMessages
    x <- getLine
    return $ sendText x
--    return $ getLine >>= sendText
--    return $ ""
--  liftIO $ getLine >>= sendText
  printMessages
  waitUntil 100000000000000 $ findElem (ByXPath [here|//*[@id="chatLog"]/div/div[2][contains(., "チャット相手がチャットを終了したよ！")]|])
  writeSource "source.html"
  screenshotWriteFile "google.png"
  closeSession

--xxx :: WD ()
--xxx = do
--  l <- liftIO getLine
--  sendText l
--  if (T.isInfixOf "終了" $ (unsafePerformIO getSource)) then return () else xxx

ttt :: T.Text -> Bool
ttt = T.isInfixOf "終了"

--i2w :: WD a -> IO a
--i2w x = do
--  a <- x
--  liftIO $ x

printCookies :: WD()
printCookies = do
  c <- cookies
  liftIO $ mapM_ print c

printMessages :: WD ()
printMessages = do
  x <- getSource
  let doc = readString [withParseHTML yes, withWarnings no] (T.unpack x)
  liftIO $ runX (doc >>> css "#chatLog .message .textInI,.textInRe,.textInSystem" >>> getAttrValue "class") >>= mapM_ putStrLn
  liftIO $ runX (doc >>> css "#chatLog .message .textInI,.textInRe,.textInSystem" >>> getAttrValue "class" &&& Text.XML.HXT.Core.deep Text.XML.HXT.Core.getText) >>= mapM_ putStrLn . concat . t2ls

t2l :: (String, String) -> [String]
t2l = (^..each)
t2ls :: [(String, String)] -> [[String]]
t2ls = map (^..each)

sendText :: String -> WD ()
sendText a = do
  textInput <- findElem . ByXPath $ "//*[@id=\"sayField\"]"
  sendKeys (T.pack a) textInput
--  sendButton <- findElem . ByXPath $ "//*[@id=\"sayButton\"]"
--  click sendButton

writeSource :: FilePath -> WD ()
writeSource name = do
  s <- getSource
  liftIO . Data.Text.IO.writeFile name $ s

screenshotWriteFile :: FilePath -> WD ()
screenshotWriteFile name = do
  string <- screenshot
  liftIO . B.writeFile name $ string

