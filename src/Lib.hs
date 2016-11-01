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
import Test.WebDriver.Common.Keys
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
  cs <- cookies
  liftIO $ mapM_ print cs
  openPage "http://chatpad.jp/"
--  cs <- cookies
--  liftIO $ mapM_ print cs
  e <- findElem . ByXPath $ "//*[@id=\"body\"]/div[2]/div[1]/div/div[4]/a"
  click e
  u <- getCurrentURL
  liftIO $ putStrLn u
  getSource >>= printMessages . T.unpack
  waitUntil 60 $ findElem (ByXPath [here|//*[@id="chatLog"]/div/div[2][contains(., "チャットを始めるよー！")]|])
  liftIO $ threadDelay 2000
  sendText $ "こんにちは" ++ (T.unpack enter)
  getSource >>= printMessages . T.unpack
--  return $ getSource >>= print
  getSource >>= printMessages . T.unpack
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

printMessages :: String -> WD ()
printMessages x = do
  let doc = readString [withParseHTML yes, withWarnings no] x
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

