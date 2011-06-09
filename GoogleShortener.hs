{-#LANGUAGE FlexibleContexts #-}
{- Author: Kevin Chen
-}

module Network.GoogleShortener
where

import Data.List
import Data.Char
import Control.Monad
import System.IO
import System.Directory
import System.Environment
import System.Random
import System.Time

import Control.Failure
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Network.HTTP.Enumerator
import qualified Network.Wai as Wai

import Text.JSON
import Text.JSON.Types 



main :: IO ()
main = do print 10


data GoogleShortenerRsp = GoogleShortenerRsp {kind :: String, shortUrl :: String, longUrl :: String}  deriving (Show)


instance JSON GoogleShortenerRsp where
    readJSON (JSObject o) = Ok GoogleShortenerRsp {
                kind = field o "kind",
                shortUrl = field o "id",
                longUrl = field o "longUrl"
    }

--Actually, this is not used in current program, but just write this as a practice.
    showJSON GoogleShortenerRsp {kind = k, shortUrl = short, longUrl = long} = makeObj [ ("kind", JSString $ toJSString k), ("id", JSString (toJSString short)), ( "longUrl", JSString $ toJSString long)]
    

--Kind of a trick of JSString and JSObject's type, use it carefully.
field o s = case get_field  o  s of 
    Nothing -> error "invalid field " ++ s 
    Just (JSString a) -> fromJSString a 


getPOSTRequest :: (Control.Failure.Failure HttpException m) => [Char] -> m Request
getPOSTRequest str = do
        req0  <- parseUrl "https://www.googleapis.com/urlshortener/v1/url"
        return req0 {method = S.pack "POST"
        , requestHeaders = [(Wai.mkCIByteString (S.pack "Content-Type") , S.pack "application/json")]
        , requestBody =  L.pack $  "{\"longUrl\": \"" ++ str ++ "\"}"
        }

--No surprise below.
shortener :: String -> IO GoogleShortenerRsp
shortener str =  
        getPOSTRequest str >>=
        httpLbs >>= \rsp -> do   
        let s = L.unpack $ responseBody  rsp
        case resultToEither $ decode s of
            Right r -> return r
            Left e -> error e


expander :: String -> IO GoogleShortenerRsp
expander str = do
        s <- fmap L.unpack (simpleHttp $ "https://www.googleapis.com/urlshortener/v1/url?shortUrl=" ++ str)
        case decode s of 
            Ok r -> return r
            Error e -> error e

