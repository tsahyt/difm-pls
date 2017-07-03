{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types (fieldLabelModifier, defaultOptions)
import Data.Char (toLower)
import Data.Conduit
import Data.List (intersperse)
import Data.Monoid
import Data.String
import GHC.Generics
import Network.HTTP.Simple
import Network.HTTP.Types
import Text.Printf

import Options.Applicative
import System.IO

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Conduit.List as CL

onHead :: (a -> a) -> [a] -> [a]
onHead _ []     = []
onHead f (x:xs) = f x : xs

data PlaylistInfo = PlaylistInfo
    { piId       :: Int
    , piKey      :: String
    , piName     :: String
    , piPlaylist :: String
    }
    deriving (Eq, Show, Ord, Read, Generic)

instance FromJSON PlaylistInfo where
    parseJSON = genericParseJSON $ defaultOptions
        { fieldLabelModifier = onHead toLower . drop 2 }

instance ToJSON PlaylistInfo where
    toJSON = genericToJSON $ defaultOptions
        { fieldLabelModifier = onHead toLower . drop 2 }

premiumReq :: Service -> String -> PlaylistInfo -> Request
premiumReq s lkey x = 
    fromString $ printf (serviceUrl s) (piKey x) lkey

data PlaylistEntry = PlaylistEntry
    { peFile   :: B.ByteString
    , peTitle  :: B.ByteString
    , peLength :: B.ByteString }
    deriving Show

plsToLinks :: B.ByteString -> [PlaylistEntry]
plsToLinks = triple . init . drop 2 . B.lines
    where triple (f : t : l : xs) = go f t l : triple xs
          triple [] = []
          triple _  = error "Malformed playlist file!"

          go f t l =
              let strip = B.tail . B.dropWhile (/= '=')
               in PlaylistEntry (strip f) (strip t) (strip l)

buildPls :: [PlaylistEntry] -> B.Builder
buildPls xs =
    let es = mconcat . intersperse "\n" . map (uncurry go) . zip [1..] $ xs
     in mconcat [ "[playlist]" <> B.char8 '\n'
                , es <> B.char8 '\n'
                , "NumberOfEntries=" <> (B.intDec . length $ xs) <> B.char8 '\n'
                , "Version=2" ]

    where go i e =
              let format x y = B.lazyByteString x <> B.intDec i 
                            <> B.char8 '=' <> B.lazyByteString y
               in mconcat [ format "File" (peFile e) <> B.char8 '\n'
                          , format "Title" (peTitle e) <> B.char8 '\n'
                          , format "Length" (peLength e) ]

run :: Options -> IO ()
run opts = do
    hSetBuffering stdout NoBuffering
    list <- getResponseBody <$> httpJSON 
                (fromString . serviceIndex . service $ opts)

    links <- runConduit 
           $ CL.sourceList list
          .| CL.mapM fetch
          .| CL.map (head . plsToLinks . getResponseBody)
          .| CL.consume

    let output = B.toLazyByteString . buildPls $ links
    
    B.writeFile (outFile opts) output

    where fetch x = do
              liftIO $ putStr ("Fetching " ++ piName x ++ "... ")
              res <- case listenKey opts of
                         Nothing -> httpLBS . fromString . piPlaylist $ x
                         Just k  -> httpLBS . premiumReq (service opts) k $ x
              liftIO . putStrLn $ 
                  case statusCode . getResponseStatus $ res of
                      200 -> "OK!"
                      403 -> "Forbidden!"
                      _   -> "Error!\n" ++ show (getResponseBody res)
              return res

data Service 
    = DigitallyImported
    | SkyFM
    | RockRadio
    | JazzRadio
    deriving (Show, Eq)

serviceIndex :: Service -> String
serviceIndex DigitallyImported = "http://listen.di.fm/public3"
serviceIndex SkyFM = "http://listen.sky.fm/public3"
serviceIndex RockRadio = "http://listen.rockradio.com/public3"
serviceIndex JazzRadio = "http://listen.jazzradio.com/public3"

serviceUrl :: Service -> String
serviceUrl DigitallyImported = "http://listen.di.fm/premium_high/%s.pls?%s"
serviceUrl SkyFM = "http://listen.radiotunes.com/premium_high/%s.pls?%s"
serviceUrl RockRadio = "http://listen.rockradio.com/premium_high/%s.pls?%s"
serviceUrl JazzRadio = "http://listen.jazzradio.com/premium_high/%s.pls?%s"

serviceOpt :: ReadM Service
serviceOpt = maybeReader $ \case
    "di.fm"          -> Just DigitallyImported
    "sky.fm"         -> Just SkyFM
    "radiotunes.com" -> Just SkyFM
    "rockradio.com"  -> Just RockRadio
    "jazzradio.com"  -> Just JazzRadio
    _                -> Nothing
             
data Options = Options
    { listenKey :: Maybe String
    , service   :: Service 
    , outFile   :: FilePath }
    deriving (Show, Eq)

options :: Parser Options
options = Options 
    <$> optional (strOption
            ( short 'k'
           <> help "Listen key" ))
    <*> option serviceOpt
            ( short 's'
           <> help "Service (di.fm, sky.fm, radiotunes.com) - Default: di.fm"
           <> value DigitallyImported )
    <*> strOption
            ( short 'o'
           <> help "Output file" )

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Fetch di.fm premium playlists"
     <> header "difm-pls - di.fm premium playlists" )
