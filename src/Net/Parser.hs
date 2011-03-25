{-# LANGUAGE TypeSynonymInstances #-}

module Net.Parser
    (
    -- * De-/Encoding
      encode
    , decode
    , Message (..)
    , Id
    ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>), string)
import qualified Text.ParserCombinators.Parsec as P

-- Every Monad is an Applicative.
instance Applicative (GenParser s a) where
    pure = return
    (<*>) = ap

-- Every MonadPlus is an Alternative.
instance Alternative (GenParser s a) where
    empty = mzero
    (<|>) = mplus

type Id = Int
type Pos = (Int,Int)

-- | Message data for network traffic
data Message = Put Id Pos
             | Size Int
             -- Responses
             | Ok (Maybe Id)
             | No (Maybe Id)
             | What
             -- connection stuff
             | Hello
             | Start
             | Bye
             -- messaging
             | Msg String
             deriving Show

-- | Encode a Message
encode :: Message -> String
encode (Put id (x,y))   = "PUT #" ++ show id ++ " (" ++ show x ++ "," ++ show y ++ ")"
encode (Size x)         = "SIZE " ++ show x

encode (Ok id)          = "OK" ++ maybe "" (\i -> " #" ++ show i) id
encode (No id)          = "NO" ++ maybe "" (\i -> " #" ++ show i) id

encode What             = "WHAT?"
encode Hello            = "HELLO"
encode Start            = "START"
encode Bye              = "BYE"
encode (Msg s)          = "MSG " ++ s



-- | Decode a String as a Message
decode :: String -> Message
decode s = case parse msgParser "decode" s of
                Left err -> What
                Right ms -> ms



-- | Parse everything
msgParser :: Parser Message
msgParser = hi <|> go <|> bye <|> ok <|> no <|> put <|> size <|> msg

-- Status/Welcome messages
hi   = const Hello <$> string "HELLO"
go   = const Start <$> string "START"
bye  = const Bye   <$> string "BYE"
msg  = Msg         <$> (string "MSG " *> many1 anyChar)

-- Responses
ok      = Ok   <$> (string "OK" *> maybeId)
no      = No   <$> (string "NO" *> maybeId)
maybeId = (space *> char '#' *> (Just . read) `fmap` many1 digit) <|> return Nothing

-- Game messages
size = Size <$> (string "SIZE " *> read `fmap` many1 digit)
put  = Put  <$> (string "PUT #" *> read `fmap` many1 digit <* space)
            <*> pair

pair = (,) <$> (char '(' *> read `fmap` many1 digit)
           <*> (char ',' *> read `fmap` many1 digit <* char ')')

-- Helper for parsing the whole command first
string = try . P.string
