module Net.Parser
    (
    -- * De-/Encoding
      encode
    , decode
    , Message (..)
    ) where

import Control.Applicative
import Control.Arrow
import Control.Monad (MonadPlus(..), ap)
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

-- | Message data for network traffic
data Message = Put  (Int,Int)
             | Size (Int,Int)
             | Ok
             | No
             | What
             -- connection stuff
             | Hello
             | Bye
             -- messaging
             | Msg String
             deriving Show

-- | Encode a Message
encode :: Message -> String
encode (Put (x,y))  = "PUT ("  ++ show x ++ "," ++ show y ++ ")"
encode (Size (x,y)) = "SIZE (" ++ show x ++ "," ++ show y ++ ")"
encode Ok           = "OK"
encode No           = "NO"
encode What         = "WHAT"
encode Hello        = "HELLO"
encode Bye          = "BYE"
encode (Msg s)      = "MSG " ++ s

-- | Decode a String as a Message
decode :: String -> Message
decode s = case parse msgParser "decode" s of
                Left err -> What
                Right ms -> ms

-- | Parse everything
msgParser :: Parser Message
msgParser = hi <|> bye <|> ok <|> no <|> put <|> size <|> msg

-- Value parser
hi   = const Hello <$> string "HELLO"
bye  = const Bye   <$> string "BYE"
ok   = const Ok    <$> string "OK"
no   = const No    <$> string "NO"
msg  = Msg         <$> (string "MSG "  *> many1 anyChar)
put  = Put         <$> (string "PUT "  *> pair)
size = Size        <$> (string "SIZE " *> pair)

pair = (read *** read) <$> ((,) <$> (char '(' *> many1 digit)
                                <*> (char ',' *> many1 digit <* char ')'))

-- Helper for parsing the whole command first
string = try . P.string
