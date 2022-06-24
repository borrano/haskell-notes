{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Aeson.Tutorial where

import Control.Applicative
import Control.Arrow
import Data.Aeson
import Data.Aeson.Key as K
import Data.Aeson.Key qualified
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Char (toLower)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Text.Lazy.Encoding qualified as T
import Data.Text.Lazy.IO qualified as T
import Data.Vector qualified as V
import GHC.Exts
import GHC.Generics

-- >>>  encode [(1 :: Int), 2, 3]
-- "[1,2,3]"
--
-- >>> decode "[1,2,3]" :: Maybe [Integer]
-- Just [1,2,3]
--
-- >>> decode "foo" :: Maybe [Integer]
-- Nothing
--

-- >>> eitherDecode "[1,2,[3,true]]" :: Either String (Int, Int, (Int, Bool))
-- Right (1,2,(3,True))
--

val :: Value
val = Object $ fromList [("numbers", Array $ fromList [Number 1, Number 2, Number 3]), ("boolean", Bool True)]

-- >>> val
-- Object (fromList [("boolean",Bool True),("numbers",Array [Number 1.0,Number 2.0,Number 3.0])])
--

-- >>> encode val
-- "{\"boolean\":true,\"numbers\":[1,2,3]}"
--
-- >>> T.putStrLn . T.decodeUtf8 . encode $ val
-- {"boolean":true,"numbers":[1,2,3]}
--
val2 :: Value
val2 = object [("numbers" .= (Array $ fromList [Number 1, Number 2, Number 3])), ("stf" .= (String "abc")), ("boolean" .= Bool True)]

-- >>> val2
-- Object (fromList [("boolean",Bool True),("numbers",Array [Number 1.0,Number 2.0,Number 3.0])])
--

revStrings :: Value -> Value
revStrings (String x) = String (T.reverse x)
revStrings (Array x) = Array (fmap revStrings x)
revStrings (Object x) =
  let revPair (k, v) = (K.fromText $ T.reverse $ K.toText k, revStrings v)
   in Object . KM.fromList . fmap revPair . KM.toList $ x
revStrings x = x

-- >>> revStrings val2
-- Object (fromList [("fts",String "cba"),("naeloob",Bool True),("srebmun",Array [Number 1.0,Number 2.0,Number 3.0])])
--

parseTuple (Object obj) = do
  a <- case KM.lookup "a" obj of
    Just (String x) -> return (x)
    Just _ -> fail "expected a str"
    Nothing -> fail "no field 'a'"
  b <- case KM.lookup "b" obj of
    Just (Bool x) -> return x
    Just _ -> fail "expected a boolean"
    Nothing -> fail "no field 'b'"
  return (a, b)
parseTuple _ = undefined

parseArray :: Value -> Parser [(T.Text, Bool)]
parseArray = withArray "x" $ \arr -> mapM parseTuple (V.toList arr)

-- parseArray (Array arr) = mapM parseTuple (V.toList arr)
-- parseArray _ = fail "x"

parseEx :: Maybe Value
parseEx = decode $ "[{\"a\":\"hello\", \"b\":true}, {\"a\":\"world\", \"b\":false}]"

parseEx2 = parseEx >>= parseMaybe parseArray

parseTuple2 = withObject "x" $ \obj -> do
  a <- case KM.lookup "a" obj of
    Just x -> parseJSON x
    Nothing -> fail "no field 'a'"
  b <- case KM.lookup "b" obj of
    Just (Bool x) -> return x
    Just _ -> fail "expected a boolean"
    Nothing -> fail "no field 'b'"
  return (a, b)

x ..: key = case KM.lookup key x of
  Just x -> parseJSON x
  Nothing -> fail "no field 'a'"

parseTuple3 = withObject "x" $ \obj -> (,) <$> obj ..: "a" <*> obj ..: "b"

-- >>> parseEx2
-- Just [("hello",True),("world",False)]
--

data Person = Person {name :: String, age :: Int}

instance FromJSON Person where
  parseJSON = withObject "Person" $ \obj -> do
    fn <- obj .: "firstname"
    sn <- obj .: "secondname"
    let name = fn <> sn
    age <- obj .:? "age" .!= (21 :: Int)
    age <- obj .: "age" <|> obj .: "AGE" <|> (pure 21)
    return $ Person {..}

data Shape = Rectangle {x :: Int, y :: Int} | Square {x :: Int} deriving (Show)

instance ToJSON Shape where
  toJSON (Rectangle x y) = object ["tag" .= ("Rectangle" :: T.Text), "x" .= x, "y" .= y]
  toJSON (Square x) = object ["tag" .= ("Square" :: T.Text), "x" .= x]

instance FromJSON Shape where
  parseJSON = withObject "Shape" $ \obj -> do
    (t :: T.Text) <- obj .: "tag"
    case t of
      "Rectangle" -> Rectangle <$> (obj .: "x") <*> (obj .: "y")
      "Square" -> Square <$> (obj .: "x")
      _ -> fail "unknown"

-- >>> encode (Rectangle 2 1)
-- "{\"tag\":\"Rectangle\",\"x\":2,\"y\":1}"
--
a :: Maybe Shape
a = decode ("{\"tag\":\"Rectangle\",\"x\":2,\"y\":1}")

-- >>> a
-- Just (Rectangle {x = 2, y = 1})
--

----------------------------------

-- EX1 : parse following to flat data
-- {
--    "name":"Nightfall",
--    "author":{
--        "name":"Isaac Asimov",
--        "born":1920
--    }
-- }
data Story = Story
  { sname :: String,
    sauthor :: String,
    sauthorBorn :: Int
  }

instance FromJSON Story where
  parseJSON = withObject "story" $ \o -> do
    sname <- o .: "name"
    authorO <- o .: "author"
    sauthor <- authorO .: "name"
    sauthorBorn <- authorO .: "born"
    return Story {..}

-------------------------
-- EX2  : parse flat data to nested
-- {
--    "name":"Сергей",
--    "patronymic":"Михайлович",
--    "surname":"Брин"
-- }

data Name = Name
  { name :: String,
    surname :: String
  }

data RussianName = RussianName
  { russianName :: Name,
    russianPatronymic :: String
  }

instance FromJSON Name where
  parseJSON = withObject "NAME" $ \obj -> Name <$> (obj .: "name") <*> (obj .: "surname")

instance ToJSON Name where
  toJSON (Name {..}) = object [("name" .= name), ("surname" .= surname)]

instance FromJSON RussianName where
  parseJSON = withObject "NAME" $ \obj -> do
    russianName <- parseJSON (Object obj)
    russianPatronymic <- obj .: "patronymic"
    return $ RussianName {..}

instance ToJSON RussianName where
  toJSON (RussianName {..}) = case toJSON russianName of
    (Object xs) -> Object $ (KM.fromList ["patronymic" .= russianPatronymic] <> xs)
    _ -> error "x"

------------------
-- UNKNOWN field names
-- {
--    "website1.com": {
--        "/page1": 3,
--        "/page2": 4
--    },
--    "website2.com": {
--        "/page": 10
--    }
-- }
data Referer = Referer
  { domain :: T.Text,
    pathAccesses :: [(T.Text, Int)]
  }
  deriving (Show)

parse :: Value -> Parser [Referer]
parse v = do
  km <- parseJSON v
  return $ fmap (\(k, v) -> Referer k (HM.toList v)) $ HM.toList km

-------------------------
--
-- drop underscore, case insensitive
data P = P
  { _name :: String,
    _age :: Int
  }
  deriving (Generic)

instance ToJSON P where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = drop 1
        }

instance FromJSON P where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = map toLower . drop 1
        }

data P2 = P2
  { name :: String,
    age :: Int
  }

deriveJSON defaultOptions ''P2