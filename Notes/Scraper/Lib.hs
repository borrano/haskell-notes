{-# LANGUAGE RecordWildCards #-}

module Scraper.Lib where

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.Async.Pool
import Control.Exception (SomeException (SomeException), catch)
import Control.Monad (liftM2)
import Control.Monad.Cont (liftIO)
import Control.Monad.Trans.Maybe
import Data.Char
import Data.Either
import Data.Foldable
import Data.List (group, sortBy, sortOn, unfoldr)
import Data.Maybe (maybeToList)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Read
import Debug.Trace
import GHC.Generics
import Text.HTML.Scalpel
import Text.Read (readMaybe)
import TextShow

-- getNumPages = maybe 0 id <$> scrapeURL ("https://www.dermoailem.com/takviye-edici-gidalar") go
--  where
--    go :: Scraper T.Text Int
--    go = chroot ("div" @: [hasClass "pageNumber"]) $ do
--      x <- (maximum . fmap fst . (rights) . fmap (decimal)) <$> texts "a"
--      return x

data Entry = Entry
  { brand :: T.Text,
    name :: T.Text,
    price :: Double,
    imgURL :: T.Text,
    productURL :: T.Text
  }
  deriving (Show)

showtext (Entry i1 i2 i3 i4 i5) = i1 <> " " <> i2 <> " " <> showt i3 <> " " <> i4 <> " " <> i5

getPrice :: T.Text -> Double
getPrice = either (\_ -> 0) fst <$> (double . T.dropAround (not . isDigit))

process filename f xs = withTaskGroup 10 $ \g -> scatterFoldMapM g (getPage <$> xs) (go)
  where
    go (Right xs) = T.appendFile filename (T.unlines ((T.pack . show) <$> xs))
    go _ = return ()
    getPage url = (concat . maybeToList) <$> scrapeURL (T.unpack url) f `catch` (\(e :: SomeException) -> return Nothing)

sites :: [(T.Text, FilePath, Int, Scraper T.Text [Entry])]
sites =
  [ -- ("https://www.dermoailem.com/takviye-edici-gidalar?sayfa=", "dermo", 1, dermoailem),
    ("https://www.trendyol.com/gida-takviyeleri-vitaminler-x-c105085?pi=", "tyol", 1, trendyol)
  ]

-- "p-card-wrppr" -- "prdct-desc-cntnr-ttl-w" -- span prdct-desc-cntnr-ttl -> brand
-- "p-card-wrppr" -- "prdct-desc-cntnr-ttl-w" -- span prdct-desc-cntnr-name -> name
-- "p-card-wrppr" -- "prc-box-dscntd"   -> price

dermoailem :: Scraper T.Text [Entry]
dermoailem = chroots ("div" @: [hasClass "productDetail"]) $ do
  brand <- text $ "div" @: [hasClass "productMarka"]
  name <- chroot ("div" @: [hasClass "productName"]) $ text "a"
  price <- getPrice <$> (chroot ("div" @: [hasClass "discountPrice"]) $ text "span")
  imgURL <- text $ "span"
  productURL <- text $ "span"

  return Entry {..}

trendyol :: Scraper T.Text [Entry]
trendyol = chroots ("div" @: [hasClass "p-card-wrppr"]) $ do
  brand <- text $ "span" @: [hasClass "prdct-desc-cntnr-ttl"]
  name <- text $ "span" @: [hasClass "prdct-desc-cntnr-name"]
  price <- getPrice <$> text ("div" @: [hasClass "prc-box-dscntd"])
  imgURLs <- chroots ("div" @: [hasClass "p-card-img-wr"]) $ attr "src" $ "img" @: [hasClass "p-card-img"]
  productURL <- chroot ("div" @: [hasClass "p-card-chldrn-cntnr"]) $ attr "href" $ "a"
  let imgURL = ""
  return $ Entry {..}

-- js-image-zoom__zoomed-image
x :: Scraper T.Text [T.Text]
x = chroots ("div" @: [hasClass "gallery-container"]) $ do
  attr "src" $ ("img")

test = forM_ sites $ \(basePath, name, maxPages, f) -> do
  T.writeFile name ""
  process name f (((basePath <>) . showt) <$> [1 .. maxPages])

ax = process "x" x ["https://www.trendyol.com/easy-fishoil/easyvit-multi-omega-3-cignenebilir-jel-formda-30-tablet-p-34410677"]

-- >>> ax
--

-- >>> test
-- Just (Rectangle {x = 2, y = 1})
--
