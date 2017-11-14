module Filters where

import LinkStruct

-- http://hackage.haskell.org/package/base-4.10.0.0/docs/Data-Maybe.html#v:fromJust
import Data.Maybe (fromJust)
-- http://hackage.haskell.org/package/base-4.8.2.0/docs/Data-List.html
import Data.List ( find ) -- , intersperse , nub)
-- http://hackage.haskell.org/package/http-client
-- http://hackage.haskell.org/package/http-client-tls
-- https://github.com/snoyberg/http-client
import Network.HTTP.Simple ( Response )
import Network.HTTP.Client ( responseBody )
-- https://hackage.haskell.org/package/tagsoup
-- https://github.com/ndmitchell/tagsoup
import Text.HTML.TagSoup

titlesFilter :: [Tag String] -> [Tag String]
titlesFilter tags = takeWhile (~/= TagClose ("title" :: String))
                  $ dropWhile (~/= TagOpen ("title" :: String) []) tags

linksFilter :: [Tag String] -> [[Tag String]]
-- linksFilter tags = filter (~== TagOpen ("a" :: String) [("href", "")]) tags
-- (take 3) is only the canonical link
-- linksFilter tags = map (take 3) (sections (~== ("<a>" :: String)) tags)
linksFilter tags = map (takeWhile (~/= TagClose ("a" :: String))) (sections (~== ("<a>" :: String)) tags)

-- The 'unwords . words' deletes all multiple spaces, replaces
-- tabs and newlines with spaces and trims the front and back
--
-- Let vs. where
-- https://wiki.haskell.org/Let_vs._Where
extractTitles :: (Show body) => Response body -> IO String
extractTitles r = do
    let tags = parseTags $ show (responseBody r)
    let contents = map f $ filter isTagText $ titlesFilter tags
          -- this where clause is part of the "contents" definition (thus indented)
          where f :: Tag String -> String
                f = unwords . words . fromTagText
    return $ unwords contents

-- | mapInd : map with index (as second argument of the function f)
mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]

extractLinks :: (Show body) => Response body -> IO [String]
extractLinks r = do
    let tags = parseTags $ show (responseBody r)
    let contents = map f $ linksFilter tags
          where f :: [Tag String] -> String
                f [tOpen, tText, tClose] | isBasicStruct tOpen tText tClose =
                                           show $ linkStruct 0
                                                (fromAttrib "href" tOpen)
                                                (fromTagText tText)
                f (tOpenA:tOpenImg:_rest) | isLinkAndImgStruct tOpenA tOpenImg =
                                           show $ linkStruct 0
                                                (fromAttrib "href" tOpenA)
                                                (fromAttrib "alt" tOpenImg)
                -- ViewPatterns
                -- f (hd:(reverse -> (tl:_))) | isTagOpenName "a" hd && isTagText tl
                -- Head&Last
                -- f (h:tgs) | isTagOpenName "a" h && isTagText (last tgs) =
                -- Finding a tagText
                f (h:tgs) | isLinkAndMixedStruct h tgs =
                        show $ linkStruct 0
                             (fromAttrib "href" h)
                             (fromTagText (fromJust (find isTagText tgs)))
                f raw = "ERROR: cannot parse " ++ show raw

                isBasicStruct :: Tag String -> Tag String -> Tag String -> Bool
                isBasicStruct tO tT tC = isTagOpenName "a" tO && isTagText tT && isTagCloseName "a" tC

                isLinkAndImgStruct :: Tag String -> Tag String -> Bool
                isLinkAndImgStruct tOa tOi = isTagOpenName "a" tOa && isTagOpenName "img" tOi

                isLinkAndMixedStruct :: Tag String -> [Tag String] -> Bool
                isLinkAndMixedStruct tO tgs = isTagOpenName "a" tO && any isTagText tgs

    return contents
