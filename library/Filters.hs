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

-- λ> takeWhile (~/= TagClose ("title" :: String)) $ dropWhile (~/= TagOpen ("title" :: String) []) ttt
-- [TagOpen "title" [],TagText "tagsoup/Sample.hs at master \183 ndmitchell/tagsoup"]

-- λ> sections (~== ("<title>" :: String)) ttt
-- [[TagOpen "title" [],TagText "tagsoup/Sample.hs at master ... ,TagClose "title",TagText " ",TagOpen "link"
-- ... ion+xml"),("href","/opensearch.xml"),("title","GitHub")],
titlesFilter :: [Tag String] -> [Tag String]
titlesFilter tags = takeWhile (~/= TagClose ("title" :: String))
                  $ dropWhile (~/= TagOpen ("title" :: String) []) tags

-- let ttt = readFile "./parseThis.html"
-- let t = fmap parseTags ttt
-- fmap (filter (~== TagOpen "a" [("href", "")])) t
linksFilter :: [Tag String] -> [[Tag String]]
-- linksFilter tags = filter (~== TagOpen ("a" :: String) [("href", "")]) tags
-- (take 3) is only the canonical link
-- linksFilter tags = map (take 3) (sections (~== ("<a>" :: String)) tags)
linksFilter tags = map (takeWhile (~/= TagClose ("a" :: String))) (sections (~== ("<a>" :: String)) tags)

-- λ> map (\t ->  fromTagText (t !! 1)) (sections (~== "<a>") s)
-- ["here","there"]
-- λ> map (\t -> (isTagOpenName "a" (t !! 0), (fromAttrib "href" (t !! 0)), fromTagText (t !! 1), isTagCloseName "a" (t !! 2))) (sections (~== "<a>") s)
-- [(True,"http://haskell.org","here",True),(True,"http://wiki.haskell.org","there",True)]
-- λ> map (\t -> LinkStruct (isTagOpenName "a" (t !! 0)) (fromAttrib "href" (t !! 0)) (fromTagText (t !! 1)) (isTagCloseName "a" (t !! 2))) (sections (~== "<a>") s)
-- [[[http://haskell.org][here]],[[http://wiki.haskell.org][there]]]

dequote :: String -> String
dequote ('\"':xs) | last xs == '\"' = init xs
dequote x = x

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

extractLinks :: (Show body) => Response body -> IO [String]
extractLinks r = do
    let tags = parseTags $ show (responseBody r)
    let contents = map f $ linksFilter tags
          where f :: [Tag String] -> String
                f [tOpen, tText, tClose] | isBasicStruct tOpen tText tClose =
                                           show $ LinkStruct 0
                                                (fromAttrib "href" tOpen)
                                                (fromTagText tText)
                f (tOpenA:tOpenImg:_rest) | isLinkAndImgStruct tOpenA tOpenImg =
                                           show $ LinkStruct 0
                                                (fromAttrib "href" tOpenA)
                                                (fromAttrib "alt" tOpenImg)
                -- ViewPatterns
                -- f (hd:(reverse -> (tl:_))) | isTagOpenName "a" hd && isTagText tl
                -- Head&Last
                -- f (h:tgs) | isTagOpenName "a" h && isTagText (last tgs) =
                -- Finding a tagText
                f (h:tgs) | isLinkAndMixedStruct h tgs =
                        show $ LinkStruct 0
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

-- λ> :m +Text.HTML.TagSoup
-- λ> :m +System.IO
-- λ> (fmap lines $ readFile "./parseThis.html") >>= \c -> mapM_ putStrLn c
-- λ> fmap parseTags (readFile "./parseThis.html")
-- λ> fmap (sections (~== ("<a>" :: String)) . parseTags) (readFile "./parseThis.html")
-- λ> map (take 3) (sections (~== "<a>") (parseTags t))
-- [[TagOpen "a" [("href","src/Text-HTML-TagSoup.html")],TagText "Source",TagClose "a"],[TagOpen "a" [("href","/package/tagsoup-0.14.1")],TagText "Contents",TagClose "a"],[TagOpen "a" [("href","doc-index.html")],TagText "Index",TagClose "a"]]
-- λ> fmap (map (take 3)) (fmap (sections (~== ("<a>" :: String)) . parseTags) (readFile "./parseThis.html"))
-- λ> fmap (map (\(a:b:c) -> LinkStruct 0 (fromAttrib "href" a) (fromTagText b) True True)) tt
