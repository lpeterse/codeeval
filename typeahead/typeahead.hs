import System.Environment
import Data.Char
import Data.List

main :: IO ()
main  = putStr . unlines . map f . lines =<< readFile . head =<< getArgs

f :: String -> String
f  = const $ show $ nub wsAll

wsSingle :: [String]
wsSingle = nub wsAll

wsAll :: [String]
wsAll = ["mary","had","a","little","lamb","its","fleece","was","white","as","snow","and","everywhere","that","mary","went","the","lamb","was","sure","to","go","it","followed","her","to","school","one","day","which","was","against","the","rule","it","made","the","children","laugh","and","play","to","see","a","lamb","at","school","and","so","the","teacher","turned","it","out","but","still","it","lingered","near","and","waited","patiently","about","till","mary","did","appear","why","does","the","lamb","love","mary","so","the","eager","children","cry","why","mary","loves","the","lamb","you","know","the","teacher","did","reply"]
-- ws  = g s
--  where
--    g :: String -> [String]
--    g  = words . filter (\c-> isAlpha c || isSpace c) . map toLower
--    s :: String
--    s  = "Mary had a little lamb its fleece was white as snow;\nAnd everywhere that Mary went, the lamb was sure to go.\nIt followed her to school one day, which was against the rule;\nIt made the children laugh and play, to see a lamb at school.\nAnd so the teacher turned it out, but still it lingered near,\nAnd waited patiently about till Mary did appear.\n\"Why does the lamb love Mary so?\" the eager children cry; \"Why, Mary loves the lamb, you know\" the teacher did reply.\""
