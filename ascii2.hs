-- Agoston Gergely Vince 521 agim1986
import Data.Char
import Control.Monad

levag :: String -> String
levag = reverse . dropWhile (== ' ') . reverse

v (x:xs) d nr ca
    | d == 0 = (take nr (repeat ca)) ++ (drop nr (x:xs))
    | otherwise = x : (v xs (d-1) nr ca)

f1 xs i nr m ca
    | (nr == 0) = xs
    | otherwise = (take (i-1) xs) ++ [ca] ++ (f1 (drop i xs) (m + 1) (nr-1) m ca)

f2 xs i nr m num
    | (nr == 0) = xs
    | otherwise = (take (i-1) xs) ++ [num] ++ (f2 (drop i xs) (m - 1) (nr-1) m num)

main :: IO ()
main = do
    input_line <- getLine
    let w = read input_line :: Int
    input_line <- getLine
    let h = read input_line :: Int
    input_line <- getLine
    let d = read input_line :: Int

    let n = 1 + h + d
    let m = w*2 + h + d

    if (w==1 && d==1)
        then do let arr1 =  f2 (take (n*m) (repeat ' ')) ((h+1)*m+d+h) d m '⠌'
                let arr2 = f1 arr1 ((2*m)+d+2) (h-1) m '⠡'
                let arr3 = v  arr2 d (2*w) '_'
                let arr4 = v arr3 (h*m+d+h) (2*w-2) '.'
                let arr5 = f1 arr4 (2*m-h+1) h m '\\'
                let arr6 = v arr5 (m*d) (2*w) '_'
                let arr7 = f1 arr6 (m*(d+1)+1) h m '\\'
                let arr8 = f2 arr7 (m+d) d m '/'
                let arr9 = f2 arr8 ((2+h)*m) d m '/'
                let arr10 = v arr9 (m*(n-1) + h) (2*w) '_'
                let arr11 = f1 arr10 (m*(d+1)+ 1 + 2*w) h m '\\'
                let arr = f2 arr11 (2*m-h) d m '/'
                sor <- forM [0..(n-1)] $ \i -> do
                    let n = levag ( take m (drop (i*m) arr) )
                    return n
                mapM putStrLn sor
    else if (w==1)
        then do let arr1 =  f2 (take (n*m) (repeat ' ')) ((h+1)*m+d+h) d m '⠌'
                let arr2 = f1 arr1 ((2*m)+d+2) (h-1) m '⠡'
                let arr3 = v  arr2 d (2*w) '_'
                let arr4 = v arr3 (h*m+d+h) (2*w) '.'
                let arr5 = f1 arr4 (2*m-h+1) h m '\\'
                let arr6 = v arr5 (m*d) (2*w) '_'
                let arr7 = f1 arr6 (m*(d+1)+1) h m '\\'
                let arr8 = f2 arr7 (m+d) d m '/'
                let arr9 = f2 arr8 ((2+h)*m) d m '/'
                let arr10 = v arr9 (m*(n-1) + h) (2*w) '_'
                let arr11 = f1 arr10 (m*(d+1)+ 1 + 2*w) h m '\\'
                let arr = f2 arr11 (2*m-h) d m '/'
                sor <- forM [0..(n-1)] $ \i -> do
                    let n = levag ( take m (drop (i*m) arr) )
                    return n
                mapM putStrLn sor
    else if (h==1)
        then do let arr2 = f1 (take (n*m) (repeat ' ')) (m+d+1) h m '⠡'
                let arr3 = v  arr2 d (2*w) '_'
                let arr4 = v arr3 (h*m+d+h) (2*w) '.'
                let arr5 = f1 arr4 (2*m-h+1) h m '\\'
                let arr6 = v arr5 (m*d) (2*w) '_'
                let arr7 = f1 arr6 (m*(d+1)+1) h m '\\'
                let arr8 = f2 arr7 (m+d) d m '/'
                let arr9 = f2 arr8 ((2+h)*m) d m '/'
                let arr10 = v arr9 (m*(n-1) + h) (2*w) '_'
                let arr11 = f1 arr10 (m*(d+1)+ 1 + 2*w) h m '\\'
                let arr12 =  f2 arr11 ((h+1)*m+d+h) (d-1) m '⠌'
                let arr = f2 arr12 (2*m-h) d m '/'
                sor <- forM [0..(n-1)] $ \i -> do
                    let n = levag ( take m (drop (i*m) arr) )
                    return n
                mapM putStrLn sor
    else if (d==1)
        then do let arr2 = f1 (take (n*m) (repeat ' ')) (m+d+1) h m '⠡'
                let arr3 = v  arr2 d (2*w) '_'
                let arr4 = v arr3 (h*m+d+h) (2*w-2) '.'
                let arr5 = f1 arr4 (2*m-h+1) h m '\\'
                let arr6 = v arr5 (m*d) (2*w) '_'
                let arr7 = f1 arr6 (m*(d+1)+1) h m '\\'
                let arr8 = f2 arr7 (m+d) d m '/'
                let arr9 = f2 arr8 ((2+h)*m) d m '/'
                let arr10 = v arr9 (m*(n-1) + h) (2*w) '_'
                let arr11 = f1 arr10 (m*(d+1)+ 1 + 2*w) h m '\\'
                let arr12 =  f2 arr11 ((h+1)*m+d+h) d m '⠌'
                let arr = f2 arr12 (2*m-h) d m '/'
                sor <- forM [0..(n-1)] $ \i -> do
                    let n = levag ( take m (drop (i*m) arr) )
                    return n
                mapM putStrLn sor      
        else do let arr1 = v (take (n*m) (repeat ' ')) d (2*w) '_'
                let arr2 = v arr1 (h*m+d+h) (2*w) '.'
                let arr3 = f1 arr2 (2*m-h+1) h m '\\'
                let arr4 = v arr3 (m*d) (2*w) '_'
                let arr5 = f1 arr4 (m*(d+1)+1) h m '\\'
                let arr6 = f2 arr5 (m+d) d m '/'
                let arr7 = f2 arr6 ((2+h)*m) d m '/'
                let arr8 = v arr7 (m*(n-1) + h) (2*w) '_'
                let arr9 =  f2 arr8 ((h+1)*m+d+h) d m '⠌'
                let arr10 = f1 arr9 (m+d+1) h m '⠡'
                let arr11 = f1 arr10 (m*(d+1)+ 1 + 2*w) h m '\\'
                let arr = f2 arr11 (2*m-h) d m '/'
                sor <- forM [0..(n-1)] $ \i -> do
                    let n = levag ( take m (drop (i*m) arr) )
                    return n
                mapM putStrLn sor

    return ()