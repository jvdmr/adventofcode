module Main where

-- import Data.List

magic = 20201227

loopCrypto sub val = mod (val * sub) magic

findLoopSize n sub val pubKey | pubKey == val = (n, pubKey)
                              | otherwise = findLoopSize (n + 1) sub (loopCrypto sub val) pubKey

encryptionKey ((cardLoop, cardKey):(doorLoop, doorKey):[]) | enc1 == enc2 = enc1
                                                           | otherwise = error $ "encryption keys do not match! enc1 = " ++ show enc1 ++ ", enc2 = " ++ show enc2
                                                           where enc1 = iterate (loopCrypto doorKey) doorKey !! (cardLoop - 1)
                                                                 enc2 = iterate (loopCrypto cardKey) cardKey !! (doorLoop - 1)

main = do
  cnt <- getContents
  print $ map (findLoopSize 1 7 7 . read) $ lines cnt
  print $ encryptionKey $ map (findLoopSize 1 7 7 . read) $ lines cnt

