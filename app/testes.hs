{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Random (randomRIO)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO


carregarPalavras :: IO [T.Text]
carregarPalavras = do
    conteudo <- TIO.readFile "palavras.txt"
    return (T.lines conteudo)


getRandomPalavra :: [T.Text] -> IO T.Text
getRandomPalavra lista = do
    index <- randomRIO (0, length lista - 1)
    return $ lista !! index


comparaPalavra :: T.Text -> T.Text -> Bool
comparaPalavra tentativa resposta = tentativa == resposta


receberPalavra :: [T.Text] -> T.Text -> IO T.Text
receberPalavra lista tentativa
    | T.length tentativa /= 5 = return "A palavra precisa ter 5 letras!"
    | otherwise = do
        resposta <- getRandomPalavra lista
        if comparaPalavra tentativa resposta
           then return "Acertou!"
           else return "Errou!"



main :: IO ()
main = scotty 3000 $ do
    middleware logStdoutDev
    get "/" $ do
        lista <- liftIO carregarPalavras
        randomPalavra <- liftIO (getRandomPalavra lista)
        text randomPalavra
