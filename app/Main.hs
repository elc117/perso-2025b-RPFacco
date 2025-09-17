{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Random (randomRIO)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (Text)


palavras :: [Text]
palavras = ["carta","pedra","plano","verde","festa","piano","prato","livro","filho","vento","tigre","canto","noite","falar","amigo","nuvem","risos","corpo","casal","trigo",
            "couro","faixa","grato","poder","rolar","saber","terra","luzir","manha","morro","banho","cesta","campo","pente","moeda","rosto","sonho","dente","certo","olhos",
            "pular","cedro","pedal","trevo","tanto","folha","magro","doido","pouco","beijo","forca","sorte","bolsa","calor","navio","cabra","prego","haste","claro","tempo"]


getRandomPalavra :: IO Text
getRandomPalavra = do
    index <- randomRIO (0, length palavras - 1)
    return $ palavras !! index



main :: IO ()
main = scotty 3000 $ do
    middleware logStdoutDev
    get "/" $ do
        randomPalavra <- liftIO getRandomPalavra  
        text randomPalavra