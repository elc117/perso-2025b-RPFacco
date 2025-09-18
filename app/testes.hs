{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Random (randomRIO)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Data.IORef

-- carregar palavras do .txt
carregarPalavras :: IO [T.Text]
carregarPalavras = do
    conteudo <- TIO.readFile "palavras.txt"
    return (T.lines conteudo)

-- seleciona uma palavra aleatoria
getRandomPalavra :: [T.Text] -> IO T.Text
getRandomPalavra lista = do
    index <- randomRIO (0, length lista - 1)
    return $ lista !! index

-- compara palpite com resposta
comparaPalavra :: T.Text -> T.Text -> Bool
comparaPalavra tentativa resposta = tentativa == resposta

-- gera dicas a partir de um palpite
gerarDicas :: T.Text -> T.Text -> (T.Text, T.Text, T.Text)
gerarDicas tentativa resposta =
    let certas   = [c | (c,r) <- T.zip tentativa resposta, c == r]
        erradas  = [c | c <- T.unpack tentativa, c `elem` T.unpack resposta, c `notElem` certas]
        ausentes = [c | c <- T.unpack tentativa, c `notElem` T.unpack resposta]
    in (T.pack certas, T.pack erradas, T.pack ausentes)

-- controla limite de tentativas
checarTentativas :: Int -> Bool
checarTentativas n = n < 6

-- recebe um palpite e retorna mensagem com dicas
receberPalpite :: T.Text -> T.Text -> Int -> T.Text
receberPalpite tentativa resposta tentativas
    | T.length tentativa /= 5 = "A palavra precisa ter 5 letras!"
    | comparaPalavra tentativa resposta = "Acertou!"
    | tentativas >= 5 = "Errou! A resposta era: " <> resposta
    | otherwise =
        let (certas, erradas, ausentes) = gerarDicas tentativa resposta
        in "Letras em posicao certa: " <> certas
        <> " | Letras em posicao errada: " <> erradas
        <> " | Letras ausentes: " <> ausentes


jogar :: T.Text -> Int -> IO ()
jogar resposta tentativa
    | tentativa >= 6 = putStrLn $ "Você perdeu! A resposta era: " ++ T.unpack resposta
    | otherwise = do
        putStrLn "Digite uma palavra de 5 letras:"
        palpite <- getLine
        if length palpite /= 5
           then do
               putStrLn "A palavra precisa ter 5 letras! Tente novamente."
               jogar resposta tentativa  -- não incrementa
           else do
               let resultado = receberPalpite (T.pack palpite) resposta tentativa
               putStrLn (T.unpack resultado)
               if resultado == "Acertou!"
                  then putStrLn "Parabéns!"
                  else jogar resposta (tentativa + 1) -- aqui sim incrementa




main :: IO ()
main = scotty 3000 $ do
    middleware logStdoutDev
    get "/" $ do
        lista <- liftIO carregarPalavras
        randomPalavra <- liftIO (getRandomPalavra lista)
        text randomPalavra
