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
comparaPalavra palpite resposta = palpite == resposta

-- gera dicas a partir de um palpite
gerarDicas :: T.Text -> T.Text -> (T.Text, T.Text, T.Text)
gerarDicas palpite resposta =
        -- cria pares e compara
    let certas   = [c | (c,r) <- T.zip palpite resposta, c == r]
        -- letras que aparecem na resposta, mas nao estao em 'certas'
        erradas  = [c | c <- T.unpack palpite, c `elem` T.unpack resposta, c `notElem` certas]
        -- letras que nao estao na resposta
        ausentes = [c | c <- T.unpack palpite, c `notElem` T.unpack resposta]
    in (T.pack certas, T.pack erradas, T.pack ausentes)

-- controla limite de tentativas
checarTentativas :: Int -> Bool
checarTentativas numTentativa = numTentativa < 6

-- recebe um palpite e retorna mensagem com dicas
receberPalpite :: T.Text -> T.Text -> Int -> T.Text
receberPalpite palpite resposta numTentativa
    | T.length palpite /= 5 = "A palavra precisa ter 5 letras!"
    | comparaPalavra palpite resposta = "Acertou!"
    | otherwise =
        let (certas, erradas, ausentes) = gerarDicas palpite resposta
        in "Letras em posicao certa: " <> certas
        <> " | Letras em posicao errada: " <> erradas
        <> " | Letras ausentes: " <> ausentes

-- começar o jogo
jogar :: T.Text -> Int -> IO ()
jogar resposta numTentativa
    | numTentativa >= 6 = putStrLn $ "Você perdeu! A resposta era: " ++ T.unpack resposta
    | otherwise = do
        putStrLn "Digite uma palavra de 5 letras:"
        entrada <- getLine
        if length entrada /= 5
           then do
               putStrLn "A palavra precisa ter 5 letras! Tente novamente."
               jogar resposta numTentativa  -- não incrementa
           else do
               let resultado = receberPalpite (T.pack entrada) resposta numTentativa
               putStrLn (T.unpack resultado)
               if resultado == "Acertou!"
                  then putStrLn "Parabéns!"
                  else jogar resposta (numTentativa + 1)



main :: IO ()
main = scotty 3000 $ do
    middleware logStdoutDev
    get "/" $ do
        lista <- liftIO carregarPalavras
        randomPalavra <- liftIO (getRandomPalavra lista)
        text randomPalavra
