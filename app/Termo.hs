{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import System.Random (randomRIO)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import Data.List (nub)
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
comparaPalavra palpite resposta = T.toLower palpite == T.toLower resposta

checarTentativas :: Int -> Bool
checarTentativas numTentativa = numTentativa < 6

-- gera dicas
gerarDicas :: T.Text -> T.Text -> (T.Text, T.Text, T.Text)
gerarDicas palpite resposta =
    let p = T.toLower palpite
        r = T.toLower resposta
        certas   = [c | (c,r') <- T.zip p r, c == r']
        erradas  = [c | c <- T.unpack p, c `elem` T.unpack r, c `notElem` certas]
        ausentes = [c | c <- T.unpack p, c `notElem` T.unpack r]
    in ( T.pack certas
       , T.pack (nub erradas)
       , T.pack (nub ausentes)
       )

-- recebe palpite
receberPalpite :: T.Text -> T.Text -> Int -> T.Text
receberPalpite palpite resposta numTentativa
    | T.length palpite /= 5 = "A palavra precisa ter 5 letras!"
    | comparaPalavra palpite resposta = "Acertou!"
    | otherwise =
        let (certas, erradas, ausentes) = gerarDicas palpite resposta
        in "Tentativa " <> T.pack (show (numTentativa+1)) <> ":\n"
        <> " Certas: " <> certas
        <> " | Erradas: " <> erradas
        <> " | Ausentes: " <> ausentes

        

main :: IO ()
main = do
    lista <- carregarPalavras
    palavraInicial <- getRandomPalavra lista
    palavraRef <- newIORef palavraInicial
    tentativasRef <- newIORef (0 :: Int)

    scotty 3000 $ do
        middleware logStdoutDev
        get "/" $ file "static/index.html"

        -- iniciar nova partida (sorteia nova palavra e zera tentativas)
        get "/nova" $ do
            lista <- liftIO carregarPalavras
            palavra <- liftIO (getRandomPalavra lista)
            liftIO (writeIORef palavraRef palavra)
            liftIO (writeIORef tentativasRef 0)
            text "Nova palavra sorteada! Boa sorte."

        -- rota de palpite
        get "/palpite/:p" $ do
            palpiteLazy <- param "p"
            let palpite = TL.toStrict palpiteLazy

            palavra <- liftIO (readIORef palavraRef)
            tentativas <- liftIO (readIORef tentativasRef)

            if not (checarTentativas tentativas)
            then text (TL.fromStrict ("Fim de jogo! A palavra era: " <> palavra))
            else do
                let resposta = receberPalpite palpite palavra tentativas

                if T.length palpite /= 5
                then text (TL.fromStrict resposta)  -- não conta tentativa
                else if comparaPalavra palpite palavra
                    then text (TL.fromStrict ("Parabéns, você acertou a palavra: " <> palavra))
                    else do
                    liftIO $ writeIORef tentativasRef (tentativas + 1)
                    text (TL.fromStrict resposta)




