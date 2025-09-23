{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

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
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Resposta = Resposta
  { mensagem  :: T.Text
  , certas    :: [String]
  , erradas   :: [String]
  , ausentes  :: [String]
  , mascara   :: T.Text
  , palpite   :: T.Text
  , tentativa :: Int
  , fimDeJogo :: Bool
  } deriving (Show, Generic)

data PalpiteReq = PalpiteReq
  { palp :: T.Text
  } deriving (Show, Generic)

instance ToJSON Resposta
instance FromJSON Resposta
instance FromJSON PalpiteReq

toListChars :: T.Text -> [String]
toListChars = map (:[]) . T.unpack

maskCorretas :: T.Text -> T.Text -> T.Text
maskCorretas palpite resposta =
  let p = T.toLower palpite
      r = T.toLower resposta
  in T.pack [ if c == r' then c else '-' | (c, r') <- T.zip p r ]


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


receberPalpite :: T.Text -> T.Text -> Int -> Resposta
receberPalpite palpite resposta numTentativa
    -- palpite invalido (nao conta tentativa)
    | T.length palpite /= 5 =
        Resposta
          { mensagem  = "A palavra precisa ter 5 letras!"
          , certas    = []
          , erradas   = []
          , ausentes  = []
          , mascara   = T.replicate 5 "-"
          , palpite   = palpite
          , tentativa = numTentativa
          , fimDeJogo = False
          }

    -- acertou (fim de jogo)
    | comparaPalavra palpite resposta =
        let (certasT, _, _) = gerarDicas palpite resposta
        in Resposta
          { mensagem  = "Parabéns, você acertou a palavra!"
          , certas    = toListChars certasT
          , erradas   = []
          , ausentes  = []
          , mascara   = maskCorretas palpite resposta
          , palpite   = palpite
          , tentativa = numTentativa + 1
          , fimDeJogo = True
          }

    -- errou, mas eh tentativa valida (conta +1)
    | otherwise =
        let (certasT, erradasT, ausentesT) = gerarDicas palpite resposta
        in Resposta
          { mensagem  = "Tentativa " <> T.pack (show (numTentativa + 1))
          , certas    = toListChars certasT
          , erradas   = toListChars erradasT
          , ausentes  = toListChars ausentesT
          , mascara   = maskCorretas palpite resposta
          , palpite   = palpite
          , tentativa = numTentativa + 1
          , fimDeJogo = False
          }

        

main :: IO ()
main = do
    lista <- carregarPalavras
    palavraInicial <- getRandomPalavra lista
    palavraRef <- newIORef (palavraInicial :: T.Text)
    tentativasRef <- newIORef (0 :: Int)

    scotty 3000 $ do
        middleware logStdoutDev
        middleware $ staticPolicy (addBase "static")
        
        get "/" $ file "static/index.html"

        -- iniciar nova partida (sorteia nova palavra e zera tentativas)
        get "/nova" $ do
            lista <- liftIO carregarPalavras
            palavra <- liftIO (getRandomPalavra lista)
            liftIO (writeIORef palavraRef palavra)
            liftIO (writeIORef tentativasRef 0)
            json Resposta
                { mensagem  = "Nova palavra sorteada! Boa sorte."
                , certas    = []
                , erradas   = []
                , ausentes  = []
                , mascara   = T.replicate 5 "-"
                , palpite   = ""
                , tentativa = 0
                , fimDeJogo = False
                }


        -- rota de palpite
        post "/palpite" $ do
            PalpiteReq palpite <- jsonData
            
            palavra <- liftIO (readIORef palavraRef)
            tentativas <- liftIO (readIORef tentativasRef)

            if not (checarTentativas tentativas)
            then json Resposta
                    { mensagem  = "Fim de jogo! A palavra era: " <> palavra
                    , certas    = []
                    , erradas   = []
                    , ausentes  = []
                    , mascara   = T.replicate 5 "-"
                    , palpite   = palpite
                    , tentativa = tentativas
                    , fimDeJogo = True
                    }
            else do
                let resposta = receberPalpite palpite palavra tentativas
                if T.length palpite /= 5 then
                    --palpite inválido nao conta tentativa
                    json resposta

                else if comparaPalavra palpite palavra then
                    --acertou: fim de jogo a vem dentro de 'resposta'
                    json resposta

                else do
                    --errou com palpite valido: incrementa e, se virou a 6, encerra
                    let tent' = tentativas + 1
                    liftIO $ writeIORef tentativasRef tent'

                    if tent' >= 6 then
                        json Resposta
                            { mensagem  = "Fim de jogo! A palavra era: " <> palavra
                            , certas    = []
                            , erradas   = []
                            , ausentes  = []
                            , mascara   = T.replicate 5 "-"
                            , palpite   = ""          
                            , tentativa = tent'
                            , fimDeJogo = True
                            }
                        else
                            json resposta






