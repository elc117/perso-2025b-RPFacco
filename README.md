# Jogo Termo

## **Identificação**
   
Nome: Ricardo Facco Pigatto  
Curso: Sistemas de Informação

## **Tema:**   
Jogo de adivinhar a palavra-resposta "Termo".
   
**Objetivo:** O usuário deve tentar adivinhar a palavra sorteada de 5 letras em no máximo 6 tentativas. Para cada tentativa, o programa mostra as seguintes dicas:  
  
**Dica 1:** O programa vai dizer quais as “letras que existem na palavra-resposta, e estão na posição correta.” com base na palavra digitada pelo usuário.  
**Dica 2:** O programa vai dizer quais as “letras que existem na palavra-resposta, mas não estão na posição correta.” com base na palavra digitada pelo usuário.  
**Dica 3:** O programa vai dizer quais as “letras que não existem na palavra-resposta.” com base na palavra digitada pelo usuário.  

## **Desenvolvimento:**
  
Primeiramente, eu desenvolvi um protótipo do jogo possível de ser testado no terminal, e liguei um servidor com Scotty para testar o sorteio da palavra. Fiz isso criando funções em haskell para ler o arquivo .txt de palavras e escolher uma aleatoriamente, utilizando o exemplo "randomAdviceService" diponibilizado pela professora. Depois criei funções para receber o palpite do usuário, comparar o palpite com a palavra-resposta, gerar dicas, checar o total de tentativas, e "jogar", que é uma função recursiva.

    
  <img width="687" height="236" alt="image" src="https://github.com/user-attachments/assets/29f1d891-dc7b-4ac3-81e9-dfac68ce75dc" />
  
  
Uma dificuldade que tive foi na utilização de IO vs valores "puros", e entendi que, em Haskell, operações que tocam o mundo externo pertencem a IO (ler/escrever arquivos, imprimir no console, etc). Também pesquisei sobre Data.Text para entender como isso seria útil no código, e percebi que ele é mais eficiente que String para manipular texto e lida melhor com Unicode (acentos). Comecei a usar Text na lógica e só converto para Lazy Text com TL.fromStrict, que é o que o Scotty espera em text.  
  
Depois, precisei transformar as informações em JSON para a comunicação com o frontend (Elm). Fiz a escolha de usar elm por ser parecido com haskell. Eu ainda não domino a linguagem, mas consegui entender o básico da arquitetura model-update-view e, fazendo uso dos exemplos fornecidos pelo próprio site da Elm, além de .html e .css, fiz a construção do front. Como esse projeto é só um protótipo do jogo "Termo", priorizei o que eu dominava melhor no frontend e fui incorporando o restante aos poucos.  
  
## **Orientações para execução:**  
Pré-requisitos: GHC + Cabal  
Na raiz do projeto:
- cabal update
- cabal run
   
Navegador em http://localhost:3000  
  
## **Resultado Final:**
  
https://github.com/user-attachments/assets/a5eafc80-72e6-4af0-86da-d2a68e748c16
  


## **Referências e créditos**
   1. https://raw.githubusercontent.com/elc117/demo-scotty-codespace-2025b/main/src/02-scotty-random-advice/randomAdviceService.hs
   2. https://www.haskell.org/tutorial/io.html
   3. https://learnyouahaskell.com/recursion
   4. https://hackage.haskell.org/package/text-0.1/docs/Data-Text.html
   5. https://hackage.haskell.org/package/aeson
   6. https://guide.elm-lang.org
   7. https://elm-lang.org/examples
   8. https://guide.elm-lang.org/effects/json