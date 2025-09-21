module Main exposing (main)

import Browser
import Html exposing (Html, button, div, form, h1, h2, input, li, span, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode exposing (Decoder)



type alias Resposta =
    { mensagem : String
    , certas : List String
    , erradas : List String
    , ausentes : List String
    , mascara : String
    , palpite : String
    , tentativa : Int
    , fimDeJogo : Bool
    }


respostaDecoder : Decoder Resposta
respostaDecoder =
    Decode.map8 Resposta
        (Decode.field "mensagem" Decode.string)
        (Decode.field "certas" (Decode.list Decode.string))
        (Decode.field "erradas" (Decode.list Decode.string))
        (Decode.field "ausentes" (Decode.list Decode.string))
        (Decode.field "mascara"  Decode.string)
        (Decode.field "palpite"  Decode.string)
        (Decode.field "tentativa" Decode.int)
        (Decode.field "fimDeJogo" Decode.bool)



-- MODEL

type alias Model =
    { palpite : String
    , historico : List Resposta
    , jogoAtivo : Bool
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( { palpite = "", historico = [], jogoAtivo = True }
    , Cmd.none
    )



-- UPDATE

type Msg
    = AtualizarPalpite String
    | Enviar
    | RecebeResposta (Result Http.Error Resposta)
    | NovaPartida
    | RecebeNova (Result Http.Error Resposta)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AtualizarPalpite novo ->
            ( { model | palpite = novo }, Cmd.none )

        Enviar ->
            if String.length model.palpite /= 5 then
                let
                    aviso =
                        { mensagem = "A palavra precisa ter 5 letras!"
                        , certas = []
                        , erradas = []
                        , ausentes = []
                        , mascara = "-----"
                        , palpite = model.palpite
                        , tentativa = List.length model.historico
                        , fimDeJogo = False
                        }
                in
                ( { model | historico = aviso :: model.historico }
                , Cmd.none
                )

            else
                ( model
                , Http.get
                    { url = "/palpite/" ++ model.palpite
                    , expect = Http.expectJson RecebeResposta respostaDecoder
                    }
                )

        RecebeResposta (Ok r) ->
            ( { model
                | historico = r :: model.historico
                , palpite = ""
                , jogoAtivo = not r.fimDeJogo
              }
            , Cmd.none
            )

        RecebeResposta (Err _) ->
            let
                erro =
                    { mensagem = "Erro de conexão com o servidor"
                    , certas = []
                    , erradas = []
                    , ausentes = []
                    , mascara = "-----"
                    , palpite = model.palpite
                    , tentativa = List.length model.historico
                    , fimDeJogo = False
                    }
            in
            ( { model | historico = erro :: model.historico }, Cmd.none )

        NovaPartida ->
            ( model
            , Http.get
                { url = "/nova"
                , expect = Http.expectJson RecebeNova respostaDecoder
                }
            )

        RecebeNova (Ok r) ->
            ( { model | historico = [ r ], palpite = "", jogoAtivo = True }
            , Cmd.none
            )

        RecebeNova (Err _) ->
            let
                erroNova =
                    { mensagem = "Erro ao iniciar nova partida"
                    , certas = []
                    , erradas = []
                    , ausentes = []
                    , mascara = "-----"
                    , palpite = ""
                    , tentativa = 0
                    , fimDeJogo = False
                    }
            in
            ( { model | historico = erroNova :: model.historico  }
            , Cmd.none
            )



-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Termo em Haskell (Elm)" ]
        , button [ onClick NovaPartida ] [ text "Nova Partida" ]
        , Html.form [ onSubmit Enviar ]
            [ input
                [ placeholder "Digite aqui"
                , value model.palpite
                , onInput AtualizarPalpite
                , disabled (not model.jogoAtivo)
                ]
                []
            , button [ type_ "submit", disabled (not model.jogoAtivo) ] [ text "Chutar" ]
            ]
        , h2 [] [ text "Tentativas:" ]
        , div [ class "attempts" ]
            [ ul [ class "attempt-list" ] (List.map viewResposta model.historico) ]
        ]



viewResposta : Resposta -> Html Msg
viewResposta r =
    li []
        [ div []
            [ span [ class "certas" ]  [ text ("Certas: "  ++ String.join "" r.certas) ]
            , text " "
            , span [ class "erradas" ] [ text ("Erradas: " ++ String.join "" r.erradas) ]
            , text " "
            , span [ class "ausentes" ] [ text ("Ausentes: " ++ String.join "" r.ausentes) ]
            ]
        , div [] [ text ("Posições: " ++ r.mascara) ]
        , if r.palpite /= "" then
            div [] [ text ("Palpite: " ++ r.palpite) ]
          else
            text ""
        , div [] [ text r.mensagem ]
        ]






-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
