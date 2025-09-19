module Main exposing (main)

import Browser
import Html exposing (Html, button, div, form, h1, h2, input, li, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http


-- MODEL

type alias Model =
    { palpite : String
    , historico : List String
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
    | RecebeResposta (Result Http.Error String)
    | NovaPartida
    | RecebeNova (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AtualizarPalpite novo ->
            ( { model | palpite = novo }, Cmd.none )

        Enviar ->
            if String.length model.palpite /= 5 then
                ( { model
                    | historico = model.historico ++ [ "A palavra precisa ter 5 letras!" ]
                  }
                , Cmd.none
                )
            else
                ( model
                , Http.get
                    { url = "/palpite/" ++ model.palpite
                    , expect = Http.expectString RecebeResposta
                    }
                )

        RecebeResposta (Ok resposta) ->
            let
                acabou =
                    String.contains "Fim de jogo" resposta
                        || String.contains "Parabéns" resposta
            in
            ( { model
                | historico = model.historico ++ [ resposta ]
                , palpite = ""
                , jogoAtivo = not acabou
              }
            , Cmd.none
            )

        RecebeResposta (Err _) ->
            ( { model | historico = model.historico ++ [ "Erro de conexão com o servidor" ] }
            , Cmd.none
            )

        NovaPartida ->
            ( model
            , Http.get
                { url = "/nova"
                , expect = Http.expectString RecebeNova
                }
            )

        RecebeNova (Ok resposta) ->
            ( { model
                | historico = [ resposta ]
                , palpite = ""
                , jogoAtivo = True
              }
            , Cmd.none
            )

        RecebeNova (Err _) ->
            ( { model | historico = model.historico ++ [ "Erro ao iniciar nova partida" ] }
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
        , ul [] (List.map (\t -> li [] [ text t ]) model.historico)
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
