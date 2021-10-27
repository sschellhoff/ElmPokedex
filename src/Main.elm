module Main exposing (main)

import Browser
import Html exposing (Html, a, div, h1, img, li, span, text, ul)
import Html.Attributes exposing (..)
import Http
import Json.Decode


type alias PokemonInfo =
    { name : String
    , url : String
    }

type alias PokemonDetails =
    { id: Int
    , name: String
    , height: Int
    , weight: Int
    , baseExp: Int
    , sprite: String
    }

type ApplicationState
    = Loading
    | Overview (List PokemonInfo)
    | Error String
    | Details PokemonDetails


type alias Model =
    { state : ApplicationState
    }


type Msg
    = ShowOverview
    | ShowPokemonDetails
    | GotOverview (Result Http.Error (List PokemonInfo))


view : Model -> Html Msg
view model =
    div [ class "content" ] <| renderModel model


renderModel : Model -> List (Html Msg)
renderModel model =
    case model.state of
        Loading ->
            [ h1 [] [ text "Loading" ] ]

        Overview pokemonInfos ->
            renderOverview pokemonInfos

        Error msg ->
            [ h1 [] [ text msg ] ]

        Details pokemonDetails ->
            renderPokemonDetails pokemonDetails


renderOverview : List PokemonInfo -> List (Html Msg)
renderOverview pokemonList =
    [ h1 [] [ text "Pokedex" ]
    , ul [] <| List.map renderPokemonInfo pokemonList
    ]


renderPokemonInfo : PokemonInfo -> Html Msg
renderPokemonInfo pokemonInfo =
    li [] [ a [ href pokemonInfo.url ] [ text pokemonInfo.name ] ]

renderPokemonDetails : PokemonDetails -> List (Html Msg)
renderPokemonDetails pokemonDetails =
    [ h1 [] [ text pokemonDetails.name ]
    , renderPokemonDetail "no" pokemonDetails.id
    , renderPokemonDetail "height" pokemonDetails.height
    , renderPokemonDetail "weight" pokemonDetails.weight
    , renderPokemonDetail "base exp." pokemonDetails.baseExp
    , img [ src pokemonDetails.sprite ] []
    ]

renderPokemonDetail : String -> Int -> Html Msg
renderPokemonDetail fieldName fieldValue =
    div []
    [ span [] [ text fieldName ]
    , span [] [ text <| String.fromInt fieldValue ]
    ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowOverview ->
            ( model, Cmd.none )

        ShowPokemonDetails ->
            ( model, Cmd.none )

        GotOverview result ->
            case result of
                Ok response ->
                    ( { model | state = Overview response }, Cmd.none )

                Err _ ->
                    ( { model | state = Error "Server error!" }, Cmd.none )


getOverview : Cmd Msg
getOverview =
    Http.get
        { url = "https://pokeapi.co/api/v2/pokemon/"
        , expect = Http.expectJson GotOverview decodeOverviewResponse
        }


decodeOverviewResponse : Json.Decode.Decoder (List PokemonInfo)
decodeOverviewResponse =
    Json.Decode.field "results" (Json.Decode.list decodePokemonInfo)


decodePokemonInfo : Json.Decode.Decoder PokemonInfo
decodePokemonInfo =
    Json.Decode.map2
        (\name url -> { name = name, url = url })
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "url" Json.Decode.string)


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( { state = Loading }, getOverview )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
