module Overview exposing (Model, Msg, PokemonInfo, initialState, update, view)

import Browser
import Html exposing (Html, a, div, h1, li, text, ul)
import Html.Attributes exposing (href)
import Http
import Json.Decode
import Lazy exposing (..)


type alias PokemonInfo =
    { name : String
    , url : String
    }


type alias Model =
    { pokemonList : Lazy (List PokemonInfo)
    }


type Msg
    = Load Int
    | GotResult (Result Http.Error (List PokemonInfo))


view : Model -> Html Msg
view model =
    case model.pokemonList of
        Lazy.Loading ->
            div [] [ text "loading..." ]

        Lazy.Finished pokemonList ->
            div [] <| renderPokemonList pokemonList

        Lazy.Error msg ->
            h1 [] [ text msg ]


renderPokemonList : List PokemonInfo -> List (Html Msg)
renderPokemonList pokemonList =
    [ h1 [] [ text "Pokedex" ]
    , ul [] <| List.map renderPokemonInfo pokemonList
    ]


renderPokemonInfo : PokemonInfo -> Html Msg
renderPokemonInfo pokemonInfo =
    li [] [ a [ href pokemonInfo.url ] [ text pokemonInfo.name ] ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Load page ->
            ( model, loadOverview page )

        GotResult result ->
            case result of
                Ok data ->
                    ( { model | pokemonList = Finished data }, Cmd.none )

                Err _ ->
                    ( { model | pokemonList = Error "Could not load data" }, Cmd.none )


loadOverview : Int -> Cmd Msg
loadOverview page =
    Http.get
        { url = "https://pokeapi.co/api/v2/pokemon/"
        , expect = Http.expectJson GotResult decodeOverviewResponse
        }


decodeOverviewResponse : Json.Decode.Decoder (List PokemonInfo)
decodeOverviewResponse =
    Json.Decode.field "results" (Json.Decode.list decodePokemonInfo)


decodePokemonInfo : Json.Decode.Decoder PokemonInfo
decodePokemonInfo =
    Json.Decode.map2
        (\name url -> { name = name, url = getPokemonDetailUrl url })
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "url" Json.Decode.string)


getPokemonDetailUrl : String -> String
getPokemonDetailUrl url =
    String.concat [ "", getPokemonNumberFromDetailUrl url ]


getPokemonNumberFromDetailUrl : String -> String
getPokemonNumberFromDetailUrl url =
    Maybe.withDefault "1" <| beforeLast <| String.split "/" url


beforeLast : List a -> Maybe a
beforeLast elements =
    Maybe.andThen List.head <| List.tail <| List.reverse elements


initialState : Int -> ( Model, Cmd Msg )
initialState page =
    ( { pokemonList = Loading }, loadOverview page )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( { pokemonList = Loading }, loadOverview 0 )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
