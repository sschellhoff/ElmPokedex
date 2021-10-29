module Overview exposing (Model, Msg, PokemonInfo, initialState, update, view)

import Browser
import Html exposing (Html, a, div, h1, li, text, ul)
import Html.Attributes exposing (href)
import Http
import Json.Decode
import Lazy exposing (..)


type alias PokemonInfo =
    { name : String
    , no : Maybe Int
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
    case pokemonInfo.no of
        Just no ->
            li [] [ a [ href <| getPokemonDetailUrl no ] [ text <| String.concat [ String.fromInt no, ": ", pokemonInfo.name ] ] ]

        Nothing ->
            li [] [ text <| String.concat [ "x: ", pokemonInfo.name ] ]


getPokemonDetailUrl : Int -> String
getPokemonDetailUrl no =
    String.concat [ "pokemon/", String.fromInt no ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Load pageSize ->
            ( model, loadOverview pageSize )

        GotResult result ->
            case result of
                Ok data ->
                    ( { model | pokemonList = Finished data }, Cmd.none )

                Err _ ->
                    ( { model | pokemonList = Error "Could not load data" }, Cmd.none )


loadOverview : Int -> Cmd Msg
loadOverview pageSize =
    Http.get
        { url = String.concat [ "https://pokeapi.co/api/v2/pokemon?limit=", String.fromInt pageSize ]
        , expect = Http.expectJson GotResult decodeOverviewResponse
        }


decodeOverviewResponse : Json.Decode.Decoder (List PokemonInfo)
decodeOverviewResponse =
    Json.Decode.field "results" (Json.Decode.list decodePokemonInfo)


decodePokemonInfo : Json.Decode.Decoder PokemonInfo
decodePokemonInfo =
    Json.Decode.map2
        (\name url -> { name = name, no = getPokemonNumberFromDetailUrl url })
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "url" Json.Decode.string)


getPokemonNumberFromDetailUrl : String -> Maybe Int
getPokemonNumberFromDetailUrl url =
    Maybe.andThen String.toInt <| beforeLast <| String.split "/" url


beforeLast : List a -> Maybe a
beforeLast elements =
    Maybe.andThen List.head <| List.tail <| List.reverse elements


initialState : Int -> ( Model, Cmd Msg )
initialState pageSize =
    ( { pokemonList = Loading }, loadOverview pageSize )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( { pokemonList = Loading }, loadOverview 151 )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
