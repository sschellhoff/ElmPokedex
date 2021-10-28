module Details exposing (Model, Msg, PokemonDetails, initialState, update, view)

import Browser
import Html exposing (Html, div, h1, img, span, text)
import Html.Attributes exposing (src)
import Http
import Json.Decode
import Lazy exposing (Lazy)
import String exposing (concat)


type alias PokemonDetails =
    { id : Int
    , name : String
    , height : Int
    , weight : Int
    , baseExp : Int
    , sprite : String
    }


type alias Model =
    { pokemonDetails : Lazy PokemonDetails
    }


type Msg
    = Load Int
    | GotResult (Result Http.Error PokemonDetails)


view : Model -> Html Msg
view model =
    case model.pokemonDetails of
        Lazy.Loading ->
            div [] [ text "loading..." ]

        Lazy.Finished pokemonDetails ->
            div [] <| renderPokemonDetails pokemonDetails

        Lazy.Error msg ->
            h1 [] [ text msg ]


renderPokemonDetails : PokemonDetails -> List (Html Msg)
renderPokemonDetails pokemonDetails =
    [ h1 [] [ text pokemonDetails.name ]
    , renderPokemonDetail "no: " pokemonDetails.id
    , renderPokemonDetail "height: " pokemonDetails.height
    , renderPokemonDetail "weight: " pokemonDetails.weight
    , renderPokemonDetail "base experience: " pokemonDetails.baseExp
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
        Load number ->
            ( model, loadDetails number )

        GotResult result ->
            case result of
                Ok data ->
                    ( { model | pokemonDetails = Lazy.Finished data }, Cmd.none )

                Err _ ->
                    ( { model | pokemonDetails = Lazy.Error "Could not load data" }, Cmd.none )


loadDetails : Int -> Cmd Msg
loadDetails number =
    Http.get
        { url = concat [ "https://pokeapi.co/api/v2/pokemon/", String.fromInt number ]
        , expect = Http.expectJson GotResult decodeDetailsResponse
        }


decodeDetailsResponse : Json.Decode.Decoder PokemonDetails
decodeDetailsResponse =
    Json.Decode.map6
        (\id name height weight baseExp sprite -> { id = id, name = name, height = height, weight = weight, baseExp = baseExp, sprite = sprite })
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "height" Json.Decode.int)
        (Json.Decode.field "weight" Json.Decode.int)
        (Json.Decode.field "base_experience" Json.Decode.int)
        (Json.Decode.field "sprites" <| Json.Decode.field "front_default" Json.Decode.string)


initialState : Int -> ( Model, Cmd Msg )
initialState no =
    ( { pokemonDetails = Lazy.Loading }, loadDetails no )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( { pokemonDetails = Lazy.Loading }, loadDetails 1 )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
