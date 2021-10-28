module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Details
import Html exposing (Html, a, div, span, text)
import Html.Attributes exposing (..)
import Overview
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s)


type State
    = Overview Overview.Model
    | Details Details.Model
    | Blank


type alias Model =
    { state : State
    , key : Nav.Key
    }


type Msg
    = GotOverviewMsg Overview.Msg
    | GotDetailsMsg Details.Msg
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest


view : Model -> Browser.Document Msg
view model =
    let
        content =
            case model.state of
                Blank ->
                    div []
                        [ span [] [ text "Nothing to see here, just visit the " ]
                        , a [ href "/pokemon" ] [ text "Pokedex" ]
                        ]

                Overview state ->
                    Overview.view state |> Html.map GotOverviewMsg

                Details state ->
                    Details.view state |> Html.map GotDetailsMsg
    in
    { title = "Pokedex"
    , body = [ div [ class "content" ] <| [ content ] ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key <| Url.toString url )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                ( state, me ) =
                    urlToState url
            in
            ( { model | state = state }, me )

        GotOverviewMsg overviewMessage ->
            case model.state of
                Overview overviewModel ->
                    updateStateWith Overview GotOverviewMsg model <| Overview.update overviewMessage overviewModel

                _ ->
                    ( model, Cmd.none )

        GotDetailsMsg detailsMessage ->
            case model.state of
                Details detailsModel ->
                    updateStateWith Details GotDetailsMsg model <| Details.update detailsMessage detailsModel

                _ ->
                    ( model, Cmd.none )


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( mo, me ) =
            urlToState url
    in
    ( { state = mo, key = key }, me )


updateStateWith : (subModel -> State) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateStateWith toModel toMsg model ( subModel, subCmd ) =
    ( { model | state = toModel subModel }
    , Cmd.map toMsg subCmd
    )


initStateWith : (subModel -> State) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( State, Cmd Msg )
initStateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


initBlank : ( State, Cmd Msg )
initBlank =
    ( Blank, Cmd.none )


urlToState : Url -> ( State, Cmd Msg )
urlToState url =
    Parser.parse parser url
        |> Maybe.withDefault initBlank


parser : Parser (( State, Cmd Msg ) -> a) a
parser =
    Parser.oneOf
        [ Parser.map initBlank Parser.top
        , Parser.map (initStateWith Overview GotOverviewMsg (Overview.initialState 0)) (s "pokemon")
        , Parser.map (\no -> initStateWith Details GotDetailsMsg (Details.initialState no)) (s "pokemon" </> Parser.int)
        ]


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
