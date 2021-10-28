module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Details
import Html exposing (Html, div, text)
import Html.Attributes exposing (..)
import Overview
import Url

type State
    = Overview Overview.Model
    | Details Details.Model
    | Blank

type alias Model =
    { state: State
    , key: Nav.Key
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
                Blank -> div [] [ text "Nothing to see here" ]
                Overview state -> Overview.view state |> Html.map GotOverviewMsg
                Details state -> Details.view state |> Html.map GotDetailsMsg
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
                    case url.fragment of
                        Nothing -> ( model, Cmd.none)
                        Just _ -> ( model, Nav.pushUrl model.key <| Url.toString url )
                Browser.External href -> ( model, Nav.load href )

        UrlChanged url ->
            let (overviewModel, overviewMessage) = Overview.init () url model.key
            in ({model | state = Overview overviewModel}, Cmd.map GotOverviewMsg overviewMessage)

        GotOverviewMsg overviewMessage ->
            case model.state of
                Overview overviewModel ->
                    updateStateWith Overview GotOverviewMsg model <| Overview.update overviewMessage overviewModel
                _ -> ( model, Cmd.none )
        GotDetailsMsg detailsMessage ->
            case model.state of
                Details detailsModel ->
                    updateStateWith Details GotDetailsMsg model <| Details.update detailsMessage detailsModel
                _ -> ( model, Cmd.none )


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    updateStateWith Details GotDetailsMsg ( initialModel key ) <| Details.init flags url key

initialModel : Nav.Key -> Model
initialModel key = {state = Blank, key = key}

updateStateWith : (subModel -> State) -> (subMsg -> Msg) -> Model -> (subModel, Cmd subMsg) -> (Model, Cmd Msg)
updateStateWith toModel toMsg model (subModel, subCmd) =
    ({model | state = toModel subModel}
    , Cmd.map toMsg subCmd
    )

initBlank flags url key =
    ({ state = Blank, key = key }, Cmd.none )

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
