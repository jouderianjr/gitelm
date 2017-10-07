module Main exposing (..)

import Types exposing (User)
import Html exposing (
    Html,
    label,
    input,
    div,
    text,
    section,
    button,
    h1,
    h2,
    figure,
    img,
    p,
    a,
    form,
    nav,
    h6,
    i,
    span,
    program)
import Html.Attributes exposing (class, disabled, src, href, target, value)
import Html.Events exposing (onInput, onClick, onSubmit)
import String exposing (isEmpty)
import List exposing (repeat, map)
import Http
import Json.Decode exposing (Decoder, list, string, int, at, succeed, field)
import Json.Decode.Extra exposing ((|:))

type Msg
    = OnTermChanged String
    | OnSearchBtnClicked
    | OnClearBtnClicked
    | OnShowAboutModalClicked
    | OnCloseAboutModalClicked
    | SearchUsers (Result Http.Error (List User))

type alias Model =
  { term: String
  , isSearchBtnDisabled : Bool
  , isLoading: Bool
  , isShowingAboutModal: Bool
  , isRequestFailed: Bool
  , users: List User
  }

model : Model
model =
  { term = ""
  , isSearchBtnDisabled = True
  , isLoading = False
  , isShowingAboutModal = False
  , isRequestFailed = False
  , users = []
  }

navbar : Html Msg
navbar =
  nav
    [ class "navbar is-dark" ]
    [ div
      [ class "navbar-brand" ]
      [ div
        [ class "navbar-item" ]
        [ h6
          [ class "is-size-3 content"]
          [ p []
            [ i [ class "fa fa-github" ] []
            , text "itElm"
            ]
          ]
        ]
      ]
    , div [ class "navbar-menu" ]
      [ div [ class "navbar-end" ]
        [ div [ class "navbar-item" ]
          [ button
            [ class "button is-outlined is-white"
            , onClick OnShowAboutModalClicked
            ]
            [ text "About" ]
          ]
        ]
      ]
    ]


searchBox : Model -> Html Msg
searchBox {isLoading, isSearchBtnDisabled, term} =
  div [ class "box" ]
    [ form
      [ class "field"
      , onSubmit OnSearchBtnClicked
      ]
      [ label [class "label"] [text "Search GitHub user"]
      , div [ class "control" ]
        [ input
          [ class "input"
          , onInput OnTermChanged
          , disabled isLoading
          , value term
          ]
          []
        ]
      ]
    , div [ class "field is-grouped"]
      [ div [ class "control" ]
        [ button
          [ class (searchBtnClasses isLoading)
          , onClick OnSearchBtnClicked
          , disabled isSearchBtnDisabled
          ]
          [text "Search"]
        ]
      , div [ class "control" ]
        [ button
          [ class "button is-danger is-medium"
          , onClick OnClearBtnClicked
          ]
          [ text "Clear" ]
        ]
      ]
    ]

searchBtnClasses : Bool -> String
searchBtnClasses isLoading =
  let
    defaultClasses = "button is-dark is-medium"
  in
    if isLoading then defaultClasses ++ " is-loading" else defaultClasses

header : Html msg
header = h1 [class "title is-1 has-text-centered"] [ text "Gitelm" ]

renderUsers : List User -> List (Html Msg)
renderUsers users =
  map renderUserCard users

renderUserCard : User -> Html Msg
renderUserCard {login, avatar_url, html_url} =
  div [ class "card" ]
    [ div [ class "card-content" ]
        [ div [ class "media"]
            [ div [ class "media-left" ]
                [ figure [ class "image is-48x48" ]
                    [ img [ src avatar_url ] [] ]
                ]
            , div [ class "media-content" ]
                [ a
                  [ class "is-title is-4"
                  , href html_url
                  , target "_blank"
                  ] [ text ("@" ++ login) ] ]
            ]
        ]
    ]

mainPage : Model -> List (Html Msg) -> Html Msg
mainPage model children =
  div []
    [ aboutModal model
    , navbar
    , section [ class "section" ]
      [ div [ class "container" ] children ]
    ]

searchUsers : String -> Cmd Msg
searchUsers term =
  let
    url = "https://api.github.com/search/users?q=" ++ term
  in
    Http.send SearchUsers (Http.get url dataDecoder)

dataDecoder : Decoder (List User)
dataDecoder =
    field "items" (list userDecoder)

userDecoder : Decoder User
userDecoder =
  succeed
    User
      |: (field "login" string)
      |: (field "id" int)
      |: (field "avatar_url" string)
      |: (field "html_url" string)
      |: (field "url" string)

aboutModal : Model -> Html Msg
aboutModal {isShowingAboutModal} =
  let
    animationClass =
      if isShowingAboutModal then " show" else ""
  in
    div [ class ("about-modal" ++ animationClass) ]
      [ button
        [ class "button is-outlined is-white is-large close-button is-pulled-right"
        , onClick OnCloseAboutModalClicked
        ]
        [ i [ class " fa fa-close"] [] ]
      , div [ class "section is-clearfix" ]
        [ div [ class "" ]
          [ p [ class "title is-2 has-text-warning" ] [text "Do you like it?"]
          , p [ class "answer title is-3 has-text-primary" ] [text "This was built using Elm"]
          , p [ class "title is-2 has-text-warning" ] [text "Would like to see the code?"]
          , p [ class "title"]
            [ a
              [ class "answer title is-3 has-text-primary"
              , href "https://github.com/jouderianjr/gitelm"
              ]
              [ text "https://github.com/jouderianjr/gitelm" ]
            ]
          , p [ class "title is-2 has-text-warning" ] [text "Who built it?"]
          , a
            [ href "http://www.github.com/jouderianjr"
            , target "_blank"
            , class "who-built-it"
            ]
            [ img
              [ src "https://avatars2.githubusercontent.com/u/1559013?v=4"
              , class "answer image is-64x64"
              ]
              []
            , i [class "who-built-it-icon has-text-primary is-2 fa fa-hand-o-left"] []
            , span [ class "who-built-it-text title is-3 has-text-primary" ] [ text "Click here" ]
            ]
          ]
        ]
      , div [ class "container" ]
        [ div [ class "columns" ]
          [ div [class "column"]
            [ h2 [ class "title is-3 has-text-warning has-text-centered" ] [text "Pay me a coffee!"]
            , a
              [ href "http://ko-fi.com/jouderianjr"
              , target "_blank"
              ]
              [ i [ class "donate-icon fa fa-coffee has-text-primary"] [] ]
            ]
          , div [class "column"]
            [ h2 [ class "title is-3 has-text-warning has-text-centered" ] [text "Send me Bitcoins!"]
            , p [ class "btc-address has-text-primary" ] [ text "1FLq2BAhF9esFYxWy2NHLL3urK6jUb1n1N" ]
            ]
          ]
        ]
      ]

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

init : (Model, Cmd Msg)
init = ( model , Cmd.none )

view : Model -> Html Msg
view model =
  mainPage model
    [ searchBox model
    , div [ class "section" ] ( renderUsers model.users )
    ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    OnTermChanged newTerm ->
      if isEmpty newTerm then
        ({ model | term = newTerm, isSearchBtnDisabled = True }, Cmd.none )
      else
        ({ model | term = newTerm, isSearchBtnDisabled = False }, Cmd.none)
    OnClearBtnClicked ->
      ({ model | term = "", users = [] }, Cmd.none)
    OnSearchBtnClicked ->
      ({ model | isLoading = True, users = [] }, searchUsers model.term)
    OnShowAboutModalClicked ->
      ({model | isShowingAboutModal = True }, Cmd.none)
    OnCloseAboutModalClicked ->
      ({model | isShowingAboutModal = False }, Cmd.none)
    SearchUsers (Ok users) ->
      ({ model | isLoading = False, users = users }, Cmd.none)
    SearchUsers (Err _) ->
      ({ model | isLoading = False, users = [] }, Cmd.none)

main : Program Never Model Msg
main = program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
    }
