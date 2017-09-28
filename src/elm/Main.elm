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
    figure,
    img,
    p,
    a,
    form,
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
    | SearchUsers (Result Http.Error (List User))


type alias Model =
  { term: String
  , isSearchBtnDisabled : Bool
  , isLoading: Bool
  , isRequestFailed: Bool
  , users: List User
  }

model =
  { term = ""
  , isSearchBtnDisabled = True
  , isLoading = False
  , isRequestFailed = False
  , users = []
  }

searchBox : Model -> Html Msg
searchBox {isLoading, isSearchBtnDisabled, term} =
  div []
    [ form
      [ class "field"
      , onSubmit OnSearchBtnClicked
      ]
      [ label [class "label"] [text "Digite o nome do usuário"]
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
          [text "Procurar"]
        ]
      , div [ class "control" ]
        [ button
          [ class "button is-danger is-medium"
          , onClick OnClearBtnClicked
          ]
          [ text "Limpar" ]
        ]
      ]
    ]

searchBtnClasses : Bool -> String
searchBtnClasses isLoading =
  if isLoading then "button is-primary is-medium is-loading" else "button is-primary is-medium"

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

mainPage : List (Html Msg) -> Html Msg
mainPage children =
  section [ class "section" ]
    [ div [ class "container" ] children ]

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

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

init : (Model, Cmd Msg)
init = ( model , Cmd.none )

view : Model -> Html Msg
view model =
  mainPage
    [ header
    , searchBox model
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
    SearchUsers (Ok users) ->
      ({ model | isLoading = False, users = users }, Cmd.none)
    SearchUsers (Err _) ->
    ({ model | isLoading = False, users = [] }, Cmd.none)

main = program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
    }
