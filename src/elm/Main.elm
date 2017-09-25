module Main exposing (..)

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
    program)
import Html.Attributes exposing (class, disabled, src)
import Html.Events exposing (onInput, onClick)
import String exposing (isEmpty)
import List exposing (repeat, map)
import Http
import Json.Decode exposing (Decoder, list, string, int, at, succeed, field)
import Json.Decode.Extra exposing ((|:))

type Msg
    = OnTermChanged String
    | OnSearchBtnClicked
    | SearchUsers (Result Http.Error Users)

type alias User =
  { login: String
  , id: Int
  , avatar_url: String
  }

type alias Users = {items : List User}

type alias Model =
  { term: String
  , isSearchBtnDisabled : Bool
  , isLoading: Bool
  , isRequestFailed: Bool
  , users: Users
  }

model =
  { term = ""
  , isSearchBtnDisabled = True
  , isLoading = False
  , isRequestFailed = False
  , users = {items = []}
  }

searchBox : Model -> Html Msg
searchBox {isLoading, isSearchBtnDisabled} =
  div []
    [ div [ class "field" ]
      [ label [class "label"] [text "Digite o nome do usuário"]
      , input
        [ class "input"
        , onInput OnTermChanged
        , disabled isLoading
        ]
        []
      ]
    , button
        [ class (searchBtnClasses isLoading)
        , onClick OnSearchBtnClicked
        , disabled isSearchBtnDisabled
        ]
        [text "Procurar"]
    ]

searchBtnClasses : Bool -> String
searchBtnClasses isLoading =
  if isLoading then "button is-primary is-large is-loading" else "button is-primary is-large"

header = h1 [class "title is-1 has-text-centered"] [ text "Gitelm" ]

renderUsers : List User -> List (Html Msg)
renderUsers users =
  map renderUserCard users

renderUserCard : User -> Html Msg
renderUserCard {login, avatar_url} =
  div [ class "card" ]
    [ div [ class "card-content" ]
        [ div [ class "media"]
            [ div [ class "media-left" ]
                [ figure [ class "image is-48x48" ]
                    [ img [ src avatar_url ] [] ]
                ]
            , div [ class "media-content" ]
                [ p [ class "is-title is-4" ] [ text ("@" ++ login) ] ]
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
    Http.send SearchUsers (Http.get url decodeData)

decodeData : Decoder Users
decodeData =
  succeed
    Users
      |: (field "items" (list userDecoder))

userDecoder : Decoder User
userDecoder =
  succeed
    User
      |: (field "login" string)
      |: (field "id" int)
      |: (field "avatar_url" string)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

init : (Model, Cmd Msg)
init = ( model , Cmd.none )

view : Model -> Html Msg
view model =
  mainPage
    [ header
    , searchBox model
    , div [ class "section" ] ( renderUsers model.users.items )
    ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    OnTermChanged newTerm ->
      if isEmpty newTerm then
        ({ model | term = newTerm, isSearchBtnDisabled = True }, Cmd.none )
      else
        ({ model | term = newTerm, isSearchBtnDisabled = False }, Cmd.none)
    OnSearchBtnClicked ->
      ({ model | isLoading = True }, searchUsers model.term)
    SearchUsers (Ok s) ->
      ({ model | isLoading = False, users = s }, Cmd.none)
    SearchUsers (Err _) ->
    ({ model | isLoading = False }, Cmd.none)

main = program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
    }
