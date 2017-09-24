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
    beginnerProgram)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onInput, onClick)
import String exposing (isEmpty)

type Msg
    = Nothing
    | OnTermChanged String
    | OnSearchBtnClicked

type alias Model =
  { term: String
  , isSearchBtnDisabled : Bool
  , isLoading: Bool
  }

model =
  { term = ""
  , isSearchBtnDisabled = True
  , isLoading = False
  }

update : Msg -> Model -> Model
update msg model =
  case msg of
    OnTermChanged newTerm ->
      if isEmpty newTerm then
        { model | term = newTerm, isSearchBtnDisabled = True}
      else
        { model | term = newTerm, isSearchBtnDisabled = False}
    OnSearchBtnClicked ->
      { model | isLoading = True }
    Nothing ->
      model

view : Model -> Html Msg
view model =
  mainPage
    [ header, searchBox model ]

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

header =
  h1 [class "title is-1 has-text-centered"] [ text "Gitelm" ]

mainPage : List (Html Msg) -> Html Msg
mainPage children =
  section [ class "section" ]
    [ div [ class "container" ] children ]

main = beginnerProgram { model = model , view = view , update = update }
