module Guess exposing (Model, main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


main =
    Html.beginnerProgram { model = model, view = view, update = update }


type alias Model =
    { word : String
    , guess : String
    , isCorrect : Bool
    , revealedWord : { pos : Int, text : String }
    }


model : Model
model =
    Model "Saturday" "" False { pos = 2, text = "S" }


type Msg
    = Answer String
    | Reveal


update : Msg -> Model -> Model
update msg model =
    case msg of
        Answer txt ->
            { model | guess = txt, isCorrect = checkIfCorrect model txt }

        Reveal ->
            { model | revealedWord = revealAndIncrement model }


revealAndIncrement : Model -> { pos : Int, text : String }
revealAndIncrement { revealedWord, word } =
    if revealedWord.text == word then
        revealedWord

    else
        { revealedWord | pos = revealedWord.pos + 1, text = String.slice 0 revealedWord.pos word }

 
checkIfCorrect : Model -> String -> Bool
checkIfCorrect model txt =
    if txt == model.word then
        True

    else
        False


generateResult : Model -> Html Msg
generateResult { isCorrect, revealedWord, word } =
    let
        txt =
            if revealedWord.text == word then
                text "You didn't get it"

            else if isCorrect then
                text "You got it!"

            else
                text "Nope"
    in
        txt


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text 
                    ("I'm thinking of a word that starts with " 
                    ++ model.revealedWord.text
                    ++ " that has "
                    ++ toString (String.length model.word)
                    ++ " letters."
                    
                    ) ]
        , input [ placeholder "Type your guess", onInput Answer ] []
        , button [ onClick Reveal ] [ text "Give me a hint" ]
        , div [] [ generateResult model ]
        ]
