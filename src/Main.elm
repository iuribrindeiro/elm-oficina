module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Todo =
    { title : String
    , completed : Bool
    }


type alias Model =
    { newTodoTitle : String
    , todos : List Todo
    }


init : Model
init =
    Model "" []



-- UPDATE


type Msg
    = ChangeInputContent String
    | AddTodo
    | ToggleTodo Todo


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeInputContent content ->
            { model | newTodoTitle = content }

        AddTodo ->
            { todos = Todo model.newTodoTitle False :: model.todos, newTodoTitle = "" }

        ToggleTodo todo ->
            { model | todos = model.todos |> List.map (toggleTodo todo) }


toggleTodo : Todo -> Todo -> Todo
toggleTodo todo t =
    if t == todo then
        { t | completed = not t.completed }

    else
        t



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "display" "flex", style "flex-direction" "column", style "align-items" "center" ]
        [ div [ style "display" "flex", style "flex-direction" "row", style "justify-content" "space-between" ]
            [ input [ onInput ChangeInputContent, value model.newTodoTitle ] []
            , button [ onClick AddTodo ] [ text "Adicionar" ]
            ]
        , ul [] (List.map viewTodo model.todos)
        ]


viewTodo todo =
    li
        [ onClick (ToggleTodo todo)
        , style "text-decoration"
            (if todo.completed then
                "line-through"

             else
                "none"
            )
        , style "cursor" "pointer"
        ]
        [ text todo.title ]
