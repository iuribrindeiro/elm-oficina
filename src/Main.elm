module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (bool, field, map2, string)



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }



-- MODEL


type alias Todo =
    { title : String
    , completed : Bool
    }


type alias Model =
    { newTodoTitle : String
    , todos : List Todo
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" []
    , Http.get
        { url = "https://jsonplaceholder.typicode.com/todos"
        , expect = Http.expectJson GotTodos todoDecoder
        }
    )


todoDecoder : Json.Decode.Decoder (List Todo)
todoDecoder =
    map2 Todo
        (field "title" string)
        (field "completed" bool)
        |> Json.Decode.list



-- UPDATE


type Msg
    = ChangeInputContent String
    | AddTodo
    | ToggleTodo Todo
    | GotTodos (Result Http.Error (List Todo))


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        ChangeInputContent content ->
            ( { model | newTodoTitle = content }, Cmd.none )

        AddTodo ->
            ( { todos = model.todos ++ [ Todo model.newTodoTitle False ], newTodoTitle = "" }, Cmd.none )

        ToggleTodo todo ->
            ( { model | todos = model.todos |> List.map (toggleTodo todo) }, Cmd.none )

        GotTodos (Ok todos) ->
            ( { model | todos = todos |> List.take 10 }, Cmd.none )

        GotTodos (Err _) ->
            ( model, Cmd.none )


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
        , ul [] (model.todos |> List.map viewTodo)
        ]


viewTodo : Todo -> Html Msg
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
