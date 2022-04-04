-- START:module
module Midoto exposing (main)
-- END:module

-- START:import
import Browser
import Browser.Events exposing (onKeyUp)
import Browser.Dom as Dom
import Html exposing (..)
import Html.Events exposing (onInput, onSubmit)
import Html.Attributes exposing (..)
import Json.Decode as JsonDecode
import Task
import Time
-- END:import

-- START:main
main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
-- END:main

-- START:model
type TodoStatus
    = Active
    | Incomplete
    | Completed

type alias Todo =
    { id : Int
    , name : String
    , workedTime : Float
    , status : TodoStatus
    }

type alias Model =
    { todos : List Todo
    , inputText : String
    , isShowForm : Bool
    , isWorking : Bool
    }

init : () -> (Model, Cmd Msg)
init _ =
    ( { todos = []
      , inputText = ""
      , isShowForm = False
      , isWorking = False
      }
    , Cmd.none
    )
-- END:model

-- START:update
type Msg
    = NoOp
    | PressCharacter Char
    | PressControl String
    | ChangeInput String
    | AddTodo (List String)
    | Check Int
    | Uncheck Int
    | Delete Int
    | ActiveOn Int
    | Start Int
    | Stop
    | Tick

-- END:update

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        PressCharacter keyChar ->
            let
                cmd = if model.isShowForm then Cmd.none else Task.attempt (\_ -> NoOp) (Dom.focus "input-command-box")
            in
            ({ model | isShowForm = (keyChar == 'i' || model.isShowForm ) }
            , cmd
            )
        PressControl keyStr ->
            ({ model | isShowForm = checkControlKey keyStr model.isShowForm }
            , Cmd.none
            )
        ChangeInput input ->
            ({ model | inputText = input }
            , Cmd.none
            )
        NoOp ->
            ({ model | inputText = "" }
            , Cmd.none
            )         
        AddTodo options ->
            ({ model
                | todos = addToTodos (String.join " " options) model.todos
                , inputText = ""
            }
            , Cmd.none
            )
        Check index ->
            ({ model
                | todos = setCompleteTodo index Completed model.todos
                , inputText = ""
            }
            , Cmd.none
            )
        Uncheck index ->
            ({ model
                | todos = setCompleteTodo index Incomplete model.todos
                , inputText = ""
            }
            , Cmd.none
            )
        Delete index ->
            ({ model
                | todos = deleteTodo index model.todos
                , inputText = ""
            }
            , Cmd.none
            )
        ActiveOn index ->
            ({ model
                | todos = setActiveTodo index model.todos
                , inputText = ""
            }
            , Cmd.none
            )
        Start index->
            ({ model
                | isWorking = True
                , todos = startTodo index model.todos
                , inputText = ""
            }
            , Cmd.none
            )
        Stop ->
            ({ model
                | isWorking = False
                , inputText = ""
            }
            , Cmd.none
            )
        Tick ->
            ({ model
                | todos = updateWorkedTime model.todos
            }
            , Cmd.none
            )

lastElem : List a -> Maybe a
lastElem list =
    case list of
        [] ->
            Nothing
        [last] ->
            Just last
        _::rest ->
            lastElem rest

addToTodos : String -> List Todo -> List Todo
addToTodos input todos =
    let
        lastIndex =
            case lastElem todos of
                Just t ->
                    t.id + 1
                Nothing ->
                    1
    in
    todos ++ [ Todo lastIndex input 0 Incomplete]

setCompleteTodo : Int -> TodoStatus -> List Todo -> List Todo
setCompleteTodo index completedStatus todos =
    List.map (\todo ->
        if todo.id == index then
            { todo | status = completedStatus }
        else
            todo
    ) todos

setActiveTodo : Int -> List Todo -> List Todo
setActiveTodo index todos =
    List.map (\todo ->
        if todo.id == index then
            { todo | status = Active }
        else if todo.status == Active then
            { todo | status = Incomplete }
        else
            todo
    ) todos

hasActiveTodo : List Todo -> Bool
hasActiveTodo todos =
    (List.filter (\todo -> todo.status == Active) todos |> List.length) > 0

startTodo : Int -> List Todo -> List Todo
startTodo index todos =
    if hasActiveTodo todos then
        List.map (\todo ->
            if todo.status == Active then
                { todo | status = Active }
            else
                todo
        ) todos
    else
        List.map (\todo ->
            if todo.id == index then
                { todo | status = Active }
            else
                todo
        ) todos

deleteTodo : Int -> List Todo -> List Todo
deleteTodo index todos =
    List.filter (\todo -> todo.id /= index) todos

updateWorkedTime : List Todo -> List Todo
updateWorkedTime todos =
    List.map (\todo ->
        if todo.status == Active then
            { todo | workedTime = todo.workedTime + 1 }
        else
            todo
    ) todos

checkControlKey : String -> Bool -> Bool
checkControlKey keyStr isShowForm =
    if isShowForm && (keyStr == "Enter" || keyStr == "Escape") then
        False
    else
        isShowForm

-- START:subscription
keyDecoder : JsonDecode.Decoder Msg
keyDecoder =
    JsonDecode.map toKey (JsonDecode.field "key" JsonDecode.string)

toKey : String -> Msg
toKey string =
    case String.uncons string of
        Just ( char, "" ) ->
            PressCharacter char

        _ ->
            PressControl string

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ onKeySub, (timerSub model) ]

onKeySub : Sub Msg
onKeySub =
    onKeyUp keyDecoder

timerSub : Model -> Sub Msg
timerSub model =
    if model.isWorking then
        Time.every 1000 (\_ -> Tick)
    else
        Sub.none
-- END:subscription

-- START:view
view : Model -> Html Msg
view model =
    div [ style "width" "100vw", style "height" "100vh", style "overflow" "hidden" ]
        [
            Html.form 
                ([ onSubmit (parseMsg (tokenize model.inputText) model.todos)
                , style "justify-content" "center"
                , style "align-items" "center"
                , style "position" "absolute"
                , style "height" "100vh"
                , style "width" "100vw"
                , style "background" "rgba(0, 0, 0, 0.5)"
                ] ++ displayForm model.isShowForm)
                [ input
                    ([ placeholder "What do you want to do?"
                    , id "input-command-box"
                    , value model.inputText
                    , onInput ChangeInput
                    ] ++ styleInputBox
                    )
                    []
                ]
            , div
                styleApplicationBody
                [ div styleOfListBox <| (h3 [style "margin-left" "20px"] [ text "On Going Tasks" ])::(List.map viewTodo <| onGoingTodos model.todos)
                , div styleOfListBox <| (h3 [style "margin-left" "20px"] [ text "Completed Tasks" ])::(List.map viewTodo <| completedTodos model.todos)
                ]

        ]

viewTodo : (Int, Todo) -> Html Msg
viewTodo (uiIndex, todo) =
    p []
        [ div
            [ style "text-decoration"
                (if todo.status == Completed then
                    "line-through"
                else
                    "none"
                )
            , style "font-weight"
                (if todo.status == Active then
                    "600"
                else
                    "300"
                )
            , style "display" "flex"
            , style "justify-content" "space-between"
            , style "margin-left" "16px"
            , style "margin-right" "16px"
            ]
            [ span [] [ text <| (String.fromInt uiIndex) ++ ". " ++ todo.name ]
            , span [ style "display"
                    ( if todo.status == Active || todo.status == Completed then
                        "flex"
                    else
                        "none"
                    )
                ]
                [ text <| parseWorkingTimeToString todo.workedTime ]
            ]
        ]

onGoingTodos : List Todo -> List (Int, Todo)
onGoingTodos todos =
    List.filter (\todo -> todo.status /= Completed) todos |> List.indexedMap (\x y -> (x+1, y))

completedTodos : List Todo -> List (Int, Todo)
completedTodos todos =
    List.filter (\todo -> todo.status == Completed) todos |> List.indexedMap (\x y -> (x+1, y))

getTrueIndex : Maybe Int -> List (Int, Todo) -> Maybe Int
getTrueIndex uiIndex todoTuples =
    case todoTuples of
        [] ->
            Nothing
        [(_, x)] ->
            Just x.id
        xs ->
            List.filter (\(x, _) -> uiIndex == Just x) xs |> List.map (\(_, y) -> y.id) |> List.head

tokenize : String -> List String
tokenize input =
    String.words input

toTwoCharString : Float -> String
toTwoCharString num =
    if num > 9 then
        String.fromFloat num
    else
        "0" ++ String.fromFloat num

parseWorkingTimeToString : Float -> String
parseWorkingTimeToString workingTime =
    let
        hours = workingTime / (3600)
        hoursRounded = hours |> truncate |> toFloat
        minutes = (workingTime - hoursRounded * 3600) / 60
        minutesRouded = minutes |> truncate |> toFloat
        seconds = workingTime - hoursRounded * 3600 - minutesRouded * 60
    in
        (String.fromFloat hoursRounded) ++ ":" ++ toTwoCharString minutesRouded ++ ":" ++ toTwoCharString seconds

parseCommandUseIndex : (Int -> Msg) -> List String -> List (Int, Todo) -> Msg
parseCommandUseIndex command list todoTuples =
    let
        parseMaybeInt maybeInt =
            case maybeInt of
                Just i ->
                    command i
                Nothing ->
                    NoOp
    in
    case list of
        [] ->
            NoOp
        [x] ->
            parseMaybeInt (getTrueIndex (String.toInt x) todoTuples)
        x::_ ->
            parseMaybeInt (getTrueIndex (String.toInt x) todoTuples)

parseMsg : List String -> List Todo -> Msg
parseMsg list todos =
    case list of
        [] ->
            NoOp
        [x] ->
            case String.toLower x of
                "start" ->
                    case (List.map Tuple.first (onGoingTodos todos) |> List.head) of
                        Just i ->
                            Start i
                        Nothing ->
                            NoOp
                "stop" ->
                    Stop
                _ ->
                    NoOp
        x::xs ->
            case String.toLower x of
                "add" ->
                    AddTodo xs
                "a" ->
                    AddTodo xs
                "check" ->
                    parseCommandUseIndex Check xs (onGoingTodos todos)
                "c" ->
                    parseCommandUseIndex Check xs (onGoingTodos todos)
                "uncheck" ->
                    parseCommandUseIndex Uncheck xs (completedTodos todos)
                "uc" ->
                    parseCommandUseIndex Uncheck xs (completedTodos todos)
                "delete" ->
                    parseCommandUseIndex Delete xs (onGoingTodos todos)
                "d" ->
                    parseCommandUseIndex Delete xs (onGoingTodos todos)
                "wk" ->
                    parseCommandUseIndex ActiveOn xs (onGoingTodos todos)
                _ ->
                    AddTodo list

displayForm : Bool -> List (Html.Attribute msg)
displayForm isShowForm =
    if isShowForm then
        [ style "display" "flex" ]
    else
        [ style "display" "none"]

styleApplicationBody : List (Html.Attribute msg)
styleApplicationBody =
    [ style "display" "flex"
    , style "flex-direction" "row"
    , style "justify-content" "space-evenly"
    , style "align-items" "center"
    , style "padding-right" "50px"
    , style "width" "100%"
    , style "height" "100%"
    ]

styleOfListBox : List (Html.Attribute msg)
styleOfListBox =
    [ style "padding" "10px"
    , style "border" "1px solid #666"
    , style "border-radius" "4px"
    , style "width" "40%"
    , style "height" "80%"
    , style "font-size" "16px"
    , style "overflow" "auto"
    ]

styleInputBox : List (Html.Attribute msg)
styleInputBox =
    [ style "font-size" "16px"
    , style "letter-spacing" "0.8px"
    , style "height" "45px"
    , style "width" "500px"
    , style "padding" "0px 10px"
    , style "border" "1px solid #666"
    , style "border-radius" "4px"
    , style "box-shadow" "0 0 50px rgba(0, 0, 0, 0.25)"
    ]

-- END:view
