module Main exposing (main)

import Array exposing (Array)
import Browser exposing (Document)
import Browser.Dom
import Css
import Css.Global
import Html as H exposing (Attribute, Html)
import Html.Attributes as HA
import Html.Events as HE
import Html.Lazy
import Html.Styled
import Json.Decode as Decode exposing (Decoder)
import Random exposing (Generator, Seed)
import Random.Array
import Regex exposing (Regex)
import Task exposing (Task)
import Time


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { buffer : Array String }


type alias RowCol =
    { row : Int
    , col : Int
    }


init _ =
    ( { buffer = Array.empty }
    , Task.perform RandomBuffer (randomBuffer 120 500 |> randomToTask)
    )


type Msg
    = Scroll ScrollEvent
    | RandomBuffer (Array String)


update msg model =
    case Debug.log "update" msg of
        RandomBuffer buffer ->
            ( { model | buffer = buffer }, Cmd.none )

        _ ->
            ( model, Cmd.none )


subscriptions _ =
    Sub.none



-- Styling


fontSize : Float
fontSize =
    15


lineHeight : Float
lineHeight =
    fontSize * 1.4


global : List Css.Global.Snippet
global =
    [ Css.Global.class "editor-main"
        [ Css.position Css.relative
        , Css.px 800 |> Css.width
        , Css.px 406 |> Css.height
        ]
    , Css.Global.class "editor-main-inner"
        [ Css.displayFlex
        , Css.flexDirection Css.row
        , Css.fontFamily Css.monospace
        , Css.whiteSpace Css.pre
        ]
    , Css.Global.class "content-main"
        [ Css.position Css.relative
        , Css.property "flex" "1"
        , Css.property "user-select" "none"
        , Css.em 1 |> Css.marginLeft
        , Css.em 1 |> Css.marginRight
        ]
    , Css.Global.class "v-scroll-bar"
        [ Css.position Css.absolute
        , Css.overflowX Css.hidden
        , Css.overflowY Css.scroll
        , Css.px 0 |> Css.right
        , Css.px 0 |> Css.top
        , Css.px 0 |> Css.bottom
        ]
    , Css.Global.class "v-scroll-bar-inner"
        [ Css.px 1 |> Css.minWidth
        , Css.px 775 |> Css.height
        ]
    , Css.Global.class "h-scroll-bar"
        [ Css.position Css.absolute
        , Css.overflowX Css.scroll
        , Css.overflowY Css.hidden
        , Css.px 0 |> Css.left
        , Css.px 0 |> Css.right
        , Css.px 0 |> Css.bottom
        , Css.int 5 |> Css.zIndex
        ]
    , Css.Global.class "h-scroll-bar-inner"
        [ Css.px 1 |> Css.minHeight
        , Css.pct 100 |> Css.height
        , Css.px 1600 |> Css.width
        ]
    , Css.Global.class "code-line-numbers"
        [ Css.em 2 |> Css.width
        , Css.textAlign Css.right
        , Css.displayFlex
        , Css.flexDirection Css.column
        , Css.property "user-select" "none"
        ]
    , Css.Global.class "content-line"
        [ Css.position Css.absolute
        , Css.px 0 |> Css.left
        , Css.px 0 |> Css.right
        , Css.px lineHeight |> Css.height
        ]
    ]



-- View


view : Model -> Document Msg
view model =
    { title = "Scroll Spike"
    , body =
        [ Css.Global.global global |> Html.Styled.toUnstyled
        , editorView model
        ]
    }


editorView : Model -> Html Msg
editorView model =
    H.div [ HA.class "editor-main" ]
        [ viewVScrollBar
        , viewHScrollBar
        , H.div
            [ HA.class "editor-main-inner"
            , HA.id "editor"
            , HA.tabindex 0
            ]
            [ viewLineNumbers model
            , viewContent model
            ]
        ]


viewVScrollBar : Html Msg
viewVScrollBar =
    H.div
        [ HA.class "v-scroll-bar"
        , HE.on "scroll" scrollDecoder
        ]
        [ H.div [ HA.class "v-scroll-bar-inner" ] [] ]


viewHScrollBar : Html Msg
viewHScrollBar =
    H.div
        [ HA.class "h-scroll-bar"
        , HE.on "scroll" scrollDecoder
        ]
        [ H.div [ HA.class "h-scroll-bar-inner" ] [] ]


viewLineNumbers : Model -> Html Msg
viewLineNumbers model =
    H.div
        [ HA.class "code-line-numbers"
        ]
        (List.range 1 (Array.length model.buffer)
            |> List.map viewLineNumber
        )


viewLineNumber : Int -> Html Msg
viewLineNumber n =
    H.span [] [ H.text (String.fromInt n) ]


viewContent : Model -> Html Msg
viewContent model =
    H.div
        [ HA.class "content-main" ]
        [ viewLines model.buffer ]


viewLines : Array String -> Html Msg
viewLines buffer =
    H.div []
        (indexedFoldl
            (\idx row accum ->
                viewLine buffer idx row :: accum
            )
            []
            buffer
        )


viewLine : Array String -> Int -> String -> Html Msg
viewLine buffer row content =
    H.div
        [ HA.class "content-line"
        , HA.style "top" (String.fromFloat (toFloat row * lineHeight) ++ "px")
        ]
        (viewChars buffer row content)


viewChars : Array String -> Int -> String -> List (Html Msg)
viewChars buffer row content =
    content
        |> String.toList
        |> List.indexedMap (viewChar buffer row)


viewChar : Array String -> Int -> Int -> Char -> Html Msg
viewChar buffer row col char =
    H.span
        []
        [ H.text (String.fromChar char) ]



-- Scroll events


type alias ScrollEvent =
    { scrollTop : Maybe Float
    , scrollHeight : Maybe Float
    , scrollLeft : Maybe Float
    , scrollWidth : Maybe Float
    }


scrollDecoder : Decoder Msg
scrollDecoder =
    Decode.succeed ScrollEvent
        |> andMap (Decode.at [ "target", "scrollTop" ] Decode.float |> Decode.maybe)
        |> andMap (Decode.at [ "target", "scrollHeight" ] Decode.float |> Decode.maybe)
        |> andMap (Decode.at [ "target", "scrollLeft" ] Decode.float |> Decode.maybe)
        |> andMap (Decode.at [ "target", "scrollWidth" ] Decode.float |> Decode.maybe)
        |> Decode.map Scroll



-- Random buffer initialization.


randomBuffer : Int -> Int -> Generator (Array String)
randomBuffer width length =
    let
        regex =
            Regex.fromString "(\\b[^\\s]+\\b)"
                |> Maybe.withDefault Regex.never

        wordList =
            Regex.find regex lorumIpsum
                |> List.map (.match >> String.toLower)
                |> Array.fromList

        wordGenerator : Generator (Maybe String)
        wordGenerator =
            Random.Array.sample wordList

        line : Int -> List String -> Generator (Maybe String) -> Generator String
        line curLength curLine generator =
            if curLength >= width then
                String.concat curLine
                    |> Random.constant

            else
                generator
                    |> Random.andThen
                        (\randomWord ->
                            case randomWord of
                                Nothing ->
                                    -- This should not happen.
                                    line curLength curLine generator

                                Just val ->
                                    line (curLength + String.length val + 1)
                                        ((val ++ " ") :: curLine)
                                        generator
                        )

        lines =
            line 0 [] wordGenerator
                |> Random.Array.array length
    in
    lines


lorumIpsum : String
lorumIpsum =
    """
  Lorem ipsum dolor sit amet, consectetur adipiscing elit. Cras ut feugiat orci. Ut cursus malesuada nunc id tempor. Nam dignissim luctus mi ac vestibulum. Fusce fermentum purus quis rutrum facilisis. Sed ut justo ac nulla ornare dictum. Ut condimentum pellentesque volutpat. Aliquam sapien eros, ornare eget nisi porta, mattis lobortis mauris. In hac habitasse platea dictumst. Aliquam scelerisque risus sed luctus accumsan.

Mauris posuere pellentesque urna, in consectetur enim tempor volutpat. Nulla convallis, turpis nec convallis eleifend, nisi elit vulputate nibh, nec cursus tellus purus eu mauris. Nulla facilisi. Ut placerat vulputate pharetra. Etiam libero est, eleifend quis semper ac, fringilla vitae arcu. Quisque ut cursus leo. Suspendisse augue tortor, venenatis at ex sit amet, fringilla malesuada ligula. Phasellus nulla nibh, mollis ut vulputate quis, congue vitae mi. Nunc porta, ex quis luctus scelerisque, eros mauris placerat mauris, vitae finibus leo nulla in lacus. Donec ac ex leo. Aliquam ut quam tincidunt, maximus neque et, hendrerit magna.

Integer elementum leo lacinia risus pharetra, sit amet condimentum felis porta. Phasellus sollicitudin mauris at risus semper, in facilisis ex dictum. Vestibulum tincidunt eros a vehicula dignissim. Quisque a ex et arcu bibendum congue. Praesent gravida nulla metus, sed luctus justo fermentum et. Nullam scelerisque, felis tempor placerat eleifend, neque risus dictum nisi, eget ullamcorper massa mauris non metus. Nam rhoncus mollis justo, eu luctus arcu pharetra et. Aenean auctor et massa tempus consectetur. Duis tempus nunc volutpat dolor pellentesque, non imperdiet purus sagittis. Sed ultricies neque vel condimentum tincidunt. Suspendisse ornare sodales risus, sed tincidunt tortor rutrum sed. Suspendisse pellentesque quis quam vel elementum.

Sed blandit orci ut lectus efficitur tempor. Maecenas vitae risus sodales leo fringilla posuere. Sed facilisis magna non eros porttitor, a molestie neque ultricies. Nulla ac sapien lacus. Aliquam erat volutpat. Etiam volutpat sem mauris, vitae luctus neque imperdiet non. Ut condimentum eget ipsum lobortis eleifend. Ut dignissim laoreet fringilla. Ut sit amet metus pharetra, malesuada diam vel, convallis ipsum.

Integer ac pellentesque turpis, id placerat libero. Fusce commodo mauris vitae augue laoreet, id dignissim enim placerat. Suspendisse et tellus semper, dictum nibh quis, tempus est. Etiam sagittis non lectus eu dapibus. Nullam metus nunc, lacinia ut varius et, commodo quis metus. In vel efficitur nisl. Mauris ac mi sed dolor scelerisque vulputate. In gravida urna ut tempus tempor. Quisque pulvinar velit ac lacus gravida vulputate. Integer ante odio, ultricies a posuere ut, sollicitudin ut risus.

Nullam volutpat consequat metus ac gravida. Curabitur iaculis nibh leo, non lacinia velit porta vitae. Aliquam convallis libero sed quam pharetra, eget cursus ex sagittis. Donec sodales in libero et finibus. Nunc rhoncus eleifend odio maximus sollicitudin. Fusce euismod erat quis enim cursus, eget imperdiet lectus rhoncus. Integer aliquet, nunc nec posuere condimentum, ex mauris fringilla urna, sit amet fermentum neque risus eu felis. Suspendisse tortor nibh, commodo et varius a, pulvinar vel urna. Nam porta aliquet egestas. Nam in fringilla ipsum. Praesent gravida nisl nec arcu pretium, pharetra vestibulum dolor placerat. Nullam rutrum in dolor ac mollis. Duis ornare laoreet enim.
  """



-- Helpers


indexedFoldl : (Int -> String -> b -> b) -> b -> Array String -> b
indexedFoldl fn accum buffer =
    Array.foldl (\val ( idx, acc ) -> ( idx + 1, fn idx val acc ))
        ( 0, accum )
        buffer
        |> Tuple.second


randomToTask : Generator a -> Task x a
randomToTask generator =
    Time.now
        |> Task.map (Tuple.first << Random.step generator << Random.initialSeed << Time.posixToMillis)


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap =
    Decode.map2 (|>)
