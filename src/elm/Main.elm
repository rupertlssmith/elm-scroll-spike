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
import Task


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
    ( { buffer = Array.fromList [ "one", "two", "three" ] }
    , Cmd.none
    )


type Msg
    = Scroll ScrollEvent


update msg model =
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
        , Html.Lazy.lazy editorView model
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
    }


scrollDecoder : Decoder Msg
scrollDecoder =
    Decode.succeed ScrollEvent
        |> andMap (Decode.at [ "target", "scrollTop" ] Decode.float |> Decode.maybe)
        |> andMap (Decode.at [ "target", "scrollHeight" ] Decode.float |> Decode.maybe)
        |> Decode.map Scroll


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap =
    Decode.map2 (|>)



-- Helpers


indexedFoldl : (Int -> String -> b -> b) -> b -> Array String -> b
indexedFoldl fn accum buffer =
    Array.foldl (\val ( idx, acc ) -> ( idx + 1, fn idx val acc ))
        ( 0, accum )
        buffer
        |> Tuple.second
