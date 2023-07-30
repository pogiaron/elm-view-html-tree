module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Html.Parser exposing (..)
import Http
import Parser exposing (..)
import Tree exposing (..)

import RemoteData exposing (..)

-- MAIN


main : Program () Model Msg
main =
  Browser.document
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL
type alias Model = 
    { url: String
    , htmlStr: String
    , htmlTreeWebData: WebDataParsedHtml
    }

type alias WebDataParsedHtml = WebData (Result (List DeadEnd) (Tree HtmlNode))

type HtmlNode = HtmlText String | HtmlComment String | HtmlElement String (List ( String, String ))

-- Function to convert Node (from danneu/html-parser) to Tree (zwilias/elm-rosetree )
convertToTree : Node -> Tree HtmlNode
convertToTree node =
    case node of
        Text text ->
            tree (HtmlText text) []

        Comment comment ->
            tree (HtmlComment comment) []

        Element tag attributes children ->
           let
                childNodes =
                    List.filterMap (\child -> case child of
                        Comment _ -> Nothing -- Skip Comment nodes
                        Text text -> 
                            if List.any (\n -> n == text) ["\n", ""] 
                                then 
                                    Nothing 
                                else 
                                    Just (convertToTree child) --remove empty lines and string
                        _ -> Just (convertToTree child)
                    ) children
            in
            tree (HtmlElement tag attributes) childNodes --(List.map convertToTree children)

init : () -> (Model, Cmd Msg)
init _ =
  ( { url = "", htmlTreeWebData = NotAsked, htmlStr = ""}
  , Cmd.none
  )



-- UPDATE


type Msg
  = UrlChange String
  | GotHtml (WebData String)
  | GetUrl

  | HtmlStrChange String
  | ParseHtmlStr


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UrlChange url ->
        ({model | url = url}, Cmd.none)
    GetUrl ->
        ({model | htmlTreeWebData = Loading}, get model.url)
    GotHtml result ->
        let
            htmlTreeWebData = 
                result 
                |> RemoteData.map (\htmlStr -> runDocument allCharRefs htmlStr) --parse the html tree
                |> RemoteData.map (\document -> Result.map (\doc -> doc.root |> convertToTree) document) --convert to Tree HtmlNode
        in
        ({model | htmlTreeWebData = htmlTreeWebData}, Cmd.none)
    
    HtmlStrChange htmlStr ->
        ({model | htmlStr = htmlStr}, Cmd.none)
    
    ParseHtmlStr ->
        let
            htmlTreeWebData = 
                model.htmlStr 
                |> runDocument allCharRefs 
                |> Result.map (\doc -> doc.root |> convertToTree)
        in
        ({model | htmlTreeWebData = Success htmlTreeWebData }, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "html structure view"
    , body = 
        [ Html.node "link" [ rel "stylesheet", href "./css/style.css"] []
        , div [] 
            [ div [] 
                [ input [onInput UrlChange] []
                , button [onClick GetUrl] [text "View html structure"]
                ]
            , div []
                [ textarea [onInput HtmlStrChange] []
                , button [onClick ParseHtmlStr] [text "Parse and view html structure"]
                ]
            , viewHtmlTreeWebData model.htmlTreeWebData
            ]
        ]
    }

viewHtmlTreeWebData : WebDataParsedHtml -> Html msg
viewHtmlTreeWebData htmlTreeWebData =
    case htmlTreeWebData of
        NotAsked -> div [] []
        Loading -> div [] [text "Loading..."]
        Failure err -> div [] [text (Debug.toString err)]
        Success htmlTreeParsed ->
            case htmlTreeParsed of
                Ok htmlTree ->
                    viewHtmlTree htmlTree
                Err error ->
                    div [] [text (Debug.toString error)]


viewHtmlTree htmlTree =
    htmlTree
    |> restructure labelToHtml toListItems
    |> \root -> Html.ul [class ["tree"]] [ root ]

labelToHtml : HtmlNode -> Html msg
labelToHtml htmlNode =
    let
        _ = htmlNode |> Debug.toString |> Debug.log "htmlNode"
    in
    case htmlNode of
        HtmlText str ->
            Html.code [] [text str]
        HtmlComment str ->
            Html.code [] [text str]
        HtmlElement tag _ ->
            Html.code [] [text tag]


toListItems : Html msg -> List (Html msg) -> Html msg
toListItems label children =
    case children of
        [] ->
            Html.li [] [ label ]
        _ ->
            Html.li []
                [ label
                , Html.ul [] children
                ]
class : List String -> Html.Attribute msg
class classes =
    attribute "class" (String.join " " classes)


-- HTTP
get : String -> Cmd Msg
get url =
    Http.request
        { method = "GET"
        , headers = [ Http.header "x-cors-api-key" "temp_57f9c415799143dac880db307c8b9926" ]
        , url = "https://proxy.cors.sh/${url}" |> String.replace "${url}" url
        , body = Http.emptyBody
        , expect = Http.expectString (fromResult >> GotHtml)
        , timeout = Nothing
        , tracker = Nothing
        }