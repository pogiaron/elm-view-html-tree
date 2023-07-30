module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Parser exposing (..)
import Http
import Parser exposing (..)
import RemoteData exposing (..)
import Tree exposing (Tree)



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
    { url : String
    , htmlStr : String
    , htmlTreeWebData : WebDataParsedHtml
    , logo : Maybe (Tree HtmlNode)
    }


type alias WebDataParsedHtml =
    WebData (Result (List DeadEnd) (Tree HtmlNode))


type HtmlNode
    = HtmlText String
    | HtmlComment String
    | HtmlElement String (List ( String, String ))



-- Function to convert Node (from danneu/html-parser) to Tree (zwilias/elm-rosetree )


convertToTree : Node -> Tree HtmlNode
convertToTree node =
    case node of
        Text text ->
            Tree.tree (HtmlText text) []

        Comment comment ->
            Tree.tree (HtmlComment comment) []

        Element tag attributes children ->
            let
                childNodes =
                    List.filterMap
                        (\child ->
                            case child of
                                Comment _ ->
                                    Nothing

                                -- Skip Comment nodes
                                Text text ->
                                    if List.any (\n -> n == text) [ "\n", "" ] then
                                        Nothing

                                    else
                                        Just (convertToTree child)

                                --remove empty lines and string
                                _ ->
                                    Just (convertToTree child)
                        )
                        children
            in
            Tree.tree (HtmlElement tag attributes) childNodes



--(List.map convertToTree children)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { url = "", htmlTreeWebData = NotAsked, htmlStr = "", logo = Nothing }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UrlChange String
    | GotHtml (WebData String)
    | GetUrl
    | HtmlStrChange String
    | ParseHtmlStr


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange url ->
            ( { model | url = url }, Cmd.none )

        GetUrl ->
            ( { model | htmlTreeWebData = Loading }, get model.url )

        GotHtml result ->
            let
                htmlTreeWebData =
                    result
                        |> RemoteData.map (\htmlStr -> runDocument allCharRefs (String.trimLeft htmlStr))
                        -- parse the html tree
                        |> RemoteData.map (\document -> Result.map (\doc -> doc.root |> convertToTree) document)

                -- convert to Tree HtmlNode
                links =
                    htmlTreeWebData
                        -- Inside RemoteData a Result is wrapped the code below unpack that to Maybe (List String)
                        |> RemoteData.toMaybe
                        -- Maybe
                        |> Maybe.andThen
                            (\htmlTreeParseResult ->
                                htmlTreeParseResult
                                    -- Parser Result
                                    |> Result.toMaybe
                                    -- Maybe
                                    |> Maybe.map findLinks
                            )

                _ =
                    Debug.toString links |> Debug.log "links"

                logoHtmlElement =
                    htmlTreeWebData
                        |> RemoteData.toMaybe
                        |> Maybe.andThen
                            (\htmlTreeParseResult ->
                                htmlTreeParseResult
                                    -- Parser Result
                                    |> Result.toMaybe
                                    -- Maybe
                                    |> Maybe.map (findLogoElements [])
                            )
                        |> Maybe.andThen (\logoElements -> List.head logoElements)

                _ =
                    Debug.toString logoHtmlElement |> Debug.log "logo"
            in
            ( { model | htmlTreeWebData = htmlTreeWebData, logo = logoHtmlElement }, Cmd.none )

        HtmlStrChange htmlStr ->
            ( { model | htmlStr = htmlStr }, Cmd.none )

        ParseHtmlStr ->
            let
                htmlTreeWebData =
                    model.htmlStr
                        |> runDocument allCharRefs
                        |> Result.map (\doc -> doc.root |> convertToTree)
            in
            ( { model | htmlTreeWebData = Success htmlTreeWebData }, Cmd.none )


findLinks : Tree HtmlNode -> List String
findLinks htmlTree =
    let
        transformFn =
            \htmlNode ->
                case htmlNode of
                    HtmlElement "a" attributes ->
                        attributes
                            |> List.filterMap filterMapHref
                            |> List.head

                    _ ->
                        Nothing
    in
    htmlTree
        |> filterMapTree transformFn


filterMapHref : ( String, String ) -> Maybe String
filterMapHref ( attribute, value ) =
    if "href" == attribute && value /= "" && not (String.contains "mailto" value) then
        Just value

    else
        Nothing


hasHref : List ( String, String ) -> Bool
hasHref attrs =
    List.any (\( key, value ) -> String.toLower key == "href") attrs


filterMapTree : (a -> Maybe b) -> Tree a -> List b
filterMapTree transform tree =
    case transform (Tree.label tree) of
        Just result ->
            result :: List.concatMap (filterMapTree transform) (Tree.children tree)

        Nothing ->
            List.concatMap (filterMapTree transform) (Tree.children tree)



-- Function to find all elements in the tree that match the given predicate


filterTree : (a -> Bool) -> Tree a -> List a
filterTree predicate tree =
    if predicate (Tree.label tree) then
        Tree.label tree :: List.concatMap (filterTree predicate) (Tree.children tree)

    else
        List.concatMap (filterTree predicate) (Tree.children tree)


findLogoElements : List String -> Tree HtmlNode -> List (Tree HtmlNode)
findLogoElements classAccumulator node =
    let
        currentNode =
            Tree.label node

        children =
            Tree.children node

        classAccumulatorNew =
            \attributes ->
                let
                    classes =
                        List.filterMap
                            (\( key, value ) ->
                                if key == "class" then
                                    Just value

                                else
                                    Nothing
                            )
                            attributes
                in
                classes ++ classAccumulator
    in
    case currentNode of
        HtmlElement "svg" attributes ->
            if hasLogoClass (classAccumulatorNew attributes) then
                [ node ]

            else
                []

        HtmlElement "img" attributes ->
            if hasLogoClass (classAccumulatorNew attributes) then
                [ node ]

            else
                []

        HtmlElement _ attributes ->
            List.concatMap (findLogoElements (classAccumulatorNew attributes)) children

        _ ->
            []


hasLogoClass : List String -> Bool
hasLogoClass classes =
    List.any (\c -> String.contains "logo" c) classes



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "html structure view"
    , body =
        [ Html.node "link" [ rel "stylesheet", href "./css/style.css" ] []
        , div []
            [ div []
                [ input [ onInput UrlChange ] []
                , button [ onClick GetUrl ] [ text "View html structure" ]
                ]
            , div []
                [ model.logo
                    |> Maybe.map (\logoHtml -> renderHtml logoHtml)
                    |> Maybe.withDefault (text "")
                ]

            -- , div []
            --     [ textarea [onInput HtmlStrChange] []
            --     , button [onClick ParseHtmlStr] [text "Parse and view html structure"]
            --     ]
            , viewHtmlTreeWebData model.htmlTreeWebData
            ]
        ]
    }


renderHtml : Tree HtmlNode -> Html msg
renderHtml htmlTree =
    let
        currentNode =
            Tree.label htmlTree

        children =
            Tree.children htmlTree
    in
    case currentNode of
        HtmlElement tag attributes ->
            Html.node tag
                (List.map (\( key, val ) -> attribute key val) attributes)
                (List.map renderHtml children)

        HtmlText t ->
            text t

        _ ->
            text ""


viewHtmlTreeWebData : WebDataParsedHtml -> Html msg
viewHtmlTreeWebData htmlTreeWebData =
    case htmlTreeWebData of
        NotAsked ->
            div [] []

        Loading ->
            div [] [ text "Loading..." ]

        Failure err ->
            div [] [ text (Debug.toString err) ]

        Success htmlTreeParsed ->
            case htmlTreeParsed of
                Ok htmlTree ->
                    viewHtmlTree htmlTree

                Err error ->
                    div [] [ text (Debug.toString error) ]


viewHtmlTree htmlTree =
    htmlTree
        |> Tree.restructure labelToHtml toListItems
        |> (\root -> Html.ul [ class "tree" ] [ root ])


labelToHtml : HtmlNode -> Html msg
labelToHtml htmlNode =
    case htmlNode of
        HtmlText str ->
            Html.code [] [ text str ]

        HtmlComment str ->
            Html.code [] [ text str ]

        HtmlElement tag _ ->
            Html.code [] [ text tag ]


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
