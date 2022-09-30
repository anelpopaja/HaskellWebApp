module Main exposing (..)

import Browser
import Browser.Events
import Element as E
import Element.Background as EBG
import Element.Border as EB
import Element.Font as EF
import Element.Input as EI
import Html
import Http exposing (Error(..), emptyBody, header, jsonBody, stringBody)
import Json.Decode as JD
import Json.Encode as JE
import Svg as S
import Svg.Attributes as SA

type alias Model =
    { cardTitle : String
    , cardImage : String
    , cardStrength : Int -- Int  --Maybe Int maybe.... PROMIJENITI NA MAYBE INT
    , cardLink : String
    , cardClass : String
    , cardDeckId : Int --PROMIJENITI NA MAYBE INT
    , cardId : Int --PROMIJENITI NA MAYBE INT
    , poruka : String
    , results : List Card
    , resultCard : Maybe Card --MOZDA I NE MORA MAYBE JER SIGURNO DOBIJAMO BOOK KAO REZULTAT AKO NEMA GRESKE, ALI INIT MODEL PRAVI PROBLEM BEZ TOGA....
    , errorMessage : Maybe String
    , loading : Bool
    }


type alias Card =
    { title : String
    , image : Maybe String
    , link : String
    , strength : Maybe Int
    , class : Maybe String
    }


type Msg
    = MsgGetCards
    | MsgGetCardsMonsters
    | MsgGetCardsNeutral
    | MsgGetCardsNilfgaard
    | MsgGetCardsNorthernRealms
    | MsgGetCardsScoiatael
    | MsgGetCardsLargest
    | MsgGotResults (Result Http.Error (List Card))
    | MsgSuccesfulPost (Result Http.Error ()) --(Result Http.Error String) --TODO DODAJ S
      --| GotText (Result Http.Error String)
    | MsgInputTitleField String
    | MsgInputImageField String
    | MsgInputStrengthFieldAsString String
    | MsgInputLinkField String
    | MsgInputClassField String
    | MsgInputDeckIdFieldAsString String
    | MsgAddCard
    | MsgDeleteCard
    | MsgShowCard
    | MsgInputIdFieldAsString String
    | MsgGotResult (Result Http.Error (Maybe Card))
    | MsgSuccessfulDelete (Result Http.Error ())



--    | MsgKeyPressed String


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, cmdSearchAll )


initModel : Model
initModel =
    { cardTitle = ""
    , cardImage = ""
    , cardStrength = 0 --Maybe Int maybe.... mora 0 difoltno
    , cardLink = ""
    , cardClass = ""
    , cardDeckId = 1 --Mora 1 difoltno
    , cardId = 1
    , poruka = ""
    , results = []
    , resultCard = Nothing
    , errorMessage = Nothing
    , loading = False
    }


view : Model -> Html.Html Msg
view model =
    viewLayout model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MsgInputTitleField newTextInput ->
            ( { model | cardTitle = newTextInput }, Cmd.none )

        MsgInputImageField newImage ->
            ( { model | cardImage = newImage }, Cmd.none )

        MsgInputStrengthFieldAsString newStrength ->
            ( { model | cardStrength = Maybe.withDefault 0 (String.toInt newStrength) }, Cmd.none )

        --JOS JE BOLJE DA SE RAZLOZI NA SLUCAJEVE I POSEBNO OBRADE UMJESTO Maybe.withDefault
        MsgInputLinkField newLink ->
            ( { model | cardLink = newLink }, Cmd.none )

        MsgInputClassField newClass ->
            ( { model | cardClass = newClass }, Cmd.none )

        MsgInputDeckIdFieldAsString newDeckId ->
            ( { model | cardDeckId = Maybe.withDefault 0 (String.toInt newDeckId) }, Cmd.none )

        --JOS JE BOLJE DA SE RAZLOZI NA SLUCAJEVE I POSEBNO OBRADE UMJESTO Maybe.withDefault
        MsgAddCard ->
            updateAddCard model

        MsgDeleteCard ->
            updateDeleteCard model

        MsgShowCard ->
            updateShowCard model

        MsgGetCards ->
            updateSve model

        MsgGetCardsMonsters ->
            updateMonsters model

        MsgGetCardsNeutral ->
            updateNeutral model

        MsgGetCardsNilfgaard ->
            updateNilfgaard model

        MsgGetCardsNorthernRealms ->
            updateNorthernRealms model

        MsgGetCardsScoiatael ->
            updateScoiatael model

        MsgGetCardsLargest ->
            updateLargest model

        MsgInputIdFieldAsString newId ->
            ( { model | cardId = Maybe.withDefault 1 (String.toInt newId) }, Cmd.none )

        --JOS JE BOLJE DA SE RAZLOZI NA SLUCAJEVE I POSEBNO OBRADE UMJESTO Maybe.withDefault
        MsgSuccesfulPost result ->
            let
                newModel =
                    { model | loading = False }
            in
            case result of
                Ok data ->
                    ( { newModel | errorMessage = Nothing }, Cmd.none )

                --poruka = data, errorMessage = Nothing }, Cmd.none )
                Err error ->
                    let
                        errorMessage =
                            case error of
                                NetworkError ->
                                    "Network Error"

                                BadUrl _ ->
                                    "Bad URL"

                                Timeout ->
                                    "Timeout"

                                BadStatus _ ->
                                    "Bad status"

                                BadBody reason ->
                                    reason
                    in
                    ( { newModel | errorMessage = Just errorMessage }, Cmd.none )

        MsgGotResults result ->
            let
                newModel =
                    { model | loading = False }
            in
            case result of
                Ok data ->
                    ( { newModel | results = data, errorMessage = Nothing }, Cmd.none )

                Err error ->
                    let
                        errorMessage =
                            case error of
                                NetworkError ->
                                    "Network Error"

                                BadUrl _ ->
                                    "Bad URL"

                                Timeout ->
                                    "Timeout"

                                BadStatus _ ->
                                    "Bad status"

                                BadBody reason ->
                                    reason
                    in
                    ( { newModel | errorMessage = Just errorMessage }, Cmd.none )

        MsgGotResult result ->
            let
                newModel =
                    { model | loading = False }
            in
            case result of
                Ok data ->
                    ( { newModel | resultCard = data, results = [], errorMessage = Nothing }, Cmd.none )

                Err error ->
                    let
                        errorMessage =
                            case error of
                                NetworkError ->
                                    "Network Error"

                                BadUrl _ ->
                                    "Bad URL"

                                Timeout ->
                                    "Timeout"

                                BadStatus _ ->
                                    "Bad status"

                                BadBody reason ->
                                    reason
                    in
                    ( { newModel | errorMessage = Just errorMessage }, Cmd.none )

        MsgSuccessfulDelete result ->
            let
                newModel =
                    { model | loading = False }
            in
            case result of
                Ok data ->
                    ( { newModel | errorMessage = Nothing }, Cmd.none )

                --poruka = data, errorMessage = Nothing }, Cmd.none )
                Err error ->
                    let
                        errorMessage =
                            case error of
                                NetworkError ->
                                    "Network Error"

                                BadUrl _ ->
                                    "Bad URL"

                                Timeout ->
                                    "Timeout"

                                BadStatus _ ->
                                    "Bad status"

                                BadBody reason ->
                                    reason
                    in
                    ( { newModel | errorMessage = Just errorMessage }, Cmd.none )


updateAddCard : Model -> ( Model, Cmd Msg )
updateAddCard model =
    ( { model | loading = True }, postCards model )


updateDeleteCard : Model -> ( Model, Cmd Msg )
updateDeleteCard model =
    ( { model | loading = True }, cmdDeleteCard model )


updateShowCard : Model -> ( Model, Cmd Msg )
updateShowCard model =
    ( { model | loading = True }, cmdShowCard model )


updateSve : Model -> ( Model, Cmd Msg )
updateSve model =
    ( { model | loading = True }, cmdSearchAll )


updateMonsters : Model -> ( Model, Cmd Msg )
updateMonsters model =
    ( { model | loading = True }, cmdSearchMonsters )


updateNeutral : Model -> ( Model, Cmd Msg )
updateNeutral model =
    ( { model | loading = True }, cmdSearchNeutral )


updateNilfgaard : Model -> ( Model, Cmd Msg )
updateNilfgaard model =
    ( { model | loading = True }, cmdSearchNilfgaard )


updateNorthernRealms : Model -> ( Model, Cmd Msg )
updateNorthernRealms model =
    ( { model | loading = True }, cmdSearchNorthernRealms )


updateScoiatael : Model -> ( Model, Cmd Msg )
updateScoiatael model =
    ( { model | loading = True }, cmdSearchScoiatael )


updateLargest : Model -> ( Model, Cmd Msg )
updateLargest model =
    ( { model | loading = True }, cmdSearchLargest )



--subscriptions : model -> Sub Msg
--subscriptions _ =
--    Browser.Events.onKeyPress keyPressed


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--keyPressed : JD.Decoder Msg
--keyPressed =
--    JD.map MsgKeyPressed (JD.field "key" JD.string)


viewLayout : Model -> Html.Html Msg
viewLayout model =
    E.layoutWith
        { options =
            [ E.focusStyle
                { borderColor = Just (E.rgb255 0xC9 0xC1 0x9F)
                , backgroundColor = Nothing
                , shadow = Nothing
                }
            ]
        }
        []
        (E.column [ E.padding 2 ]
            [ viewSearchBar model
            , viewErrorMessage model
            , viewResults model
            , viewResult model
            ]
        )


viewSearchBar : Model -> E.Element Msg
viewSearchBar model =
    E.column []
        [ E.row [ E.spacing 10, E.paddingXY 100 30, EBG.color (E.rgb255 0xC9 0xC1 0x9F) ]
            [ EI.search []
                { onChange = MsgInputTitleField
                , text = model.cardTitle
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 0xEE 0xEE 0xEE) ] (E.text "Title:")
                }
            , EI.search []
                { onChange = MsgInputImageField
                , text = model.cardImage
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 0xEE 0xEE 0xEE) ] (E.text "Image:")
                }
            , EI.search []
                { onChange = MsgInputStrengthFieldAsString
                , text = String.fromInt model.cardStrength --PROMENITI DA MOZE MAYBE INT
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 0xEE 0xEE 0xEE) ] (E.text "Strength:")
                }
            , EI.search []
                { onChange = MsgInputLinkField
                , text = model.cardLink
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 0xEE 0xEE 0xEE) ] (E.text "Link:")
                }
            , EI.search []
                { onChange = MsgInputClassField
                , text = model.cardClass
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 0xEE 0xEE 0xEE) ] (E.text "Type:")
                }
            , EI.search []
                { onChange = MsgInputDeckIdFieldAsString
                , text = String.fromInt model.cardDeckId --PROMENITI DA MOZE MAYBE INT
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 0xEE 0xEE 0xEE) ] (E.text "Deck Id:")
                }
            , viewAddCardButton
            ]
        , E.row [ E.spacing 10, E.centerX, E.paddingXY 500 30, EBG.color (E.rgb255 0xC9 0xC1 0x9F) ]
            [ EI.search []
                { onChange = MsgInputIdFieldAsString
                , text = String.fromInt model.cardId --PROMENITI DA MOZE MAYBE INT
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 0xEE 0xEE 0xEE) ] (E.text "Card Id:")
                }
            , viewDeleteCardButton
            , viewShowCardButton
            ]
        , E.row [ E.spacing 2, E.paddingXY 0 5, E.centerX ]
            [ viewGetCardsButton
            , viewGetCardsMonstersButton
            , viewGetCardsNeutralButton
            , viewGetCardsNilfgaardButton
            , viewGetCardsNorthernRealmsButton
            , viewGetCardsScoiataelButton
            , viewGetCardsLargestButton
            , if model.loading then
                E.html loadingImage

              else
                E.none
            ]
        ]


loadingImage : Html.Html msg
loadingImage =
    S.svg
        [ SA.width "64px"
        , SA.height "64px"
        , SA.viewBox "0 0 48 48"
        ]
        [ S.circle
            [ SA.cx "24"
            , SA.cy "24"
            , SA.stroke "#6699AA"
            , SA.strokeWidth "4"
            , SA.r "8"
            , SA.fill "none"
            ]
            [ S.animate
                [ SA.attributeName "opacity"
                , SA.values "0;.8;0"
                , SA.dur "2s"
                , SA.repeatCount "indefinite"
                ]
                []
            ]
        ]


viewErrorMessage : Model -> E.Element msg
viewErrorMessage model =
    case model.errorMessage of
        Just errorMessage ->
            E.text errorMessage

        Nothing ->
            E.none


viewResults : Model -> E.Element msg
viewResults model =
    E.wrappedRow [ E.spacing 12, E.centerX, E.paddingXY 45 20 ]
        (List.map viewCard model.results)


viewResult : Model -> E.Element msg
viewResult model =
    let
        cardPlaceholder =
            case model.resultCard of
                Just card ->
                    viewCard card

                Nothing ->
                    E.none
    in
    E.wrappedRow [ E.spacing 12, E.centerX ]
        [ cardPlaceholder ]


viewCard : Card -> E.Element msg
viewCard card =
    let
        titleE =
            E.paragraph [ EF.bold, E.paddingXY 0 12 ] [ E.text card.title ]

        imageE =
            case card.image of
                Just image ->
                    viewCardCover image card.title

                Nothing ->
                    E.none

        strengthE =
            case card.strength of
                Just strength ->
                    E.paragraph [ EF.size 14 ]
                        [ E.text ("Strength:" ++ String.fromInt strength) ]

                Nothing ->
                    E.none

        classE =
            case card.class of
                Just class ->
                    E.paragraph [ EF.size 16, EF.bold, E.paddingXY 0 12]
                        [ E.text ("Type:" ++ class) ]

                Nothing ->
                    E.none
    in
    E.newTabLink
        [ E.width (E.px 340)
        , E.height (E.px 300)
        , EBG.color (E.rgb255 0xDA 0xD5 0xBE)
        , EB.rounded 20
        , E.padding 10
        , E.mouseOver
            [ EBG.color (E.rgb255 0x9D 0x90 0x58)
            ]
        , E.focused
            [ EBG.color (E.rgb255 0xF8 0xF8 0xDD)
            ]
        ]
        { url = card.link
        , label =
            E.row [ E.centerX ]
                [ imageE
                , E.column [ E.padding 20 ]
                    [ titleE
                    , classE
                    , strengthE
                    ]
                ]
        }


viewCardCover : String -> String -> E.Element msg
viewCardCover image title =
    E.image []
        { src = image
        , description = title
        }


viewButtonGeneric : String -> Msg -> E.Element Msg
viewButtonGeneric naziv msg =
    EI.button
        [ EBG.color (E.rgb255 0x9D 0x90 0x58)
        , EF.color (E.rgb255 0xEE 0xEE 0xEE)
        , EB.rounded 5
        , E.paddingXY 20 12
        , E.mouseOver
            [ EBG.color (E.rgb255 0x76 0x6C 0x42)
            , EF.color (E.rgb255 0xDD 0xDD 0xDD)
            ]
        , E.focused
            [ EBG.color (E.rgb255 0x76 0x6C 0x42)
            , EF.color (E.rgb255 0xDD 0xDD 0xDD)
            ]
        ]
        { onPress = Just msg
        , label = E.text naziv
        }


viewAddCardButton : E.Element Msg
viewAddCardButton =
    viewButtonGeneric "Add Card" MsgAddCard


viewDeleteCardButton : E.Element Msg
viewDeleteCardButton =
    viewButtonGeneric "Delete Card" MsgDeleteCard


viewShowCardButton : E.Element Msg
viewShowCardButton =
    viewButtonGeneric "Get card by Id" MsgShowCard


viewGetCardsButton : E.Element Msg
viewGetCardsButton =
    viewButtonGeneric "All Cards" MsgGetCards


viewGetCardsMonstersButton : E.Element Msg
viewGetCardsMonstersButton =
    viewButtonGeneric "Monsters" MsgGetCardsMonsters


viewGetCardsNeutralButton : E.Element Msg
viewGetCardsNeutralButton =
    viewButtonGeneric "Neutral" MsgGetCardsNeutral


viewGetCardsNilfgaardButton : E.Element Msg
viewGetCardsNilfgaardButton =
    viewButtonGeneric "Nilfgaard" MsgGetCardsNilfgaard


viewGetCardsNorthernRealmsButton : E.Element Msg
viewGetCardsNorthernRealmsButton =
    viewButtonGeneric "Northern Realms" MsgGetCardsNorthernRealms


viewGetCardsScoiataelButton : E.Element Msg
viewGetCardsScoiataelButton =
    viewButtonGeneric "Scoia'tael" MsgGetCardsScoiatael



viewGetCardsLargestButton : E.Element Msg
viewGetCardsLargestButton =
    viewButtonGeneric "10 Strongest Cards" MsgGetCardsLargest



--NE KORISTIM OVAJ, ZBOG HEDERA NEODGOVARAJUCIH ZA MOJ SERVER


cmdAddCard : Model -> Cmd Msg
cmdAddCard model =
    Http.request
        { method = "POST"
        , headers = [ header "Access-Control-Allow-Origin" "http://localhost:5000", header "Access-Control-Allow-Credentials" "true", header "Content-Type" "application/json" ]
        , url = "http://localhost:5000/cards"
        , body = jsonBody (encode model)
        , expect = Http.expectWhatever MsgSuccesfulPost
        , timeout = Nothing
        , tracker = Nothing
        }


postCards : Model -> Cmd Msg
postCards model =
    Http.post
        { url = "http://localhost:5000/cards"
        , body = jsonBody (encode model)
        , expect = Http.expectWhatever MsgSuccesfulPost
        }


cmdDeleteCard : Model -> Cmd Msg
cmdDeleteCard model =
    Http.post
        { url = "http://localhost:5000/cards/" ++ String.fromInt model.cardId
        , body = emptyBody
        , expect = Http.expectWhatever MsgSuccessfulDelete
        }


cmdShowCard : Model -> Cmd Msg
cmdShowCard model =
    Http.get
        { url = "http://localhost:5000/cards/" ++ String.fromInt model.cardId
        , expect = Http.expectJson MsgGotResult (JD.maybe decodeItem)
        }


cmdSearchAll : Cmd Msg
cmdSearchAll =
    Http.get
        { url = "http://localhost:5000/cards/sve"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


cmdSearchMonsters : Cmd Msg
cmdSearchMonsters =
    Http.get
        { url = "http://localhost:5000/cards/monsters"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


cmdSearchNeutral : Cmd Msg
cmdSearchNeutral =
    Http.get
        { url = "http://localhost:5000/cards/neutral"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


cmdSearchNilfgaard : Cmd Msg
cmdSearchNilfgaard =
    Http.get
        { url = "http://localhost:5000/cards/nilfgaard"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


cmdSearchNorthernRealms : Cmd Msg
cmdSearchNorthernRealms =
    Http.get
        { url = "http://localhost:5000/cards/northernRealms"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


cmdSearchScoiatael : Cmd Msg
cmdSearchScoiatael =
    Http.get
        { url = "http://localhost:5000/cards/scoiatael"
        , expect = Http.expectJson MsgGotResults decodeItems
        }




cmdSearchLargest : Cmd Msg
cmdSearchLargest =
    Http.get
        { url = "http://localhost:5000/cards/join"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


encode : Model -> JE.Value
encode model =
    JE.object
        [ ( "title", JE.string model.cardTitle )
        , ( "image", JE.string model.cardImage )
        , ( "strength", JE.int model.cardStrength )
        , ( "link", JE.string model.cardLink )
        , ( "type", JE.string model.cardClass )
        , ( "deckId", JE.int model.cardDeckId )
        ]


decodeItems : JD.Decoder (List Card)
decodeItems =
    JD.list decodeItem


decodeItem : JD.Decoder Card
decodeItem =
    JD.map5 Card
        (JD.field "title" JD.string)
        (JD.maybe (JD.field "image" JD.string))
        (JD.field "link" JD.string)
        (JD.maybe (JD.field "strength" JD.int))
        (JD.maybe (JD.field "type" JD.string))
