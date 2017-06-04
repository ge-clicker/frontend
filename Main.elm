module Main exposing (..)

import AnimationFrame exposing (..)
import Debug
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Html exposing (Html, body, div, h1, img, option, program, select, text)
import Html.Attributes exposing (align, src, style, value)
import Html.Events exposing (on, onClick, onMouseDown, targetValue)
import Http
import Json.Decode.Pipeline exposing (decode, hardcoded, optional, required)
import List
import List.Extra exposing (find, findIndex, updateAt, updateIf)
import Task
import Time exposing (..)


type alias Party =
    { name : String
    , image : String
    , primaryColour : String
    , secondaryColour : String
    , id : Int
    , clicks : Clicks
    }


type alias Parties =
    List Party


type alias Clicks =
    { total : Int
    , day : Int
    , hour : Int
    , mins : Int
    , rate : Float
    }


type alias ReceivedClicks =
    { clicks : Clicks
    , id : Int
    }


type alias AllClicks =
    List ReceivedClicks


type alias MyClick =
    { count : Int
    , id : Int
    }


type alias MyClicks =
    List MyClick


type alias Model =
    { parties : Parties
    , myClicks : MyClicks
    , lastReceiveTime : Time
    , getter : Clicks -> Int
    }


type SelectType
    = TenMins
    | Hour
    | Day
    | Total


type Msg
    = InitialRequest (Result Http.Error Parties)
    | ReceiveClicks (Result Http.Error AllClicks)
    | ReceiveTime Time
    | SendClicks
    | Tick Time
    | ClickEvent Int
    | Selected SelectType


api : String -> String
api path =
    "https://ge-clicker-backend.herokuapp.com/api/" ++ path



-----------------
-- Init and Subs
-----------------


init : ( Model, Cmd Msg )
init =
    ( { parties = []
      , myClicks = []
      , lastReceiveTime = 0
      , getter = .mins
      }
    , Http.send InitialRequest <| Http.get (api "party") initialDecoder
    )


initialDecoder : Decoder Parties
initialDecoder =
    Decode.list
        (decode Party
            |> required "name" Decode.string
            |> required "image" Decode.string
            |> optional "primary_color" Decode.string "white"
            |> optional "secondary_color" Decode.string "white"
            |> required "id" Decode.int
            |> hardcoded (Clicks 0 0 0 0 0)
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ every second (\_ -> SendClicks)
          -- , times (\_ -> Tick)
        ]



----------
-- Update
----------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        debug =
            Debug.log (toString msg) ( model, Cmd.none )
    in
        case msg of
            InitialRequest (Ok parties) ->
                ( { model | parties = parties }, getClicks )

            InitialRequest (Err _) ->
                debug

            SendClicks ->
                ( model
                , case model.myClicks of
                    [] ->
                        getClicks

                    _ ->
                        sendClicks model.myClicks
                )

            ClickEvent id ->
                ( { model
                    | myClicks =
                        case findIndex (\click -> click.id == id) model.myClicks of
                            Just i ->
                                case
                                    updateAt i
                                        (\click ->
                                            { click | count = click.count + 1 }
                                        )
                                        model.myClicks
                                of
                                    Just list ->
                                        list

                                    Nothing ->
                                        model.myClicks

                            Nothing ->
                                { count = 1, id = id } :: model.myClicks
                  }
                , Cmd.none
                )

            ReceiveClicks (Ok receiveClicks) ->
                ( { model
                    | parties =
                        List.map
                            (\party ->
                                case find (\r -> r.id == party.id) receiveClicks of
                                    Just e ->
                                        { party | clicks = e.clicks }

                                    Nothing ->
                                        party
                            )
                            model.parties
                    , myClicks = []
                  }
                , Task.perform ReceiveTime Time.now
                )

            ReceiveClicks _ ->
                debug

            ReceiveTime time ->
                ( { model | lastReceiveTime = time }
                , Cmd.none
                )

            Selected s ->
                ( Debug.log "selected model"
                    { model
                        | getter =
                            case s of
                                TenMins ->
                                    .mins

                                Hour ->
                                    .hour

                                Day ->
                                    .day

                                Total ->
                                    .total
                    }
                , Cmd.none
                )

            Tick time ->
                -- TODO
                debug


sendClicks : MyClicks -> Cmd Msg
sendClicks myClicks =
    Http.send ReceiveClicks <| Http.post (api "click") (Http.jsonBody <| clicksValue myClicks) allClicksDecoder


getClicks : Cmd Msg
getClicks =
    Http.send ReceiveClicks <| Http.get (api "click") allClicksDecoder


allClicksDecoder : Decoder AllClicks
allClicksDecoder =
    Decode.list
        (decode (\a b c d e f -> { clicks = Clicks a b c d e, id = f })
            |> required "all_time" Decode.int
            |> required "one_day" Decode.int
            |> required "one_hour" Decode.int
            |> required "ten_minutes" Decode.int
            |> required "rate" Decode.float
            |> required "id" Decode.int
        )


clicksValue : MyClicks -> Encode.Value
clicksValue =
    Encode.list
        << List.map
            (\click ->
                Encode.object
                    [ ( "clicks", Encode.int click.count )
                    , ( "id", Encode.int click.id )
                    ]
            )



--------
-- View
--------


view : Model -> Html Msg
view model =
    let
        countClicks =
            Debug.log "list" <|
                List.map (\p -> model.getter p.clicks) <|
                    Debug.log "parties" model.parties

        minMax =
            case ( List.minimum countClicks, List.maximum countClicks ) of
                ( Just mn, Just mx ) ->
                    Debug.log "minMax" ( mn, mx )

                _ ->
                    ( 0, 1 )
    in
        body
            [ style
                [ ( "height", "100%" ) ]
            ]
            [ selectView
            , div
                [ style
                    [ ( "display", "flex" )
                    , ( "flex-direction", "row" )
                    , ( "width", "100%" )
                    , ( "height", "90%" )
                    ]
                ]
                (List.map
                    (\party ->
                        case find (\click -> click.id == party.id) model.myClicks of
                            Just click ->
                                viewParty party click.count minMax model.getter

                            Nothing ->
                                viewParty party 0 minMax model.getter
                    )
                    model.parties
                )
            ]


targetValueRoleDecoder : Decode.Decoder SelectType
targetValueRoleDecoder =
    targetValue
        |> Decode.andThen
            (\val ->
                case val of
                    "ten_minutes" ->
                        Decode.succeed TenMins

                    "one_hour" ->
                        Decode.succeed Hour

                    "one_day" ->
                        Decode.succeed Day

                    "all_time" ->
                        Decode.succeed Total

                    _ ->
                        Decode.fail ("Invalid Role: " ++ val)
            )


selectView : Html Msg
selectView =
    select [ on "change" (Decode.map Selected targetValueRoleDecoder) ]
        [ option [ value "ten_minutes" ]
            [ text "5 Seconds" ]
        , option [ value "one_hour" ]
            [ text "10 Seconds" ]
        , option [ value "one_day" ]
            [ text "1 Minute" ]
        , option [ value "all_time" ]
            [ text "All time" ]
        ]


viewParty : Party -> Int -> ( Int, Int ) -> (Clicks -> Int) -> Html Msg
viewParty party myClicks ( mn, mx ) getter =
    let
        block flex colour =
            div
                [ style
                    [ ( "backgroundColor", colour )
                    , ( "flex", toString flex )
                    ]
                ]
                []

        emptyFlex =
            block 2 "none"

        numClicks =
            getter party.clicks
                |> (+) myClicks

        pos =
            (toFloat <| numClicks - mn) / (toFloat <| mx - mn)
    in
        div
            [ onClick (ClickEvent party.id)
            , style
                [ ( "display", "flex" )
                , ( "flex-direction", "column" )
                , ( "width", "10em" )
                , ( "flex", "1" )
                , ( "position", "relative" )
                ]
            ]
            [ block (1 - pos) "none"
            , div
                [ style
                    [ ( "flex", toString pos )
                    , ( "display", "flex" )
                    , ( "flex-direction", "row" )
                    ]
                ]
                [ emptyFlex
                , block 0.2 party.secondaryColour
                , block 0.3 party.primaryColour
                , block 0.2 party.secondaryColour
                , emptyFlex
                , div
                    [ style
                        [ ( "position", "absolute" )
                        , ( "width", "100%" )
                        , ( "padding-top", "100%" )
                        , ( "margin-top", "-125%" )
                        , ( "margin-left", "12.5%" )
                        ]
                    ]
                    [ img
                        [ src party.image
                        , style
                            [ ( "width", "75%" )
                            , ( "top", "0" )
                            , ( "left", "0" )
                            , ( "border-radius", "50%" )
                            , ( "border", "5px solid " ++ party.primaryColour )
                            ]
                        ]
                        []
                    ]
                ]
            , h1
                [ style
                    [ ( "text-align", "center" )
                    , ( "width", "100%" )
                    ]
                ]
                [ numClicks
                    |> toString
                    |> text
                ]
            ]



--------
-- Main
--------


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
