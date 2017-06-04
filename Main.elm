module Main exposing (..)

import AnimationFrame exposing (..)
import Debug
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Html exposing (Html, div, img, program, text)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode.Pipeline exposing (decode, hardcoded, required)
import List
import List.Extra exposing (updateIf)
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


type alias Model =
    { parties : Parties
    }


type alias Clicks =
    { total : Int
    , day : Int
    , hour : Int
    , mins : Int
    }


type alias WithRateClicks =
    { total : Int
    , day : Int
    , hour : Int
    , mins : Int
    , rate : Float
    , id : Int
    }


type alias AllClicks =
    List WithRateClicks


type Msg
    = InitialRequest (Result Http.Error Parties)
    | ReceiveClicks (Result Http.Error AllClicks)
    | SendClicks
    | Tick
    | ClickEvent Party


api : String -> String
api path =
    "https://ge-clicker-backend.herokuapp.com/api/" ++ path



-----------------
-- Init and Subs
-----------------


init : ( Model, Cmd Msg )
init =
    ( { parties = [ labour, labour, labour ] }
    , Http.send InitialRequest <| Http.get (api "party") initialDecoder
    )


initialDecoder : Decoder Parties
initialDecoder =
    Decode.list
        (decode Party
            |> required "name" Decode.string
            |> required "image" Decode.string
            |> required "primary_color" Decode.string
            |> required "secondary_color" Decode.string
            |> required "id" Decode.int
            |> hardcoded (Clicks 0 0 0 0)
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

            _ ->
                debug


sendClicks : List ( Party, Int ) -> Cmd Msg
sendClicks partyClicks =
    Http.send ReceiveClicks <| Http.post (api "click") (Http.jsonBody <| clicksValue partyClicks) allClicksDecoder


getClicks : Cmd Msg
getClicks =
    Http.send ReceiveClicks <| Http.get (api "click") allClicksDecoder


allClicksDecoder : Decoder AllClicks
allClicksDecoder =
    Decode.list
        (decode WithRateClicks
            |> required "all_time" Decode.int
            |> required "one_day" Decode.int
            |> required "one_hour" Decode.int
            |> required "ten_minutes" Decode.int
            |> required "rate" Decode.float
            |> required "id" Decode.int
        )


clicksValue : List ( Party, Int ) -> Encode.Value
clicksValue =
    Encode.list
        << List.map
            (\( party, clicks ) ->
                Encode.object
                    [ ( "clicks", Encode.int clicks )
                    , ( "id", Encode.int party.id )
                    ]
            )



--------
-- View
--------


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "display", "flex" )
            , ( "flex-direction", "row" )
            , ( "width", "100%" )
            , ( "height", "80%" )
            ]
        ]
        (List.map viewParty model.parties)


viewParty : Party -> Html Msg
viewParty party =
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
    in
        div
            [ onClick (ClickEvent party)
            , style
                [ ( "display", "flex" )
                , ( "flex-direction", "column" )
                , ( "width", "10em" )
                , ( "flex", "1" )
                , ( "position", "relative" )
                ]
            ]
            [ emptyFlex
            , div
                [ style
                    [ ( "flex", "1" )
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
                        , ( "margin-left", "25%" )
                        ]
                    ]
                    [ img
                        [ src party.image
                        , style
                            [ ( "width", "50%" )
                            , ( "top", "0" )
                            , ( "left", "0" )
                            , ( "border-radius", "50%" )
                            , ( "border", "5px solid " ++ party.primaryColour )
                            ]
                        ]
                        []
                    ]
                ]
            ]


labour : Party
labour =
    { name = "Labour"
    , image = "labour.jpg"
    , primaryColour = "#d50000"
    , secondaryColour = "#9B301C"
    , id = 0
    , clicks = Clicks 0 0 0 0
    }



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
