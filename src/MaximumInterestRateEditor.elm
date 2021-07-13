module MaximumInterestRateEditor exposing (main)

import Browser
import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Input.Float as MaskedPercentage
import Interest
import Round
import Task
import Time exposing (Month(..))


type alias Flags =
    { below_3000 : Int
    , over_3000 : Int
    , over_6000 : Int
    }


type alias Model =
    { publicationName : String
    , below3000 : FieldInfo
    , over3000 : FieldInfo
    , over6000 : FieldInfo
    }


type FieldType
    = Below3000
    | Over3000
    | Over6000


type alias FieldInfo =
    Maybe Int


type Msg
    = InputChanged FieldType String
    | ReceiveDate Date
    | SetPublicationName String


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { publicationName = ""
      , below3000 = Just flags.below_3000
      , over3000 = Just flags.over_3000
      , over6000 = Just flags.over_6000
      }
    , Date.today |> Task.perform ReceiveDate
    )


validateInputRate : String -> Maybe Int
validateInputRate maybe_rate =
    maybe_rate
        |> String.toInt
        |> Maybe.andThen
            (\rate ->
                if rate > 0 && rate < 10000 then
                    Just rate

                else
                    Nothing
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveDate today ->
            ( { model | publicationName = publicationNameFromDate today }, Cmd.none )

        SetPublicationName value ->
            ( { model | publicationName = value }, Cmd.none )

        InputChanged Below3000 value ->
            ( { model | below3000 = validateInputRate value }, Cmd.none )

        InputChanged Over3000 value ->
            ( { model | over3000 = validateInputRate value }, Cmd.none )

        InputChanged Over6000 value ->
            ( { model | over6000 = validateInputRate value }, Cmd.none )


publicationNameFromDate : Date -> String
publicationNameFromDate value =
    let
        year =
            Date.year value |> String.fromInt

        quarter =
            Date.quarter value |> String.fromInt
    in
    year ++ "T" ++ quarter


percentageInput : FieldType -> FieldInfo -> String -> List (Html Msg)
percentageInput field fieldInfo formName =
    [ div [ class "input-group" ]
        [ input
            [ class "form-control"
            , onInput <| InputChanged field
            , id formName
            , value <| String.fromInt <| Maybe.withDefault 0 fieldInfo
            ]
            []
        , span [ class "input-group-addon" ] [ text "bps" ]
        ]
    , case fieldInfo of
        Just rate ->
            input
                [ type_ "hidden"
                , formName ++ "_rate" |> name
                , rate |> String.fromInt |> value
                ]
                []

        Nothing ->
            text ""
    ]


showTableFor : Int -> Model -> Html Msg
showTableFor x model =
    let
        months =
            String.fromInt x

        title =
            "P" ++ months ++ "X"
    in
    div [ class "col-sm-12" ]
        [ p [ class "col-sm-offset-1 col-sm-1" ] [ strong [] [ text title ] ]
        , p [ class "col-sm-2 text-right" ] [ text <| Interest.show x model.below3000 model.publicationName ]
        , p [ class "col-sm-offset-1 col-sm-1" ] [ strong [] [ text title ] ]
        , p [ class "col-sm-2 text-right" ] [ text <| Interest.show x model.over3000 model.publicationName ]
        , p [ class "col-sm-offset-1 col-sm-1" ] [ strong [] [ text title ] ]
        , p [ class "col-sm-2 text-right" ] [ text <| Interest.show x model.over6000 model.publicationName ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ div [ class "form-group" ]
            [ label [ for "publication_name", class "col-sm-2 control-label" ] [ text "Parution" ]
            , div [ class "col-sm-10" ]
                [ input
                    [ type_ "text"
                    , class "form-control"
                    , id "publication_name"
                    , name "publication_name"
                    , value model.publicationName
                    , onInput SetPublicationName
                    ]
                    []
                ]
            ]
        , div [ class "form-group" ]
            [ label [ for "below_3000", class "col-sm-2 control-label" ] [ text "Jusqu'à 3000€" ]
            , percentageInput Below3000 model.below3000 "below_3000" |> div [ class "col-sm-2" ]
            , label [ for "over_3000", class "col-sm-2 control-label" ] [ text "De 3000€ à 6000€" ]
            , percentageInput Over3000 model.over3000 "over_3000" |> div [ class "col-sm-2" ]
            , label [ for "over_6000", class "col-sm-2 control-label" ] [ text "Au-delà 6000€" ]
            , percentageInput Over6000 model.over6000 "over_6000" |> div [ class "col-sm-2" ]
            ]
        , div []
            [ showTableFor 2 model
            , showTableFor 3 model
            , showTableFor 4 model
            , showTableFor 10 model
            ]
        ]
