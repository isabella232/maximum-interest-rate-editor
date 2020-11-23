module MaximumInterestRateEditor exposing (main)

import Browser
import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Input.Float as MaskedPercentage
import Round
import Task
import Time exposing (Month(..))


type alias Model =
    { publicationDate : String
    , below3000 : FieldInfo
    , over3000 : FieldInfo
    , over6000 : FieldInfo
    }


type FieldType
    = Below3000
    | Over3000
    | Over6000


type alias FieldInfo =
    Maybe Float


type Msg
    = InputChanged FieldType (Maybe Float)
    | ReceiveDate Date
    | SetPublicationDate String


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { publicationDate = ""
      , below3000 = Just 20.83
      , over3000 = Just 10.16
      , over6000 = Just 5.19
      }
    , Date.today |> Task.perform ReceiveDate
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveDate today ->
            ( { model | publicationDate = publicationDateFromDate today }, Cmd.none )

        SetPublicationDate value ->
            ( { model | publicationDate = value }, Cmd.none )

        InputChanged Below3000 value ->
            ( { model | below3000 = value }, Cmd.none )

        InputChanged Over3000 value ->
            ( { model | over3000 = value }, Cmd.none )

        InputChanged Over6000 value ->
            ( { model | over6000 = value }, Cmd.none )


inputOptions : FieldType -> MaskedPercentage.Options Msg
inputOptions field =
    let
        defaultOptions =
            MaskedPercentage.defaultOptions (InputChanged field)
    in
    { defaultOptions
        | maxValue = Just 100
        , minValue = Just 0
    }


publicationDateFromDate : Date -> String
publicationDateFromDate value =
    let
        year =
            Date.year value |> String.fromInt

        quarter =
            Date.quarter value |> String.fromInt
    in
    year ++ "T" ++ quarter


percentageInput : FieldType -> FieldInfo -> Html Msg
percentageInput field fieldInfo =
    div [ class "input-group" ]
        [ MaskedPercentage.input
            (inputOptions field)
            [ class "form-control" ]
            fieldInfo
        , span [ class "input-group-addon" ] [ text "%" ]
        ]


showInterest : Int -> Maybe Float -> String
showInterest n maybe_rate =
    case maybe_rate of
        Nothing ->
            "-,-- %"

        Just rate ->
            let
                base_rate =
                    (((1 + rate / 100) ^ (1 / 12) - 1) / (1 - (1 + rate / 100) ^ -1) * 12) - 1

                rounded_value =
                    (base_rate * (toFloat n - 1) / 12 * 100)
                        |> Round.round 2
                        |> String.replace "." ","
            in
            rounded_value ++ " %"


showTableFor : Int -> Model -> Html Msg
showTableFor x model =
    let
        months =
            String.fromInt x

        title =
            "P" ++ months ++ "X"
    in
    div [ class "col-sm-offset-2 col-sm-10" ]
        [ p [ class "col-sm-1 text-center" ] [ strong [] [ text title ] ]
        , p [ class "col-sm-1 text-center" ] [ text <| showInterest x model.below3000 ]
        , p [ class "col-sm-offset-3 col-sm-1 text-center" ] [ strong [] [ text title ] ]
        , p [ class "col-sm-1 text-center" ] [ text <| showInterest x model.over3000 ]
        , p [ class "col-sm-offset-3 col-sm-1 text-center" ] [ strong [] [ text title ] ]
        , p [ class "col-sm-1 text-center" ] [ text <| showInterest x model.over6000 ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ div [ class "row" ]
            [ div [ class "col-xs-6" ]
                [ div [ class "panel panel-default" ]
                    [ div [ class "panel-heading" ]
                        [ h3 [ class "panel-title" ] [ text "Configuration du taux d'usure" ] ]
                    , div [ class "panel-body" ]
                        [ Html.form [ class "form-horizontal" ]
                            [ div [ class "form-group" ]
                                [ label [ for "parution", class "col-sm-2 control-label" ] [ text "Parution" ]
                                , div [ class "col-sm-10" ]
                                    [ input [ type_ "text", class "form-control", id "parution", value model.publicationDate, onInput SetPublicationDate ] []
                                    ]
                                ]
                            , div [ class "form-group" ]
                                [ label [ for "below_3000", class "col-sm-2 control-label" ] [ text "Jusqu'à 3000€" ]
                                , div [ class "col-sm-2" ]
                                    [ percentageInput Below3000 model.below3000 ]
                                , label [ for "below_3000", class "col-sm-2 control-label" ] [ text "De 3000€ à 6000€" ]
                                , div [ class "col-sm-2" ]
                                    [ percentageInput Over3000 model.over3000 ]
                                , label [ for "below_3000", class "col-sm-2 control-label" ] [ text "Au-delà 6000€" ]
                                , div [ class "col-sm-2" ]
                                    [ percentageInput Over6000 model.over6000 ]
                                ]
                            , div []
                                [ showTableFor 2 model
                                , showTableFor 3 model
                                , showTableFor 4 model
                                , showTableFor 10 model
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
