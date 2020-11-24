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
    { publicationName : String
    , below3000 : FieldInfo
    , over3000 : FieldInfo
    , over6000 : FieldInfo
    }


type FieldType
    = Below3000
    | Over3000
    | Over6000


type Quarter
    = Q1
    | Q2
    | Q3
    | Q4


type alias FieldInfo =
    Maybe Float


type Msg
    = InputChanged FieldType (Maybe Float)
    | ReceiveDate Date
    | SetPublicationName String


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
    ( { publicationName = ""
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
            ( { model | publicationName = publicationNameFromDate today }, Cmd.none )

        SetPublicationName value ->
            ( { model | publicationName = value }, Cmd.none )

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
        [ MaskedPercentage.input
            (inputOptions field)
            [ class "form-control", id formName ]
            fieldInfo
        , span [ class "input-group-addon" ] [ text "%" ]
        ]
    , case fieldInfo of
        Just rate ->
            input
                [ type_ "hidden"
                , formName ++ "_rate" |> name
                , rate * 100 |> Round.floor 0 |> value
                ]
                []

        Nothing ->
            text ""
    ]


days : Quarter -> List Int
days quarter =
    case quarter of
        Q1 ->
            [ 28, 31, 30, 31, 30, 31, 31, 30, 31 ]

        Q2 ->
            [ 30, 31, 30, 31, 31, 30, 31, 30, 31 ]

        Q3 ->
            [ 30, 31, 30, 31, 31, 28, 31, 30, 31 ]

        Q4 ->
            [ 30, 31, 31, 28, 31, 30, 31, 30, 31 ]


plan_builder : Int -> ( Int, List Int ) -> ( Int, List Int )
plan_builder next ( previous, result ) =
    let
        val =
            next + previous
    in
    ( val, val :: result )


buildPlanDays : List Int -> List Int
buildPlanDays plan_days =
    List.foldl plan_builder ( 0, [] ) plan_days
        |> Tuple.second
        |> List.reverse


quarterFromName : String -> Maybe Quarter
quarterFromName name =
    let
        suffix =
            String.right 2 name
    in
    case suffix of
        "T1" ->
            Just Q1

        "T2" ->
            Just Q2

        "T3" ->
            Just Q3

        "T4" ->
            Just Q4

        _ ->
            Nothing


showInterest : Int -> Maybe Float -> Maybe Quarter -> String
showInterest n maybe_rate maybe_quarter =
    case ( maybe_rate, maybe_quarter ) of
        ( Just rate, Just quarter ) ->
            -- n = len(plans) + 1
            -- 1 / n * (n - 1 - sum([1 / (1 + r) ** (t / 365) for t in plans]))
            let
                sum =
                    days quarter
                        |> List.take (n - 1)
                        |> buildPlanDays
                        |> List.map (\t -> 1 / (1 + rate / 100) ^ (toFloat t / 365))
                        |> List.sum

                rounded_value =
                    (1 / toFloat n * (toFloat n - 1 - sum) * 10000)
                        |> Round.floor 0
            in
            rounded_value ++ " bps"

        ( _, _ ) ->
            "-,-- %"


showTableFor : Int -> Model -> Html Msg
showTableFor x model =
    let
        months =
            String.fromInt x

        title =
            "Paiement en " ++ months ++ " fois"
    in
    div [ class "col-sm-12" ]
        [ p [ class "col-sm-offset-1 col-sm-2" ] [ strong [] [ text title ] ]
        , p [ class "col-sm-1 text-right" ] [ text <| showInterest x model.below3000 <| quarterFromName model.publicationName ]
        , p [ class "col-sm-offset-1 col-sm-2" ] [ strong [] [ text title ] ]
        , p [ class "col-sm-1 text-right" ] [ text <| showInterest x model.over3000 <| quarterFromName model.publicationName ]
        , p [ class "col-sm-offset-1 col-sm-2" ] [ strong [] [ text title ] ]
        , p [ class "col-sm-1 text-right" ] [ text <| showInterest x model.over6000 <| quarterFromName model.publicationName ]
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
