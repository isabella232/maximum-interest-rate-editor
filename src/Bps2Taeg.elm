module Bps2Taeg exposing (main)

import Browser
import Date exposing (Date)
import Days
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Input.Float as CurrencyInput
import Interest
import Quarter exposing (Quarter(..))
import Round
import Task
import Time exposing (Month(..))
import Utils exposing (euros)


type alias Model =
    { bps : Maybe Int
    , installmentsCount : Maybe Int
    , minPaymentPlan : List Days.Installment
    , maxPaymentPlan : List Days.Installment
    }


type Msg
    = UserChangedInstallmentsCount String
    | UserChangedBps String


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
    let
        initialModel =
            { installmentsCount = Just 10
            , bps = Just 609
            , minPaymentPlan = []
            , maxPaymentPlan = []
            }

        ( minPaymentPlan, maxPaymentPlan ) =
            getMinMaxPaymentPlan initialModel
    in
    ( { initialModel | minPaymentPlan = minPaymentPlan, maxPaymentPlan = maxPaymentPlan }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                UserChangedInstallmentsCount value ->
                    { model | installmentsCount = String.toInt value }

                UserChangedBps value ->
                    { model | bps = String.toInt value }

        ( minPaymentPlan, maxPaymentPlan ) =
            getMinMaxPaymentPlan newModel
    in
    ( { newModel | minPaymentPlan = minPaymentPlan, maxPaymentPlan = maxPaymentPlan }, Cmd.none )


getMinMaxPaymentPlan : Model -> ( List Days.Installment, List Days.Installment )
getMinMaxPaymentPlan model =
    case
        ( model.installmentsCount, model.bps )
    of
        ( Just installmentsCount, Just bps ) ->
            let
                paymentPlanBuilder =
                    \startDate ->
                        Interest.getCreditPaymentPlan
                            installmentsCount
                            startDate
                            10000
                            bps

                candidates =
                    [ Q1, Q2, Q3, Q4 ]
                        |> List.map Quarter.initialDates
                        |> List.concat
                        |> List.map paymentPlanBuilder

                candidatesTaeg =
                    candidates
                        |> List.map (Interest.optimal_interest_rate 10000)

                minPaymentPlan =
                    List.map2 Tuple.pair candidates candidatesTaeg
                        |> List.foldl
                            (\( paymentPlan, maybeInterest ) acc ->
                                case ( acc, maybeInterest ) of
                                    ( Just ( previousPlan, Just previousInterest ), Just interest ) ->
                                        if previousInterest < interest then
                                            Just ( previousPlan, Just previousInterest )

                                        else
                                            Just ( paymentPlan, Just interest )

                                    _ ->
                                        Just ( paymentPlan, maybeInterest )
                            )
                            Nothing

                maxPaymentPlan =
                    List.map2 Tuple.pair candidates candidatesTaeg
                        |> List.foldl
                            (\( paymentPlan, maybeInterest ) acc ->
                                case ( acc, maybeInterest ) of
                                    ( Just ( previousPlan, Just previousInterest ), Just interest ) ->
                                        if previousInterest > interest then
                                            Just ( previousPlan, Just previousInterest )

                                        else
                                            Just ( paymentPlan, Just interest )

                                    _ ->
                                        Just ( paymentPlan, maybeInterest )
                            )
                            Nothing
            in
            case ( minPaymentPlan, maxPaymentPlan ) of
                ( Just ( minPlan, _ ), Just ( maxPlan, _ ) ) ->
                    ( minPlan, maxPlan )

                _ ->
                    ( [], [] )

        _ ->
            ( [], [] )


viewInstallment : Int -> Days.Installment -> Html Msg
viewInstallment i installment =
    tr [ class "" ]
        [ td []
            [ text <| "E" ++ String.fromInt (i + 1) ]
        , td []
            [ text installment.dueDate ]
        , td []
            [ text <| euros installment.totalAmount ]
        , td []
            [ text <| euros installment.purchaseAmount ]
        , td []
            [ text <| euros installment.customerInterest ]
        ]


viewPaymentPlan : List Days.Installment -> Html Msg
viewPaymentPlan paymentPlan =
    case paymentPlan of
        [] ->
            text ""

        _ ->
            let
                feeTitle =
                    if List.length paymentPlan > 4 then
                        "Intérêts"

                    else
                        "Frais client"
            in
            table [ class "table table-condensed" ]
                [ thead []
                    [ tr []
                        [ th []
                            [ text "#" ]
                        , th []
                            [ text "Date" ]
                        , th []
                            [ text "Montant" ]
                        , th []
                            [ text "Capital" ]
                        , th []
                            [ text feeTitle ]
                        ]
                    ]
                , List.indexedMap viewInstallment paymentPlan |> tbody []
                ]


annual_interest_rate : List Days.Installment -> String
annual_interest_rate paymentPlan =
    let
        maybe_taeg =
            Interest.optimal_interest_rate 10000 paymentPlan
    in
    case maybe_taeg of
        Just taeg ->
            Round.round 2 (taeg * 100)

        Nothing ->
            "-,--"


view : Model -> Html Msg
view { bps, installmentsCount, minPaymentPlan, maxPaymentPlan } =
    div [ class "row" ]
        [ div [ class "col-sm-12" ]
            [ div [ class "form-group col-sm-6" ]
                [ label [ for "bps", class "col-sm-6 control-label" ] [ text "Frais clients bps" ]
                , div [ class "col-sm-6" ]
                    [ div [ class "input-group" ]
                        [ input
                            [ type_ "text"
                            , class "form-control"
                            , id "bps"
                            , value <| String.fromInt <| Maybe.withDefault 0 bps
                            , onInput <| UserChangedBps
                            ]
                            []
                        , span [ class "input-group-addon" ] [ text "bps" ]
                        ]
                    ]
                ]
            , div [ class "form-group col-sm-6" ]
                [ label [ for "installments_count", class "col-sm-6 control-label" ] [ text "Nombre d'échéances" ]
                , div [ class "col-sm-6" ]
                    [ div [ class "input-group" ]
                        [ input
                            [ type_ "text"
                            , class "form-control"
                            , id "installments_count"
                            , value <| String.fromInt <| Maybe.withDefault 0 installmentsCount
                            , onInput <| UserChangedInstallmentsCount
                            ]
                            []
                        , span [ class "input-group-addon" ] [ text "fois" ]
                        ]
                    ]
                ]
            ]
        , div [ class "col-sm-6" ]
            [ p []
                [ h1 [ class "text-center" ]
                    [ text "Votre TAEG pour ce paiement est de "
                    , text <| annual_interest_rate minPaymentPlan
                    , text "%"
                    ]
                ]
            ]
        , div [ class "col-sm-6" ]
            [ viewPaymentPlan minPaymentPlan
            ]
        , div [ class "col-sm-6" ]
            [ p []
                [ h1 [ class "text-center" ]
                    [ text "Votre TAEG pour ce paiement est de "
                    , text <| annual_interest_rate maxPaymentPlan
                    , text "%"
                    ]
                ]
            ]
        , div [ class "col-sm-6" ]
            [ viewPaymentPlan maxPaymentPlan
            ]
        ]
