module Bps2Taeg exposing (main)

import Browser
import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Item as CI
import Days
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Interest
import Quarter exposing (Quarter(..))
import Round
import Svg as S
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
            minMaxPaymentPlan initialModel
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
            minMaxPaymentPlan newModel
    in
    ( { newModel | minPaymentPlan = minPaymentPlan, maxPaymentPlan = maxPaymentPlan }, Cmd.none )


minPaymentPlanFromCandidates : List (List Days.Installment) -> Maybe ( List Days.Installment, Maybe Float )
minPaymentPlanFromCandidates candidates =
    let
        candidatesTaeg =
            candidates
                |> List.map (Interest.optimal_interest_rate 10000)
    in
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


allPaymentPlanCandidates : Int -> Int -> List { x : String, y : Float }
allPaymentPlanCandidates bps installmentsCount =
    let
        candidatesDates =
            [ Q1, Q2, Q3, Q4 ]
                |> List.map Quarter.initialDates
                |> List.concat

        candidatesTaeg =
            candidatesDates
                |> List.map (paymentPlanBuilder bps installmentsCount)
                |> List.map (Interest.optimal_interest_rate 10000)
    in
    List.map2
        (\a b ->
            { x = a
            , y = Maybe.withDefault 0 b
            }
        )
        candidatesDates
        candidatesTaeg


maxPaymentPlanFromCandidates : List (List Days.Installment) -> Maybe ( List Days.Installment, Maybe Float )
maxPaymentPlanFromCandidates candidates =
    let
        candidatesTaeg =
            candidates
                |> List.map (Interest.optimal_interest_rate 10000)
    in
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


paymentPlanBuilder : Int -> Int -> String -> List Days.Installment
paymentPlanBuilder bps installmentsCount startDate =
    Interest.getCreditPaymentPlan
        installmentsCount
        startDate
        10000
        bps


minMaxPaymentPlan : Model -> ( List Days.Installment, List Days.Installment )
minMaxPaymentPlan model =
    case
        ( model.installmentsCount, model.bps )
    of
        ( Just installmentsCount, Just bps ) ->
            let
                candidates =
                    [ Q1, Q2, Q3, Q4 ]
                        |> List.map Quarter.initialDates
                        |> List.concat
                        |> List.map (paymentPlanBuilder bps installmentsCount)

                minPaymentPlan =
                    minPaymentPlanFromCandidates candidates

                maxPaymentPlan =
                    maxPaymentPlanFromCandidates candidates
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
    tr []
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


annualInterestRate : List Days.Installment -> String
annualInterestRate paymentPlan =
    let
        maybe_taeg =
            Interest.optimal_interest_rate 10000 paymentPlan
    in
    case maybe_taeg of
        Just taeg ->
            Round.round 2 (taeg * 100)

        Nothing ->
            "-,--"


bpsForm : Maybe Int -> Html Msg
bpsForm bps =
    div [ class "form-group col-sm-6" ]
        [ label [ for "bps", class "col-sm-6 control-label" ] [ text "Customer fees in bps" ]
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


installmentsCountForm : Maybe Int -> Html Msg
installmentsCountForm installmentsCount =
    div [ class "form-group col-sm-6" ]
        [ label [ for "installments_count", class "col-sm-6 control-label" ] [ text "Installments Count" ]
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


displayMeanInterestRate : Maybe Int -> Maybe Int -> Html Msg
displayMeanInterestRate maybeBps maybeInstallmentsCount =
    case ( maybeBps, maybeInstallmentsCount ) of
        ( Just bps, Just installmentsCount ) ->
            let
                data =
                    allPaymentPlanCandidates bps installmentsCount

                mean =
                    (data |> List.map .y |> List.sum) / (List.length data |> toFloat)
            in
            div [ class "col-sm-12" ]
                [ h4 [ class "alert alert-warning" ] [ text "Mean Interest Rate for this customer fee: ", strong [] [ text <| Round.round 2 (mean * 100), text " %" ] ]
                ]

        _ ->
            text ""


view : Model -> Html Msg
view { bps, installmentsCount, minPaymentPlan, maxPaymentPlan } =
    div []
        [ div [ class "row" ]
            [ div [ class "col-sm-12" ]
                [ bpsForm bps
                , installmentsCountForm installmentsCount
                ]
            , displayMeanInterestRate bps installmentsCount
            , div [ class "col-sm-6" ]
                [ div [ class "panel panel-default" ]
                    [ div [ class "panel-heading" ] [ text "Minimum interest rate example" ]
                    , viewPaymentPlan minPaymentPlan
                    , p []
                        [ h4 [ class "text-center" ]
                            [ text "The interest rate for this payment is  "
                            , strong []
                                [ text <| annualInterestRate minPaymentPlan
                                , text "%"
                                ]
                            ]
                        ]
                    ]
                ]
            , div [ class "col-sm-6" ]
                [ div [ class "panel panel-default" ]
                    [ div [ class "panel-heading" ] [ text "Maximum interest rate example" ]
                    , viewPaymentPlan maxPaymentPlan
                    , p
                        []
                        [ h4 [ class "text-center" ]
                            [ text "The interest rate for this payment is "
                            , strong []
                                [ text <| annualInterestRate maxPaymentPlan
                                , text "%"
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
