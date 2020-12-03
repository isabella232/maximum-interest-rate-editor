module TAEG exposing (main)

import Browser
import Date exposing (Date)
import Days
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Input.Float as MaskedPercentage
import Interest
import Round
import Task
import Time exposing (Month(..))


type alias Model =
    { purchaseAmount : Maybe Float
    , startDate : Maybe String
    , installmentsCount : Maybe Int
    , paidAmount : Maybe Float
    }


type FieldType
    = PurchaseAmount
    | InstallmentsCount
    | PaidAmount


type Msg
    = InputChanged FieldType String
    | ReceiveDate Date


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
    ( { purchaseAmount = Just 100
      , startDate = Nothing
      , installmentsCount = Just 3
      , paidAmount = Just 101.55
      }
    , Date.today |> Task.perform ReceiveDate
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveDate today ->
            ( { model | startDate = Date.toIsoString today |> Just }, Cmd.none )

        InputChanged PurchaseAmount value ->
            ( { model | purchaseAmount = String.toFloat value }, Cmd.none )

        InputChanged InstallmentsCount value ->
            ( { model | installmentsCount = String.toInt value }, Cmd.none )

        InputChanged PaidAmount value ->
            ( { model | paidAmount = String.toFloat value }, Cmd.none )


annual_interest_rate : Maybe String -> Maybe Float -> Maybe Int -> Maybe Float -> String
annual_interest_rate maybe_startDate maybe_purchaseAmount maybe_installmentsCount maybe_paidAmount =
    case ( ( maybe_startDate, maybe_purchaseAmount ), ( maybe_installmentsCount, maybe_paidAmount ) ) of
        ( ( Just startDate, Just purchaseAmount ), ( Just installmentsCount, Just paidAmount ) ) ->
            let
                customerFees =
                    paidAmount - purchaseAmount

                planDurations =
                    startDate
                        |> Days.toPosix
                        |> Days.timeBetweenPayments
                        |> Days.buildPlanDays installmentsCount
            in
            Interest.annual_interest_rate purchaseAmount customerFees planDurations

        _ ->
            "-,--"


view : Model -> Html Msg
view { startDate, purchaseAmount, installmentsCount, paidAmount } =
    div []
        [ div [ class "col-sm-6" ]
            [ div [ class "form-group" ]
                [ label [ for "purchase_amount", class "col-sm-2 control-label" ] [ text "Montant de l'achat" ]
                , div [ class "col-sm-10" ]
                    [ input
                        [ type_ "text"
                        , class "form-control"
                        , id "purchase_amount"
                        , value <| String.fromFloat <| Maybe.withDefault 0 purchaseAmount
                        , onInput <| InputChanged PurchaseAmount
                        ]
                        []
                    ]
                ]
            , div [ class "form-group" ]
                [ label [ for "installments_count", class "col-sm-2 control-label" ] [ text "Nombre d'échéances" ]
                , div [ class "col-sm-10" ]
                    [ input
                        [ type_ "text"
                        , class "form-control"
                        , id "installments_count"
                        , value <| String.fromInt <| Maybe.withDefault 0 installmentsCount
                        , onInput <| InputChanged InstallmentsCount
                        ]
                        []
                    ]
                ]
            , div [ class "form-group" ]
                [ label [ for "paid_amount", class "col-sm-2 control-label" ] [ text "Montant payé" ]
                , div [ class "col-sm-10" ]
                    [ input
                        [ type_ "text"
                        , class "form-control"
                        , id "paid_amount"
                        , value <| String.fromFloat <| Maybe.withDefault 0 paidAmount
                        , onInput <| InputChanged PaidAmount
                        ]
                        []
                    ]
                ]
            ]
        , div [ class "col-sm-6" ]
            [ p []
                [ br [] []
                , h1 [ class "text-center" ]
                    [ text "Votre TAEG pour ce paiement est de "
                    , text <| annual_interest_rate startDate purchaseAmount installmentsCount paidAmount
                    , text "%"
                    ]
                ]
            ]
        ]
