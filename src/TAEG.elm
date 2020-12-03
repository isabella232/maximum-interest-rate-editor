module TAEG exposing (main)

import Browser
import Date exposing (Date)
import Days
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Input.Float as CurrencyInput
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
    | PaidAmount


type Msg
    = DateChanged String
    | InstallmentsCountChanged String
    | CurrencyChanged FieldType (Maybe Float)
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

        DateChanged value ->
            ( { model | startDate = Just value }, Cmd.none )

        CurrencyChanged PurchaseAmount value ->
            ( { model | purchaseAmount = value }, Cmd.none )

        InstallmentsCountChanged value ->
            ( { model | installmentsCount = String.toInt value }, Cmd.none )

        CurrencyChanged PaidAmount value ->
            ( { model | paidAmount = value }, Cmd.none )


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


inputOptions : FieldType -> CurrencyInput.Options Msg
inputOptions field =
    let
        defaultOptions =
            CurrencyInput.defaultOptions (CurrencyChanged field)
    in
    { defaultOptions
        | minValue = Just 0
    }


currencyInput : FieldType -> Maybe Float -> String -> Html Msg
currencyInput field fieldInfo formName =
    div [ class "input-group" ]
        [ CurrencyInput.input
            (inputOptions field)
            [ class "form-control", id formName ]
            fieldInfo
        , span [ class "input-group-addon" ] [ text "€" ]
        ]


view : Model -> Html Msg
view { startDate, purchaseAmount, installmentsCount, paidAmount } =
    div []
        [ div [ class "col-sm-6" ]
            [ div [ class "form-group col-sm-6" ]
                [ label [ for "purchase_amount", class "col-sm-6 control-label" ] [ text "Montant de l'achat" ]
                , div [ class "col-sm-6" ]
                    [ currencyInput PurchaseAmount purchaseAmount "purchase_amount" ]
                ]
            , div [ class "form-group col-sm-6" ]
                [ label [ for "paid_amount", class "col-sm-6 control-label" ] [ text "Montant payé" ]
                , div [ class "col-sm-6" ]
                    [ currencyInput PaidAmount paidAmount "paid_amount"
                    ]
                ]
            , div [ class "form-group col-sm-6" ]
                [ label [ for "start_date", class "col-sm-6 control-label" ] [ text "Date d'achat" ]
                , div [ class "col-sm-6" ]
                    [ input
                        [ type_ "date"
                        , class "form-control"
                        , style "padding-top" "0"
                        , id "start_date"
                        , value <| Maybe.withDefault "" startDate
                        , onInput <| DateChanged
                        ]
                        []
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
                            , onInput <| InstallmentsCountChanged
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
                    , text <| annual_interest_rate startDate purchaseAmount installmentsCount paidAmount
                    , text "%"
                    ]
                ]
            ]
        ]
