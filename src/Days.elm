module Days exposing (Installment, buildPlanDays, getPNXPaymentPlan, timeBetweenPayments, toPosix)

import Date
import Iso8601
import Time exposing (Month(..), Posix, utc)
import Time.Extra as TE exposing (Interval(..))


type alias Installment =
    { dueDate : String
    , totalAmount : Int
    , purchaseAmount : Int
    , customerInterest : Int
    }


toPosix : String -> Posix
toPosix =
    Iso8601.toTime
        >> Result.withDefault (Time.millisToPosix 0)


toString : Posix -> String
toString =
    Iso8601.fromTime >> String.left 10


schedulePaymentDates : Int -> Posix -> List Posix
schedulePaymentDates installments_count starting_date =
    List.range 0 (installments_count - 1)
        |> List.map (\i -> TE.add Month i utc starting_date)


scheduleYearlyPaymentDates : Posix -> List Posix
scheduleYearlyPaymentDates =
    schedulePaymentDates 12


timeBetweenPayments : Posix -> List Int
timeBetweenPayments starting_date =
    scheduleYearlyPaymentDates starting_date
        |> List.drop 1
        |> List.map (\end_date -> TE.diff Day utc starting_date end_date)


buildPlanDays : Int -> List Int -> List Int
buildPlanDays installments_count days =
    days
        |> List.take (installments_count - 1)


getPurchaseAmountPhasing installmentsCount purchaseAmount =
    let
        n =
            toFloat installmentsCount |> Debug.log "n"

        amount =
            toFloat purchaseAmount |> Debug.log "amount"

        alpha =
            1 / n

        after =
            floor <| amount * (1 - alpha) / (n - 1)

        first =
            purchaseAmount - ((installmentsCount - 1) * after)
    in
    first :: List.repeat (installmentsCount - 1) after


getPNXPaymentPlan installmentsCount startingDate purchaseAmount customerFee =
    let
        dates =
            List.map toString <| schedulePaymentDates installmentsCount (toPosix startingDate)

        purchaseAmountPhasing =
            getPurchaseAmountPhasing installmentsCount purchaseAmount

        totalAmountPhasing =
            case purchaseAmountPhasing of
                [] ->
                    []

                x :: xs ->
                    (x + customerFee) :: xs
    in
    -- List.map3 (\dueDate purchaseAmount totalAmount -> { dueDate = toString dueDate, totalAmount = totalAmount, purchaseAmount = purchaseAmount, customerInterest = 0 }) dates purchaseAmountPhasing totalAmountPhasing
    List.map4 Installment dates totalAmountPhasing purchaseAmountPhasing <| List.repeat installmentsCount 0
