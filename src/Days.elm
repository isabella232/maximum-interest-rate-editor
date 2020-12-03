module Days exposing (buildPlanDays, timeBetweenPayments, toPosix)

import Iso8601
import Time exposing (Month(..), Posix, utc)
import Time.Extra as TE exposing (Interval(..))


toPosix : String -> Posix
toPosix =
    Iso8601.toTime
        >> Result.withDefault (Time.millisToPosix 0)


schedulePaymentDates : Posix -> List Posix
schedulePaymentDates starting_date =
    List.range 1 12
        |> List.map (\i -> TE.add Month i utc starting_date)


timeBetweenPayments : Posix -> List Int
timeBetweenPayments starting_date =
    schedulePaymentDates starting_date
        |> List.map (\end_date -> TE.diff Day utc starting_date end_date)


buildPlanDays : Int -> List Int -> List Int
buildPlanDays installments_count days =
    days
        |> List.take (installments_count - 1)
