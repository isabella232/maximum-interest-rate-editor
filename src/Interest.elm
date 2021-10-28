module Interest exposing (annual_interest_rate, show)

import Days
import Newton
import Quarter
import Round

rateWithinDays : Int -> Int -> Float
rateWithinDays rate deltaI =
    (1 + toFloat rate / 10000) ^ (toFloat deltaI / 365 - 1)

getPnxMaxBPS : Int -> Int -> List Int -> String
getPnxMaxBPS installments_count rate planDurations =
    -- n = len(plans) + 1
    -- 1 / n * (n - 1 - sum([1 / (1 + r) ** (t / 365) for t in plans]))
    let
        sum =
            planDurations
                |> Days.buildPlanDays installments_count
                |> List.map (\t -> 1 / (1 + toFloat rate / 10000) ^ (toFloat t / 365))
                |> List.sum

        rounded_value =
            (1 / toFloat installments_count * (toFloat installments_count - 1 - sum) * 10000)
                |> Round.floor 0
    in
    rounded_value

alpha : Int -> List Int -> Float
alpha rate planDuration =
    case planDuration |> List.reverse of
       [] -> 1.0
       deltaI :: remainingDuration -> ((rateWithinDays rate deltaI) + 1) * (alpha rate <| List.reverse remainingDuration) + 1

beta : Int -> List Int -> Float
beta rate planDuration =
    case planDuration |> List.reverse of
       [] -> 0.0
       deltaI :: remainingDuration ->
            let
                monthlyRate = rateWithinDays rate deltaI
            in
            (monthlyRate + 1) * (beta rate <| List.reverse remainingDuration) - monthlyRate

installmentAmount : Int -> List Int -> Float
installmentAmount rate planDuration =
    (1 - beta rate planDuration) / (alpha rate planDuration)

getCreditMaxBPS : Int -> Int -> List Int -> String
getCreditMaxBPS installments_count rate planDurations =
    let
        maxBPS = toFloat installments_count * (installmentAmount rate planDurations) - 1
    in
    Round.floor 0 (maxBPS * 10000)


getMaxBPS : Int -> Int -> List Int -> String
getMaxBPS installments_count =
    if installments_count > 4 then getCreditMaxBPS installments_count
    else getPnxMaxBPS installments_count

show : Int -> Maybe Int -> String -> String
show installments_count maybe_rate publicationName =
    let
        maybe_quarter =
            Quarter.fromName publicationName
    in
    case ( maybe_rate, maybe_quarter ) of
        ( Just rate, Just quarter ) ->
            -- n = len(plans) + 1
            -- 1 / n * (n - 1 - sum([1 / (1 + r) ** (t / 365) for t in plans]))
            let
                rounded_value =
                    Quarter.days quarter
                        |> List.map (getMaxBPS installments_count rate)
                        |> List.minimum
            in
            rounded_value
                |> Maybe.map (\bps -> bps ++ " bps")
                |> Maybe.withDefault "--- bps"

        ( _, _ ) ->
            "--- bps"


annual_interest_rate : Float -> Float -> List Int -> String
annual_interest_rate purchase_amount customer_fee planDurations =
    if customer_fee < 0 then
        "0,00"

    else
        let
            customer_fees_rate =
                customer_fee / purchase_amount

            n =
                planDurations
                    |> List.length
                    |> (+) 1
                    |> toFloat

            f_sum x =
                List.map (\d -> (1 / (1 + x)) ^ (toFloat d / 365)) planDurations
                    |> List.sum

            f =
                \x -> 1 - n * (1 - customer_fees_rate) + f_sum x

            maybe_taeg =
                Newton.optimize f
        in
        case maybe_taeg of
            Nothing ->
                "-,--"

            Just taeg ->
                if taeg > 10 then
                    "-,--"

                else
                    Round.round 2 (taeg * 100)
