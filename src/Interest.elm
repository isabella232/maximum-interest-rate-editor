module Interest exposing (annual_interest_rate, show)

import Days
import Newton
import Quarter
import Round


getPnxMaxBPS : Int -> Float -> List Int -> String
getPnxMaxBPS installments_count rate planDurations =
    -- n = len(plans) + 1
    -- 1 / n * (n - 1 - sum([1 / (1 + r) ** (t / 365) for t in plans]))
    let
        sum =
            planDurations
                |> Days.buildPlanDays installments_count
                |> List.map (\t -> 1 / (1 + rate / 100) ^ (toFloat t / 365))
                |> List.sum

        rounded_value =
            (1 / toFloat installments_count * (toFloat installments_count - 1 - sum) * 10000)
                |> Round.floor 0
    in
    rounded_value


show : Int -> Maybe Float -> String -> String
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
                        |> List.map (getPnxMaxBPS installments_count rate)
                        |> List.minimum
            in
            rounded_value
                |> Maybe.map (\bps -> bps ++ " bps")
                |> Maybe.withDefault "-,-- %"

        ( _, _ ) ->
            "-,-- %"


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
                Round.round 2 (taeg * 100)
                    |> String.replace "." ","
