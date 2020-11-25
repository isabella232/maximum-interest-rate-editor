module Interest exposing (show)

import Days
import Quarter
import Round


getPnxMaxBPS : Int -> Float -> List Int -> String
getPnxMaxBPS installments_count rate planDurations =
    -- n = len(plans) + 1
    -- 1 / n * (n - 1 - sum([1 / (1 + r) ** (t / 365) for t in plans]))
    let
        sum =
            Days.buildPlanDays planDurations installments_count
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
