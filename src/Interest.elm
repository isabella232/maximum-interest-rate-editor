module Interest exposing (show)

import Quarter exposing (Quarter)
import Round


show : Int -> Maybe Float -> String -> String
show n maybe_rate publicationName =
    let
        maybe_quarter =
            Quarter.fromName publicationName
    in
    case ( maybe_rate, maybe_quarter ) of
        ( Just rate, Just quarter ) ->
            -- n = len(plans) + 1
            -- 1 / n * (n - 1 - sum([1 / (1 + r) ** (t / 365) for t in plans]))
            let
                sum =
                    Quarter.buildPlanDays quarter n
                        |> List.map (\t -> 1 / (1 + rate / 100) ^ (toFloat t / 365))
                        |> List.sum

                rounded_value =
                    (1 / toFloat n * (toFloat n - 1 - sum) * 10000)
                        |> Round.floor 0
            in
            rounded_value ++ " bps"

        ( _, _ ) ->
            "-,-- %"
