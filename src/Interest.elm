module Interest exposing (getCreditPaymentPlan, optimal_interest_rate, show)

import Days exposing (Installment, timeBetweenPayments)
import Newton
import Quarter
import Round
import Time exposing (utc)
import Time.Extra as TE exposing (Interval(..))


rateWithinDays : Float -> Int -> Float
rateWithinDays rate days =
    (1 + rate) ^ (toFloat days / 365) - 1


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
        [] ->
            1.0

        days :: remainingDuration ->
            (rateWithinDays (toFloat rate / 10000) days + 1) * (alpha rate <| List.reverse remainingDuration) + 1


beta : Int -> List Int -> Float
beta rate planDuration =
    case planDuration |> List.reverse of
        [] ->
            0.0

        days :: remainingDuration ->
            let
                monthlyRate =
                    rateWithinDays (toFloat rate / 10000) days
            in
            (monthlyRate + 1) * (beta rate <| List.reverse remainingDuration) - monthlyRate


installmentAmount : Int -> List Int -> Float
installmentAmount rate planDuration =
    (1 - beta rate planDuration) / alpha rate planDuration


getCreditMaxBPS : Int -> Int -> List Int -> String
getCreditMaxBPS installments_count rate planDurations =
    let
        daysBetweenPayments =
            planDurations
                |> Days.buildPlanDays installments_count
                |> List.foldl (\nbDaysFromStartDate acc -> nbDaysFromStartDate - List.sum acc :: acc) []
                |> List.reverse

        maxBPS =
            toFloat installments_count * installmentAmount rate daysBetweenPayments - 1
    in
    Round.floor 0 (maxBPS * 10000)


getMaxBPS : Int -> Int -> List Int -> String
getMaxBPS installments_count =
    if installments_count > 4 then
        getCreditMaxBPS installments_count

    else
        getPnxMaxBPS installments_count


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


optimal_interest_rate : Int -> List Installment -> Maybe Float
optimal_interest_rate purchaseAmount paymentPlan =
    case paymentPlan of
        [] ->
            Nothing

        firstInstallment :: xs ->
            let
                startDate =
                    firstInstallment.dueDate
                        |> Days.toPosix
                        |> TE.add Day 3 utc

                planDurations =
                    startDate
                        |> Days.timeBetweenPayments
                        |> Days.buildPlanDays (List.length paymentPlan)
                        |> Debug.log "planDuration"

                loanAmount =
                    purchaseAmount - firstInstallment.purchaseAmount

                f_sum x =
                    List.map2 (\d installment -> toFloat installment.totalAmount * (1 / (1 + x)) ^ (toFloat d / 365)) planDurations xs
                        |> List.sum

                f x =
                    toFloat purchaseAmount - f_sum x

                maybe_taeg =
                    Newton.optimize f
                        |> Debug.log "optimize"
            in
            maybe_taeg
                |> Maybe.andThen
                    (\taeg ->
                        if taeg > 10 || taeg < 0 then
                            Nothing

                        else
                            Just taeg
                    )


getCreditPaymentPlan : Int -> String -> Int -> Int -> List Installment
getCreditPaymentPlan installmentsCount startingDate purchaseAmount customerFee =
    let
        dates =
            List.map Days.toString <| Days.schedulePaymentDates installmentsCount (Days.toPosix startingDate)

        totalAmountPhasing =
            Days.getPurchaseAmountPhasing installmentsCount (purchaseAmount + customerFee)

        customerFeePhasing =
            List.repeat installmentsCount 0

        daysBetweenPayments =
            startingDate
                |> Days.toPosix
                |> Days.timeBetweenPayments
                |> Days.buildPlanDays installmentsCount
                |> List.foldl (\nbDaysFromStartDate acc -> nbDaysFromStartDate - List.sum acc :: acc) []
                |> List.reverse

        maybe_taeg =
            List.map4 Installment
                dates
                totalAmountPhasing
                totalAmountPhasing
                customerFeePhasing
                |> optimal_interest_rate purchaseAmount
                |> Debug.log "maybe taeg"
    in
    case maybe_taeg of
        Nothing ->
            []

        Just taeg ->
            let
                ( _, purchaseAmountPhasing, interestPhasing ) =
                    List.map2 Tuple.pair totalAmountPhasing (0 :: daysBetweenPayments)
                        |> List.foldl
                            (\( totalAmount, days ) ( capitalLeftToPay, purchaseAcc, interestAcc ) ->
                                let
                                    monthlyRate =
                                        rateWithinDays taeg days
                                            |> Debug.log "monthyRate"

                                    _ =
                                        Debug.log "days" days

                                    interest =
                                        if days == 0 then
                                            0

                                        else
                                            (capitalLeftToPay * monthlyRate)
                                                |> round
                                                |> Debug.log "interest"

                                    amount =
                                        (totalAmount - interest)
                                            |> Debug.log "amount"
                                in
                                ( capitalLeftToPay - toFloat amount, amount :: purchaseAcc, interest :: interestAcc )
                            )
                            ( toFloat purchaseAmount, [], [] )
            in
            List.map4 Installment
                dates
                totalAmountPhasing
                (purchaseAmountPhasing |> List.reverse)
                (interestPhasing |> List.reverse)
