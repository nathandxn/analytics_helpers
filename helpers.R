get_stats_for_booleans <- function(x, y, test = 't') {

    if (test == 't') {

        t_test <- t.test(y ~ x)
        avg_diff <- t_test$estimate[2] - t_test$estimate[1]

        names(avg_diff) <- NULL
        results <- c(
              count_true = sum(x)
            , pct_true = round(sum(x)/length(x), 4)
            , avg_if_true = as.vector(round(t_test$estimate[2], 4))
            , avg_if_false = as.vector(round(t_test$estimate[1], 4))
            , avg_diff_in_group = avg_diff
            , p_value = t_test$p.value
            )
    }
    else if (test == 'prop') {

        prop_test <- prop.test(table(x, factor(y, levels = c('1', '0'), ordered = TRUE)))
        prop_diff <- prop_test$estimate[2] - prop_test$estimate[1]

        names(prop_diff) <- NULL
        results <- c(
              count_true = sum(x)
            , pct_true = round(sum(x)/length(x), 4)
            , prop_if_true = as.vector(round(prop_test$estimate[2], 4))
            , prop_if_false = as.vector(round(prop_test$estimate[1], 4))
            , prop_diff_in_group = avg_diff
            , p_value = t_test$p.value
            )
    }

    return(results)

}