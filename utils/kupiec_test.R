# utils/kupiec_test.R

kupiec_test <- function(actual_returns, var_series, alpha = 0.05) {
  n <- length(actual_returns)
  failures <- actual_returns < -var_series
  x <- sum(failures)
  pi_hat <- x / n
  
  if (pi_hat == 0 || pi_hat == 1) return(list(
    breaches = x,
    LR_pof = NA,
    p_value = NA,
    result = "Test undefined (0% or 100% breach rate)"
  ))
  
  lr_pof <- -2 * (log((1 - alpha)^(n - x) * alpha^x) -
                    log((1 - pi_hat)^(n - x) * pi_hat^x))
  
  p_value <- 1 - pchisq(lr_pof, df = 1)
  
  list(
    breaches = x,
    LR_pof = round(lr_pof, 4),
    p_value = round(p_value, 4),
    result = ifelse(p_value > 0.05, "Model passes Kupiec test", "Model fails Kupiec test")
  )
}
