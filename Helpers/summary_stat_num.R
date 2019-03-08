summary_stat_num = function(df, var, constraint_var = NULL, constraint_value = NULL) {
  
  if (is.null(constraint_var) & is.null(constraint_value)) {
    variable = df[, var]
  } else if (is.null(constraint_var) | is.null(constraint_value)) {
    stop("constraint_var or constraint_value is not defined. Review function values.")
  } else {
    constraint = sym(constraint_var)
    df         = df %>%
                  dplyr::filter(!!constraint == constraint_value)
    variable   = df[, var]
  }
  
  summary = data.frame(
              min    = min(variable),
              `1Q`   = quantile(variable, probs = 0.25),
              median = median(variable),
              `3Q`   = quantile(variable, probs = 0.75),
              max    = max(variable),
              iqr    = IQR(variable),
              mean   = mean(variable),
              sd     = sd(variable),
         check.names = FALSE) %>%
    dplyr::mutate_if(is.numeric, function(x) round(x, 4))

  return(summary)
  
}