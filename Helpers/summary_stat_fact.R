summary_stat_fact = function(df, var, constraint_var = NULL, constraint_value = NULL) {
  
  if (is.null(constraint_var) & is.null(constraint_value)) {
    factor = df[, var]
  } else if (is.null(constraint_var) | is.null(constraint_value)) {
    stop("constraint_var or constraint_value is not defined. Review function values.")
  } else {
    constraint = sym(constraint_var)
    df         = df %>%
                  dplyr::filter(!!constraint == constraint_value)
    factor   = df[, var]
  }
  
  frequency_df = table(factor) %>% 
    as.data.frame() %>%
    dplyr::rename(frequency = Freq)

  proportion_df = prop.table(table(factor)) %>%
    as.data.frame() %>%
    dplyr::rename(proportion = Freq) %>%
    dplyr::mutate(proportion = round(proportion, 4)*100,
                  proportion = as.character(proportion) %>% paste("%"))

  summary = frequency_df %>%
    dplyr::inner_join(proportion_df, by = "factor") %>%
    dplyr::arrange(desc(frequency))

  return(summary)
  
}