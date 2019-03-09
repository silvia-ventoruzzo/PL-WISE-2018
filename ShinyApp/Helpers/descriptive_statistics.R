descriptive_statistics = function(df, constraint_var = NULL, constraint_value = NULL) {
  
  if (is.null(constraint_var) & is.null(constraint_value)) {
    
    df = df
    
  } else if (is.null(constraint_var) | is.null(constraint_value)) {
    
    stop("constraint_var or constraint_value is not defined. Review function values.")
    
  } else {
    
    constraint = sym(constraint_var)
    df = df %>%
      dplyr::filter(!!constraint == constraint_value)
    
  }
  
  # Descriptive statistics for numeric variables
  if (unique(apply(df, 2, function(x) is.numeric(x)))) {
  
    summary = data.frame(
                variable = names(df),
                min      = apply(df, 2, min),
                `1Q`     = apply(df, 2, quantile, probs = 0.25),
                median   = apply(df, 2, median),
                `3Q`     = apply(df, 2, quantile, probs = 0.75),
                max      = apply(df, 2, max),
                iqr      = apply(df, 2, IQR),
                mean     = apply(df, 2, mean),
                sd       = apply(df, 2, sd),
        check.names = FALSE) %>%
      dplyr::mutate_if(is.numeric, function(x) round(x, 4))
  
  # Descriptive statistics for categorical variables  
  } else if (unique(apply(df, 2, function(x) is.character(x) | is.factor(x)))) {
    
    df = listings %>% select(room_type, property_type)
    
    # Frequency
    frequency_list = apply(df, 2, table)
    
    frequency_df = data.frame(frequency = unlist(frequency_list)) %>%
      tibble::rownames_to_column(var = "var_fact")
    
    freq_var = colsplit(string = frequency_df$var_fact, pattern = "\\.", names = c("variable", "factor"))
    
    frequency = freq_var %>%
      cbind(frequency_df) %>%
      dplyr::select(-var_fact)
      
    # Proportion
    proportion_list = apply(df, 2, function(x) prop.table(table(x)))
    
    proportion_df = data.frame(proportion = unlist(proportion_list)) %>%
      tibble::rownames_to_column(var = "var_fact")
    
    prop_var = colsplit(string = proportion_df$var_fact, pattern = "\\.", names = c("variable", "factor"))
    
    proportion = prop_var %>%
      cbind(proportion_df) %>%
      dplyr::select(-var_fact) %>%
      dplyr::mutate(proportion = round(proportion, 4)*100,
                    proportion = as.character(proportion) %>% paste("%"))
    
    summary = frequency %>%
      dplyr::inner_join(proportion, by = c("variable", "factor")) %>%
      dplyr::arrange(variable, desc(frequency))
    
  } else {
    
    stop("Check that all variables are either numeric or categorical.")
    
  }
  
  return(summary)
  
}