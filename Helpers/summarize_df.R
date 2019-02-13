# Summary information for numeric and factor variables
summarize_df <- function(df, wrt = NULL, vars_mean, vars_count) {
  # Create empty vector
  summary_df <- c()
  # For the case in which the summary is general
  if(is.null(wrt)) {
    # Variables of which we calculate the mean
    for (var in vars_mean) {
      var_name <- sym(var)
      mean_df <- df %>% dplyr::summarize(!!var_name := mean(!!var_name))
      summary_df <- c(summary_df,mean_df)
    }
    # Variables of which we count the unique values of the factors
    for (var in vars_count) {
      var_name <- sym(var)
      count_df <- df %>%
        recode_all(var) %>%
        group_by(!!var_name) %>% 
        dplyr::summarize(count = n()) %>%
        spread(key = !!var_name, value = count)    
      summary_df <- summary_df %>% cbind(count_df)
    }
    # For the case in which the summary is done wrt a variable
  } else {
    wrt_name <- sym(wrt)
    # Variables of which we calculate the mean
    for (var in vars_mean) {
      var_name <- sym(var)
      mean_df <- df %>%
        group_by(!!wrt_name) %>% 
        dplyr::summarize(!!var_name := mean(!!var_name))
      summary_df <- c(summary_df,mean_df)
      summary_df[duplicated(summary_df)] <- NULL
    }
    # Variables of which we count the unique values of the factors
    for (var in vars_count) {
      var_name <- sym(var)
      count_df <- df %>%
        recode_all(var) %>%
        group_by(!!var_name, !!wrt_name) %>%
        dplyr::summarize(count = n()) %>%
        spread(key = !!var_name, value = count)  
      summary_df <- summary_df %>% as.data.frame() %>%
        dplyr::mutate(!!wrt_name := as.character(!!wrt_name)) %>%
        full_join(count_df, by = wrt) %>%
        replace(is.na(.), 0)
    }
    # Percentage of the rows
    percentage_df <- df %>%
      group_by(!!wrt_name) %>%
      dplyr::summarize(percentage = round(n()/nrow(.)*100, 2))
    summary_df <- summary_df %>% full_join(percentage_df, by = wrt)
  }
  return(summary_df)
}