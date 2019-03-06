# Recode variable: rename all factor levels of that variable
recode_all <- function(df, variable) {
  variable_name <- sym(variable)
  df <- df %>%
    mutate(!!variable_name := as.factor(!!variable_name))
  levels(df[,variable]) <- gsub(" ", "_", levels(df[,variable])) %>%
    tolower() %>%
    paste(variable, ., sep = "_")
  return(df)
}