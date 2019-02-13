# Spread rows into columns using just one variable
from_row_to_col <- function(df, variable) {
  df <- df %>%
    mutate(yesno = 1) %>%
    distinct() %>%
    spread(variable, yesno, fill = 0)
  return(df)
}