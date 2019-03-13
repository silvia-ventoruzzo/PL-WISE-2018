# Spread rows into columns using just one variable
from_row_to_col = function(df, variable) {
    df = df %>%
      dplyr::mutate(yesno = 1) %>%
      dplyr::distinct() %>%
      tidyr::spread(variable, yesno, fill = 0)
    return(df)
}