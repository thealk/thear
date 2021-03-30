#' Create coefficients dataframes to use in in-line R reporting.
#' @export
#'
# Create coefs function
make_coefs <- function(model, mod_name){
  df <- summary(model)$coefficients %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    mutate_if(is.numeric,round,3) %>%
    column_to_rownames()

  b <- df['Estimate']
  p <- df['Pr(>|t|)']

  b_name <- paste0(mod_name,"_B")

  p_name <- paste0(mod_name,"_P")

  assign(b_name, b, envir=parent.frame())
  assign(p_name, p, envir=parent.frame())
}
