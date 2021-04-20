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

  # Get total variance in order to calculate effect size
  mod_var <- summary(model)$varcor %>% as.data.frame()
  tot_var <- sum(mod_var$vcov)

  df <- df %>%
    mutate(eff_size = Estimate/sqrt(tot_var))

  b <- df['Estimate']
  p <- df['Pr(>|t|)']
  d <- df['eff_size']

  b_name <- paste0(mod_name,"_B")
  p_name <- paste0(mod_name,"_P")
  d_name <- paste0(mod_name,"_D")

  assign(b_name, b, envir=parent.frame())
  assign(p_name, p, envir=parent.frame())
  assign(d_name, d, envir=parent.frame())
}
