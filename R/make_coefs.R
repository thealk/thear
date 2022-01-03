#' Create coefficients dataframes to use in in-line R reporting.
#' Output: vector of beta estimates, p-values, and effect sizes (delta)
#' Effect sizes based on Brysbaert, M. and Stevens, M. 2018 Power Analysis and Effect Size in Mixed Effects Models: A Tutorial. Journal of Cognition, 1(1): 9, pp. 1–20, DOI: https://doi.org/10.5334/joc.10
#' Hedges, L. V. (2007). Effect Sizes in Cluster-Randomized Designs. Journal of Educational and Behavioral Statistics, 32(4), 341–370.
#' https://journals.sagepub.com/doi/pdf/10.3102/1076998606298043
#' See also this Stack Exchange post: https://stats.stackexchange.com/questions/257985/how-can-i-derive-effect-sizes-in-lme4-and-describe-the-magnitude-of-fixed-effect
#' @export
#'
# Create coefs function
make_coefs <- function(model, mod_name){
  df <- summary(model)$coefficients %>%
    as.data.frame() %>%
    rownames_to_column()

  # Get total variance in order to calculate effect size
  # Source: https://www.journalofcognition.org/articles/10.5334/joc.10/
  mod_var <- summary(model)$varcor %>% as.data.frame()
  tot_var <- sum(mod_var$vcov)

  df <- df %>%
    mutate(eff_size = Estimate/sqrt(tot_var)) %>%
    mutate_if(is.numeric,round,3) %>%
    column_to_rownames()

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
