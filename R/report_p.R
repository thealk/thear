#' Report pvals in RMarkdown
#' @export

report_p <- function(x){
  return(ifelse(x<0.001,
                "<0.001",
                paste0("= ",as.character(round(x,3)))
  ))
}
