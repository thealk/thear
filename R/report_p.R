#' Report pvals in RMarkdown
#' @export

report_p <- function(x){
  return(ifelse(x<0.001,
                "< .001",
                paste0("= ",APAstyler::snip(round(x,3),1))
                #scales::pvalue(x,add_p = TRUE),1)))
                #paste0("= ",as.character(round(x,3)))
  ))
}
