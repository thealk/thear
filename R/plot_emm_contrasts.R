#' Plot results of emmeans contrasts
#' @export

# Plotting emmeans

plot_emm_contrasts <- function(emm) {
  # emm = emmeans(model, pairwise ~ rate|group|task)
  x_emm <- summary(emm)$contrasts %>%
    as.data.frame()

  x_emm$breaks <- cut(
    x_emm$p.value,
    breaks = c(-1, 0.001, 0.01, 0.05, 0.1, 1),
    # lower bound is less than 0
    labels = c("p < 0.001",
               "p < 0.01",
               "p < 0.05",
               "p < 0.1",
               "n.s.")
  )

  x_emm %>%
    ggplot(aes(x = contrast, y = estimate, fill = breaks)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("red", "orange", "yellow", "grey", "black")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  #facet_grid(group~task)
}
