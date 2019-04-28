#' Calculate habitual mean for RADI dataset
#' @export

# df requires columns: participant, utterance_duration, rate (incl H1)

# For each speaker (participant)
# Actual utterance duration = utterance_duration (dur = utterance duration plus buffer)
calculate_habitual_mean <- function(df){
  habit_df <- data.frame(matrix(ncol = 3,nrow=0))
  habit_names <- c("participant","habitual_mean")
  names(habit_df) <- habit_names

  for(p in levels(df$participant)){ # calculates based on participant levels in intell
    tmp_rate <- df %>%
      filter(participant==p) %>%
      filter(rate=="H1") %>%
      summarise_at(vars(utterance_duration),
                   funs(mean(., na.rm = TRUE))) %>%
      mutate(participant = p) %>%
      rename(mean_habit_rate = utterance_duration) %>%
      select(participant, mean_habit_rate)
    habit_df <- rbind(habit_df, tmp_rate)
  }
  return(habit_df)
}
