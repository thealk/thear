#' Calculate the alternative speech rate metrics used in RADI
#'
#' Rates include: mean_habit_wpm,, wpm, prop_wpm, dev_wpm, percChange_wpm, and prop_wpm_3, 5, and 7 (binned into 3 5 and 7 bins)
#' @param df Data frame containing columns "participant"
#' @param habit_means_df Summary data frame containing mean habitual utterance duration (`mean_habit_rate`) for each participant. Defaults to habit_means_df, and can be created with thear::calculate_habitual_mean. Defaults to `habit_means_df``
#' @export

# df requires columns: participant, prop_wpm
# takes df and adds 3 new vars, one for each bin range (3, 5, 7)
# calculates bins for each participant (so each participant will have data in each bin)

# THIS IS A GROSS FUNCTION BUT IT WORKS
# Calculates proportional rate in wpm (prop_wpm) in bins of 3, 5, and 7, centered around habitual rate (i.e., when prop_wpm = 1) for each participant
calculate_radi_rates <- function(df, habit_means_df = habit_df){
  # Takes DF, and for each PARTICIPANT!, calculates range of numeric (rate) variable (intended for prop_wpm)
  # Splits range up into nBins (default = 3) for each group

  # Create relevant dur columns first:

  df <- df %>%
  mutate(mean_habit_dur = habit_df$mean_habit_rate[
    match(unlist(df$participant),
          habit_df$participant)]) %>%
    mutate(mean_habit_wpm = (nwords/mean_habit_dur)*60,
           wpm = (nwords/utterance_duration)*60) %>%
    mutate(prop_wpm = wpm/mean_habit_wpm) %>% #>1=fast, <1=slow
    mutate(dev_wpm = wpm - mean_habit_wpm) %>% # >0 = fast, <0 = slow
    mutate(percChange_wpm = (mean_habit_wpm - wpm)/mean_habit_wpm)


  range_bins_df <- data.frame(matrix(ncol = ncol(df),nrow=0))
  names(range_bins_df) <- names(df)


  for(p in levels(df$participant)){ # used to be group, changed it to participant
    tmp_range_less1 <- df %>%
      filter(participant==p,
             prop_wpm <= 1) %>%
      mutate(prop_wpm_3 = cut(prop_wpm, 3),
             prop_wpm_5 = cut(prop_wpm, 5),
             prop_wpm_7 = cut(prop_wpm, 7))
    levels(tmp_range_less1$prop_wpm_3) = c("s3","s2","s1")
    levels(tmp_range_less1$prop_wpm_5) = c("s5","s4","s3","s2","s1")
    levels(tmp_range_less1$prop_wpm_7) = c("s7", "s6", "s5","s4","s3","s2","s1")

    tmp_range_more1 <- df %>%
      filter(participant==p,
             prop_wpm > 1) %>%
      mutate(prop_wpm_3 = cut(prop_wpm, 3),
             prop_wpm_5 = cut(prop_wpm, 5),
             prop_wpm_7 = cut(prop_wpm, 7))
    levels(tmp_range_more1$prop_wpm_3) = c("f1","f2","f3")
    levels(tmp_range_more1$prop_wpm_5) = c("f1","f2","f3","f4","f5")
    levels(tmp_range_more1$prop_wpm_7) = c("f1","f2","f3","f4","f5","f6","f7")

    tmp_range <- rbind(tmp_range_less1, tmp_range_more1)


    # Source: https://stackoverflow.com/questions/30032616/how-to-combine-two-levels-in-one-categorical-variable-in-r
    levels(tmp_range$prop_wpm_3)[levels(tmp_range$prop_wpm_3)%in%c("s3","s2")] <- "S2"
    levels(tmp_range$prop_wpm_3)[levels(tmp_range$prop_wpm_3)%in%c("s1","f1")] <- "H1"
    levels(tmp_range$prop_wpm_3)[levels(tmp_range$prop_wpm_3)%in%c("f2","f3")] <- "F1"

    levels(tmp_range$prop_wpm_5)[levels(tmp_range$prop_wpm_5) %in% c("s5","s4")] <- "S3"
    levels(tmp_range$prop_wpm_5)[levels(tmp_range$prop_wpm_5) %in% c("s3","s2")] <- "S2"
    levels(tmp_range$prop_wpm_5)[levels(tmp_range$prop_wpm_5) %in% c("s1","f1")] <- "H1"
    levels(tmp_range$prop_wpm_5)[levels(tmp_range$prop_wpm_5) %in% c("f2","f3")] <- "F2"
    levels(tmp_range$prop_wpm_5)[levels(tmp_range$prop_wpm_5) %in% c("f4","f5")] <- "F3"

    levels(tmp_range$prop_wpm_7)[levels(tmp_range$prop_wpm_7) %in% c("s7","s6")] <- "S4"
    levels(tmp_range$prop_wpm_7)[levels(tmp_range$prop_wpm_7) %in% c("s5","s4")] <- "S3"
    levels(tmp_range$prop_wpm_7)[levels(tmp_range$prop_wpm_7) %in% c("s3","s2")] <- "S2"
    levels(tmp_range$prop_wpm_7)[levels(tmp_range$prop_wpm_7) %in% c("s1","f1")] <- "H1"
    levels(tmp_range$prop_wpm_7)[levels(tmp_range$prop_wpm_7) %in% c("f2","f3")] <- "F2"
    levels(tmp_range$prop_wpm_7)[levels(tmp_range$prop_wpm_7) %in% c("f4","f5")] <- "F3"
    levels(tmp_range$prop_wpm_7)[levels(tmp_range$prop_wpm_7) %in% c("f6","f7")] <- "F4"

    # tmp_range %>% ggplot(aes(x=prop_wpm_3))+geom_histogram(stat="count")
    # tmp_range %>% ggplot(aes(x=prop_wpm_5))+geom_histogram(stat="count")

    range_bins_df <- rbind(range_bins_df, tmp_range)
  }
  return(range_bins_df)
}



# TESTING
# range_bins_df <- calculate_range_bins(intell_stops)
#
# range_bins_df %>% ggplot(aes(x=prop_wpm_3))+geom_histogram(stat="count")+facet_wrap(~group)
# range_bins_df %>% ggplot(aes(x=prop_wpm_5))+geom_histogram(stat="count")+facet_wrap(~group)
# range_bins_df %>% ggplot(aes(x=prop_wpm_7))+geom_histogram(stat="count")+facet_wrap(~group)
# range_bins_df %>%
#         #filter(group=="PD") %>%
#         tjmisc::sample_n_of(participant,size=12) %>%
#         ggplot(aes(x=prop_wpm_7, y=wpm, color=group))+
#         geom_jitter()+
#         facet_wrap(~participant,ncol=4) # not all participants will have points in each range; should the range be calculated for each PARTICIPANT rather than each group?
#
# summary(range_bins_df$prop_wpm_3)
# summary(range_bins_df$prop_wpm_5)
# summary(range_bins_df$prop_wpm_7)
# range_bins_df %>% ggplot(aes(x=prop_wpm,y=prop_wpm_3))+geom_density_ridges()+facet_wrap(~group)
# range_bins_df %>% ggplot(aes(x=prop_wpm,y=prop_wpm_5))+geom_density_ridges()+facet_wrap(~group)
# range_bins_df %>% ggplot(aes(x=prop_wpm,y=prop_wpm_7))+geom_density_ridges()+facet_wrap(~group)
#
