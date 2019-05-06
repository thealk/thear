# Thea's misc functions
# Updated: March 20, 2019

# Functions used in RADI scripts
# REFER TO THIS DOCUMENT IN FUTURE R SCRIPTS AS THE SOURCE DOCUMENT, E.G.
# source("/Users/thea/Documents/R Files/RFunctions.R")


#' Create summary table.
#' [Source](http://www.cookbook-r.com/Manipulating_data/Summarizing_data/).
#' Warning: detaches and reloads dplyr
#' @export

# SummarySE ----
# Source: http://www.cookbook-r.com/Manipulating_data/Summarizing_data/
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  #detach(package:dplyr)
  #library(plyr)

  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }

  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )

  # Rename the "mean" column
  datac <- rename(datac, c("mean" = measurevar))

  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult

  return(datac)
  #library(plyr)
}

#' Make a cumulative sums dataframe
#' @table A table
# makeCumulative----
makeCumulative <- function(table){
  #assumes two rows, 6 colums:
  # 1. 0-5ms   2. 5-10ms  3. 10-25ms  4. 25-50ms 5. 50-100ms   6. >100ms
  df <- as.data.frame(table)
  under5ms <- df[1,2]
  under10ms <- under5ms + df[2,2]
  under25ms <- under10ms + df[3,2]
  under50ms <- under25ms + df[4,2]
  under100ms <- under50ms + df[5,2]
  over100ms <- df[6,2]
  total <- under100ms + over100ms

  under5P <- 100*(under5ms/total)
  under10P <- 100*(under10ms/total)
  under25P <- 100*(under25ms/total)
  under50P <- 100*(under50ms/total)
  under100P <- 100*(under100ms/total)
  over100P <- 100*(over100ms/total)

  threshold = c("1.<5ms","2.<10ms","3.<25ms","4.<50ms","5.<100ms")
  cumulativeTot = c(under5ms,under10ms,under25ms,under50ms,under100ms)
  cumulativePerc = c(under5P,under10P,under25P,under50P,under100P)
  cumulativedf = data.frame(threshold,cumulativeTot,cumulativePerc)

  return(cumulativedf)

}

#' Plot many plots
#' @export
# multiplot----
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#' Split filenames from RADI into meaningful units
#'
#' This is designed to be used with the RADI data
#' @param df Dataframe containing a column filename to be split
#' @export
# radi_filename_prep----
# Splits filename of radi into condition, participant, item, irrelevant, wav_status, & creates new group, rate columns
# Factors condition, participant, item, group
radi_filename_prep <- function(df){
  df <- df %>%
    separate(filename, c("condition", "participant", "item", "irrelevant", "wav_status"), remove=FALSE) %>%
    mutate(group = factor(str_sub(participant, 1, 1)),
           rate = revalue(condition, replace = c("slower4x" = "S4",
                                                 "slower3x" = "S3",
                                                 "slower2x" = "S2",
                                                 "habitual" = "H1",
                                                 "faster2x" = "F2",
                                                 "faster3x" = "F3",
                                                 "faster4x" = "F4"))) %>%
    mutate(rate = factor(rate, levels = c("S4","S3","S2","H1","F2","F3","F4"))) %>%
    mutate(group = revalue(group, replace = c("1" = "YC", # There is no YC in the acoustics
                                              "2" = "OC",
                                              "3" = "PD",
                                              "5" = "DBS"))) %>%
    mutate(condition = factor(condition),
           participant = factor(participant),
           item = factor(item),
           group = factor(group))
  return(df)
}


#' Remove duplicate files from visual analog scale results
#' @export
# Remove_duplicaites_vas----
# REMOVE DUPLICATES FUNCTION for VAS.PRAAT OUTPUT

# This will get rid of duplicate filenames
# Designed to handle output csv files from vas.praat

# Input: dataframe (df) corresponding to csv output from vas.praat. Assumes input is a df that contains columns:
#    - filename
#    - listener
#    - order
# Output:
#    - df_unique: original df with duplicates removed. 'df' matches name provided as input (1st listener presentation)
#    - df_duplicates: df of the files that were duplicated (2nd listener presentation). Original structure preserved

remove_duplicates <- function(file){
  file$listener <- factor(file$listener)
  df_unique_allListeners <- data.frame()
  df_duplicate_allListeners <- data.frame()

  for(l in 1:length(levels(file$listener))){
    current_l <- levels(file$listener)[l]
    temp <- subset(file, listener==current_l)
    temp$is_duplicate <- temp$filename %in% temp$filename[duplicated(temp$filename)]
    temp_unique <- subset(temp, is_duplicate=="FALSE")
    temp_duplicate <- subset(temp, is_duplicate=="TRUE")

    # Order by listening order & create new column, order2, which corresponds to row number
    temp_duplicate <- temp_duplicate[order(temp_duplicate$filename,
                                           temp_duplicate$order),] %>%
      mutate(order2 = seq(1:length(filename)))
    # Add keep column that is TRUE if order2 is odd (was the first presentation) and FALSE if order2 is even (was second presentation)
    temp_duplicate$keep <- FALSE
    temp_duplicate$keep[temp_duplicate$order2 %% 2 != 0] <- TRUE

    # duplicate1 will get shuffled back in to the main data, duplicate2 will be preserved for ICC analyses later
    temp_duplicate_1 <- subset(temp_duplicate, keep == TRUE) %>%
      select(-is_duplicate, -order2, -keep)
    temp_duplicate_2 <- subset(temp_duplicate, keep == FALSE) %>%
      select(-is_duplicate, -order2, -keep)
    temp_unique <- temp_unique %>%
      select(-is_duplicate)

    # Shuffle temp_duplicate_1 back in to temp_unique
    df_unique <- rbind(temp_unique, temp_duplicate_1)
    df_duplicate <- temp_duplicate_2

    df_unique_allListeners <- rbind(df_unique_allListeners, df_unique)
    df_duplicate_allListeners <- rbind(df_duplicate_allListeners, df_duplicate)
  }

  # Assign return dfs names based on function input
  df_unique_name <- paste(as.character(sys.call()[-1]), "_unique", sep = "")
  df_duplicate_name <- paste(as.character(sys.call()[-1]), "_duplicate", sep = "")
  assign(df_unique_name,
         df_unique_allListeners,envir=parent.frame())
  assign(df_duplicate_name,
         df_duplicate_allListeners,envir=parent.frame())

}

# USAGE:
#    remove_duplicates(vwlsb)

# SANITY CHECK:
# If all values from the following line are FALSE, it means there are no duplicates (per listener)
#        subset(vwlsb_unique,listener=="1")$filename %in% subset(vwlsb_unique,listener=="1")$filename[duplicated(subset(vwlsb_unique,listener=="1")$filename)]

# Use this line to plot counts of filenames. All should be equal to 1
#    ggplot(vwlsb_unique, aes(x=filename))+
#         geom_bar()+
#          facet_grid(~listener)
