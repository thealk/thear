#' Open a .wav and corresponding .TextGrid in system viewer (e.g., Praat)
#' @export

# open_tg_wav()
#system2("open", "/Users/theaknowles/Documents/GitHub/dissertation_textgrids/textgrids/soi_level/textgrids_soi_edited/stops/slower3x_304_4_1_trimmed.TextGrid")

# tgPath = "/Users/theaknowles/Documents/GitHub/dissertation_textgrids/textgrids/soi_level/textgrids_soi_edited/stops/"
# wavPath = "/Users/theaknowles/Documents/radi_audio_soi/1_wavs_segmented_unscaled/stops_trimmed/"


open_tg_wav <- function(filename, wPath=wavPath, tPath=tgPath){
  system2("open", paste0(wPath,filename,".wav"))
  system2("open", paste0(tPath,filename,".TextGrid"))
}

#open_tg_wav(tmp)
