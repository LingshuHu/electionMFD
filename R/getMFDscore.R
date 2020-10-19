
strsplit2 <- function(x,
                     split,
                     type = "remove",
                     perl = FALSE,
                     ...) {
  if (type == "remove") {
    # use base::strsplit
    out <- base::strsplit(x = x, split = split, perl = perl, ...)
  } else if (type == "before") {
    # split before the delimiter and keep it
    out <- base::strsplit(x = x,
                          split = paste0("(?<=.)(?=", split, ")"),
                          perl = TRUE,
                          ...)
  } else if (type == "after") {
    # split after the delimiter and keep it
    out <- base::strsplit(x = x,
                          split = paste0("(?<=", split, ")"),
                          perl = TRUE,
                          ...)
  } else {
    # wrong type input
    stop("type must be remove, after or before!")
  }
  return(out)
}

#install.packages("quanteda")
#install.packages("devtools")
library(quanteda)
#devtools::install_github("kbenoit/quanteda.dictionaries")

data(data_dictionary_MFD, package = "quanteda.dictionaries")


### read all documents

folders <- list.files(path = "Coded.President Debate Transcript 1960-2016")
folders2 <- vector("list", length = length(folders))
for (i in seq_along(folders)) {
  folders2[[i]] <- list.files(paste0("Coded.President Debate Transcript 1960-2016/", folders[i]),
                         full.names = TRUE)
}

folders2 <- unlist(folders2)

txtall <- vector("list", length = length(folders2))
for (i in seq_along(folders2)) {
  txtall[[i]] <- textreadr::read_docx(folders2[i])
}


# "00MR\\.\\s?[[:upper:]]+:|00MS\\.\\s?[[:upper:]]+:|00THE\\s?\\.\\s?[[:upper:]]+:|00[[:upper:]]+:"
getspeech <- function(txt) {
  date <- txt[1]
  date <- sub(" Debate.*", "", date)
  txt <- paste(txt, collapse = "\n")
  ### parse data
  pattern <- "\\\n([[:upper:]]|\\.|\\s)+:"
  names <- stringr::str_extract_all(txt, pattern)
  names <- unique(unlist(names))
  pp <- unlist(strsplit2(txt, 
                         split = pattern, 
                         type = "before"))
  #speaker_num <- length(names)
  speaker_content <- vector("list", length = length(names))
  for (i in seq_along(names)) {
    indx <- grep(names[i], pp, fixed = TRUE)
    #print(indx)
    pp_one <- pp[indx]
    pp_one <- paste(pp_one, collapse = "\n")
    speaker_content[[i]] <- pp_one
  }
  speaker_content <- unlist(speaker_content)
  #speaker_content <- gsub("00", "\n", speaker_content)
  speaker_content <- gsub("^\\n", "", speaker_content)
  speakers <- gsub("\\\n|:", "", names)
  speeches <- data.frame(speaker = speakers, date = date, content = speaker_content)
  return(speeches)
}

all_speeches <- vector("list", length = length(txtall))
for (i in seq_along(txtall)) {
  all_speeches[[i]] <- getspeech(txtall[[i]])
}

all_speeches <- do.call("rbind", all_speeches)

#x <- rjson::toJSON(unname(split(all_speeches, 1:nrow(all_speeches))))
#x <- jsonify::to_json(all_speeches)
xlsx::write.xlsx(all_speeches, "debate_data/all_speeches.xlsx")
saveRDS(all_speeches, "debate_data/all_speeches.rds")

getMFDscore <- function(txt) {
  date <- txt[1]
  date <- sub(" Debate.*", "", date)
  txt <- paste(txt, collapse = "00")
  ### parse data
  pattern <- "00([[:upper:]]|\\.|\\s)+:"
  names <- stringr::str_extract_all(txt, pattern)
  names <- unique(unlist(names))
  pp <- unlist(strsplit2(txt, 
                         split = pattern, 
                         type = "before"))
  #speaker_num <- length(names)
  speaker_content <- vector("list", length = length(names))
  for (i in seq_along(names)) {
    indx <- grep(names[i], pp, fixed = TRUE)
    #print(indx)
    pp_one <- pp[indx]
    pp_one <- paste(pp_one, collapse = "\n")
    speaker_content[[i]] <- pp_one
  }
  speaker_content <- unlist(speaker_content)
  moral <- dfm_lookup(dfm(speaker_content), dictionary = data_dictionary_MFD)
  #moral <- quanteda::convert(moral, to = "data.frame")
  morals <- vector("list", length = length(speaker_content))
  for (i in seq_along(speaker_content)) {
    one_moral <- as.vector(moral[i, ])
    one_moral <- 100*one_moral/stringr::str_count(speaker_content[[i]], "\\S+")
    morals[[i]] <- one_moral
  }
  morals_df <- t(data.frame(morals))
  rownames(morals_df) <- NULL #seq_along(speaker_content)
  colnames(morals_df) <- colnames(moral)
  speakers <- gsub("00|:", "", names)
  morals_df <- data.frame(speaker = speakers, date = date, morals_df)
  return(morals_df)
}

all_morals <- vector("list", length = length(txtall))
for (i in seq_along(txtall)) {
  all_morals[[i]] <- getMFDscore(txtall[[i]])
}

all_morals <- do.call("rbind", all_morals)

write.csv(all_morals, "results/all_speakers_MFD.csv")
