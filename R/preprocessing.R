
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

install.packages("quanteda")
install.packages("devtools")
library(quanteda)
devtools::install_github("kbenoit/quanteda.dictionaries")

data(data_dictionary_MFD, package = "quanteda.dictionaries")

### 1984

txt <- textreadr::read_docx("Coded.President Debate Transcript 1960-2016/1960-4/October 7.docx")

txt$docx

txt <- readLines("debate_data/1984-1st- Reagan-Mondale.txt")

txt <- paste(txt, collapse = " \n ")

#tpc <- unlist(regmatches(txt, gregexpr("XXXX\\s*[A-z]+", txt)))

#cag <- unlist(strsplit(txt, split = "XXXX"))

#MR. MONDALE:
#THE PRESIDENT:

mondale <- sub(".*(MR. MONDALE:.*)(MR|MS).*", "\\1", cag[2])
mondale <- gsub("(\\s|[A-z]|\\n|[[:punct:]])+MR. MONDALE:((\\s|[A-z])+)", "\\1", cag[2])
#mondale <- unlist(regmatches(txt, gregexpr("MR. MONDALE", cag[2])))

mondale <- unlist(stringr::str_extract_all(
  cag[2], "MR. MONDALE:((\\s|[a-z]|[[:punct:]]|\\n|[0-9]|\\$)+|([A-Z]([a-z]|\\s|[[:punct:]])+))"))

pp <- unlist(strsplit(cag[2], split = "(?<=[A-Z][A-Z].?\\s?[A-Z]+)", perl = T))

#### use this one
#pp <- unlist(strsplit2(cag[2], split = "(MR|MS)", type = "before"))
pp <- unlist(strsplit2(txt, split = "(MR|MS|THE)", type = "before"))

indx1 <- grep("MR. MONDALE:", pp)
pp1 <- pp[indx1]
pp1 <- paste(pp_m, collapse = "\n")

indx2 <- grep("THE PRESIDENT:", pp)
pp2 <- pp[indx2]
pp2 <- paste(pp2, collapse = "\n")

deb <- c(pp1, pp2)

# number of words in a text matching the MFD dictionary
moral <- dfm(deb) %>%
  dfm_lookup(dictionary = data_dictionary_MFD)

moral <- quanteda::convert(moral, to = "data.frame")
moral[1, -1] <- apply(moral[1, -1], 1, 
                      function(x) 100*as.numeric(x)/stringr::str_count(pp_p, "\\S+"))
moral[2, -1] <- apply(moral[2, -1], 1, 
                      function(x) 100*as.numeric(x)/stringr::str_count(pp_m, "\\S+"))
moral$document <- c("Mondale", "Reagon")

write.csv(moral, "results/1984.csv")

### 1996

txt <- readLines("debate_data/1996-1st- Clinton-Dole.txt")

txt <- paste(txt, collapse = " \n ")

pp <- unlist(strsplit2(txt, split = "\\b[[:upper:]]+:", type = "before"))

indx1 <- grep("CLINTON:", pp)
pp1 <- pp[indx1]
pp1 <- paste(pp1, collapse = "\n")

indx2 <- grep("DOLE:", pp)
pp2 <- pp[indx2]
pp2 <- paste(pp2, collapse = "\n")

deb <- c(pp1, pp2)

moral <- dfm(deb) %>%
  dfm_lookup(dictionary = data_dictionary_MFD)

moral <- quanteda::convert(moral, to = "data.frame")
moral[1, -1] <- apply(moral[1, -1], 1, 
                      function(x) 100*as.numeric(x)/stringr::str_count(pp1, "\\S+"))
moral[2, -1] <- apply(moral[2, -1], 1, 
                      function(x) 100*as.numeric(x)/stringr::str_count(pp2, "\\S+"))
moral$document <- c("Clinton", "Dole")

write.csv(moral, "results/1996.csv")

### 2016

txt <- readLines("debate_data/2016-1st Trump-Hillary.txt")

txt <- paste(txt, collapse = " \n ")

pp <- unlist(strsplit2(txt, split = "\\b[[:upper:]]+:", type = "before"))

indx1 <- grep("CLINTON:", pp)
pp1 <- pp[indx1]
pp1 <- paste(pp1, collapse = "\n")

indx2 <- grep("TRUMP:", pp)
pp2 <- pp[indx2]
pp2 <- paste(pp2, collapse = "\n")

deb <- c(pp1, pp2)

moral <- dfm(deb) %>%
  dfm_lookup(dictionary = data_dictionary_MFD)

moral <- quanteda::convert(moral, to = "data.frame")
moral[1, -1] <- apply(moral[1, -1], 1, 
                      function(x) 100*as.numeric(x)/stringr::str_count(pp1, "\\S+"))
moral[2, -1] <- apply(moral[2, -1], 1, 
                      function(x) 100*as.numeric(x)/stringr::str_count(pp2, "\\S+"))
moral$document <- c("Hillary", "Trump")

write.csv(moral, "results/2016.csv")


