rm(list=ls())
library(tm)
library(stringr)
uri <- "2013_abstract.pdf"
# download.file("http://www.enar.org/meetings2013/2013_abstracts.pdf", uri)
last <- function(x) {
  l <- length(x)
  if (l == 0) return(NA)
  x[l]
}

absl <- readPDF(control = list(text = "-layout"))(elem = list(uri = uri),
                                                                  language = "en",
                                                                  id = "idabs")

xabs <- abs <- readPDF(control = list(text = "-nopgbrk -f 2 -l 118"))(elem = list(uri = uri),
                                             language = "en",
                                             id = "idabs")

abs <- xabs[ !xabs%in% c("CT", "S|", "ENAR 2013", "A", "R", "T", "ABS", "NT", 
                         "POSTER PR", "S", "E", "ES", "I", "AT", "O", "N", "Spring Meeting", "March 10 – 13", 
                         "Abstracts", "AB", "ENAR 2013 • Spring Meeting • March 10–13", "|", "C", "ON", 
                         "PO S", "ST", "TER PRESE", "TS")]

emails <- grepl("^email:", abs)
all.caps <- gsub("\\d*[a-z]\\. \\a(.*)", "\\1", abs)
# strp


ss <- strsplit(abs, " ")
last.words <- sapply(ss, last)
have.comma <- grepl(",", ss)
last.words <- last.words[ !grepl("@", last.words)]

# 
# 
# 
# 
program <- "2013_program.pdf"
# download.file("http://www.enar.org/meetings2013/2013_Spring_Preliminary_Program.pdf", uri)
# 
posters <- readPDF(control = list(text = "-f 22 -l 38"))(elem = list(uri = uri),
                                             language = "en",
                                             id = "idprog")

pres <- readPDF(control = list(text = "-f 39 -l 130 -layout"))(elem = list(uri = uri),
                                                     language = "en",
                                                     id = "idprog")
# pp2 <- strsplit(pres, "\t")

pp <- gsub("(.*((a|p)\\.\\m.)).*", "\\1", pres)
times <- grepl("(a|p)\\.\\m.", pp)
pp[!times] <- NA
### need to take out particulate matter
pp <- gsub("On the Use of a p.m.", "", pp)
### take out when session talks about lenght of next segment
pp[grepl("–", pp)] <- NA
pp <- gsub("[A-Z]", "", pp)
pp <- str_trim(pp)



pp <- str_trim(pp)
pp <- strsplit(pres, "    ")
pp <- lapply(pp, str_trim)
pp <- lapply(pp, function(x) {
  x <- x[ x!= ""]
  if (length(x) < 1) return(c("", ""))
  if (length(x) < 2) return(c("", x))
  c(x[1], str_trim(paste(x[2:length(x)], sep="", collapse=" ")))
})
pp <- do.call("rbind", pp)
# 
# ### ideas - getting all the 