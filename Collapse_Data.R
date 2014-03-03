rm(list=ls())
library(tm)
library(stringr)
library(zoo)
library(plyr)
library(wordcloud)
library(reshape2)
homedir = "~/Dropbox/database"
homedir = path.expand(homedir)
options(stringsAsFactors=FALSE)

mid = function (x, n = 6L, ...) {
  stopifnot(length(n) == 1L)
  nr = nrow(x)
  mid = floor(nr/2)
  n2 = floor(n/2)
  start = max( mid - n2, 1)
  end = min( mid + n2, nr)
  ind = seq(start, end, by=1)
  x[ind, , drop = FALSE]
}

#### 2012 data
dat = read.csv(file.path(homedir, 
                         "2k12 Full Dump 11.01.2012.csv"), 
               na.strings="", fileEncoding="latin1")
cn = colnames(dat)
pres = grep("^presenter.*ation", cn, value=TRUE)
data = dat[, c("username", "Talk_Title", "FirstCategory", "SecondCategory", 
               "category", "abstract", "presenter1affiliation")]
data$year = 2013
dat = NULL

############# 2012 data - same format as 2012
dat = read.csv(file.path(homedir, 
                         "ENAR_Abstracts_2k11FullDump_20120118.csv"), 
               na.strings="", fileEncoding="latin1")
dat = dat[, c("username", "Talk_Title", "FirstCategory", "SecondCategory", 
              "category", "abstract", "presenter1affiliation")]
dat$year = 2012
data = rbind(data, dat)

############# 2011 data - same format as 2012
dat = read.csv(file.path(homedir, 
                             "2011Full Dump 2010 11 16 FINAL with invited.csv"), 
               na.strings="", fileEncoding="latin1")
dat = dat[, c("username", "Talk_Title", "FirstCategory", "SecondCategory", 
                      "category", "abstract", "presenter1affiliation")]
dat$year = 2011
data = rbind(data, dat)


############# 2011 data - same format as 2012
dat = read.csv(file.path(homedir, 
                         "2011Full Dump 2010 11 16 FINAL with invited.csv"), 
               na.strings="", fileEncoding="latin1")
dat = dat[, c("username", "Talk_Title", "FirstCategory", "SecondCategory", 
              "category", "abstract", "presenter1affiliation")]
dat$year = 2011
data = rbind(data, dat)


############# 2010 data - same format as 2012
dat = read.csv(file.path(homedir, 
                         "AbstractsFullDump120809.csv"), 
               na.strings="", fileEncoding="latin1")
dat = dat[, c("username", "Talk_Title", "FirstCategory", "SecondCategory", 
              "category", "abstract", "presenter1affiliation")]
dat$year = 2010
data = rbind(data, dat)

############# 2009 data - same format as 2012
dat = read.csv(file.path(homedir, 
                         "ENAR Abstract Full Dump 120808.csv"), 
               na.strings="", fileEncoding="latin1")
dat = dat[, c("username", "Talk_Title", "FirstCategory", "SecondCategory", 
              "category", "abstract", "presenter1affiliation")]
dat$year = 2009
data = rbind(data, dat)


############# 2008 data - same format as 2012
dat = read.csv(file.path(homedir, 
                         "enar11-16-07.csv"), 
               na.strings="", fileEncoding="latin1")
dat = dat[, c("username", "Talk_Title", "FirstCategory", "SecondCategory", 
              "category", "abstract", "presenter1affiliation")]
dat$year = 2008
data = rbind(data, dat)



############# 2008 data - same format as 2012
dat = read.csv(file.path(homedir, 
                         "Dump2k6.csv"), 
               na.strings="", fileEncoding="latin1")
dat = dat[, c("username", "Talk_Title", "FirstCategory", "SecondCategory", 
              "category", "abstract", "presenter1affiliation")]
dat$year = 2007
data = rbind(data, dat)

save(data, file=file.path(homedir, "Collapsed_2007_2013.Rda"))