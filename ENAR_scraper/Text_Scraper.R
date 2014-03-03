rm(list=ls())
library(tm)
library(stringr)
library(zoo)
library(plyr)
library(wordcloud)
homedir = "~/Dropbox/Computing_Club/ENAR_scraper"
homedir = path.expand(homedir)
# pdfdir = file.path(homedir, "pdfs")
txtdir = file.path(homedir, "txts")

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

txts = dir(path=txtdir, pattern="titles", full.names=TRUE)

raw = readLines(txts[1])
# ss   = strsplit(raw, "")

df = data.frame(raw=raw, stringsAsFactors=FALSE)
# sapply(LETTERS, grepl())
df$times = grepl("^\\d(|\\d):\\d\\d", df$raw)
df$id = cumsum(df$times)

df = df[ df$id > 0, ]

### take out trailing and leading spaces
df$raw = str_trim(df$raw)
df = df[ !(df$raw %in% ""), ]

### remove entries 
df = df[ !(df$raw %in% "Scientific Program"), ]
df = df[ !(df$raw %in% 
             c("Monday, March 18", 
               "Tuesday, March 19",
               "Wednesday, March 20")), ]

droptimes = c("1:45–3:30 p.m.", "10:30 a.m.–12:15 p.m.", "3:30–3:45 p.m.",
              "3:45–5:30 p.m.", "10:15–10:30 a.m.",  
              "8:30–10:15 a.m.")
# "10:30 Introduction",

df = df[ !(df$raw %in% droptimes), ]

### remove free text
bad.ind = grep("Times may change slightly prior to the meetings", df$raw)
df = df[ -(bad.ind:(bad.ind+1)), ]

tab = sort(table(df$raw))
### getting session information
df$is.sess = grepl("^\\d(|\\d)\\..*", df$raw)
df$session = gsub("^(\\d(|\\d))\\..*", "\\1", df$raw)
df$session[ !df$is.sess ] = NA
df$session = na.locf(df$session)

# df$id [df$is.sess] = NA
# df$id = na.locf(df$id, fromLast=TRUE)

#### remove times
df$raw = gsub("^\\d(|\\d):\\d\\d (.*)", "\\2", df$raw)

df$nc = nchar(df$raw)

### drop not relevant rows
df$uid = 1:nrow(df)
df$spons = grepl("^Sponsor:", df$raw)
df$chair = grepl("Chair|Participants:", df$raw)
df$remove = df$spons | df$chair

### remove contributed papers
contrib.id = df$session[grepl("Contributed Papers:", df$raw)]

contributed = df[ df$session %in% contrib.id, ]

df = df[ !(df$session %in% contrib.id),]

df = ddply(.data=df, .(session), function(x){
  x$sessid = 1:nrow(x)
  x
})


### remove floor discussions
df = df[ !grepl("^\\d(|\\d):\\d\\d Floor Discussion$", df$raw), ]
df = df[ !grepl("^Floor Discussion$", df$raw), ]


### remove end of page lines
df = df[ !grepl("ENAR 2002 Spring Meeting$", df$raw),]
df = df[ !grepl("^Arlington, Virginia", df$raw),]

ldf = dlply(.data=df, .(session), function(x){
  w = which(x$remove)
  w = w[1]:w[length(w)]
  if (length(w) > 0) x = x[-w, ]
  x$remove = NULL
  x
})

df = do.call("rbind", ldf)
### checks
stopifnot( all( !grepl("Sponsor:", df$raw) ))
stopifnot( all( !grepl("Organizer(s):", df$raw) ))
stopifnot( all( !grepl("Chair(s):", df$raw) ))

### collapse session name
#### drop panel discussion
df = df[ !(df$session %in% c("60")), ]

### making the session information - collapsing multiple rows
df = ddply(.data=df, .(session), function(x){
  #   print(x$session[1])
  sess.start = which(x$is.sess)[1]
  time = which(x$times)[1]-1
  p = paste(x$raw[sess.start:time], collapse=" ", sep="")
  p = gsub("^(\\d(|\\d))\\.(.*)", "\\3", p)
  x$raw[1] = str_trim(p)
  #### the first indicator is always session
  ind = 1:nrow(x)
  sess.ind = sess.start:time
  sess.ind = sess.ind[!(sess.ind %in% 1)]
  if (length(sess.ind) > 0) x = x[- sess.ind,]
  x
})


df = df[ order(df$uid), ]
df$sess = df$raw
df$sess[ !df$is.sess ] = NA
df$sess = na.locf(df$sess)
df = df[ !df$is.sess, ]

### adding the session id
df = ddply(.data=df, .(session), function(x) {
  x$sessid = cumsum(x$times)
  x
})

df = df[ order(df$uid), ]

df$com = grepl(",", df$raw)

ldf = dlply(.data=df, .(session), function(x) x)



### collapsing time info to the first comma 

#### drop discussion - like floor discussions, but may be multi-line
disc.ind =  grepl("^Discussant(s|)(:|,)", df$raw)
disc.ids = df$id[ disc.ind]

disc = df[  (df$id %in% disc.ids),]
# df$raw[ df$id %in% disc.ids]

df = df[ !(df$id %in% disc.ids), ]

stopifnot( !any(grepl("^Discussant", df$raw)))


df = ddply(.data=df, .(id), function(x){
#   print(x$id[1])
  start = which(x$times)[1]
  coms = which(x$com)
  first.com = coms[1]
  if (first.com == start) first.com = coms[2]
  end = first.com-1
  p = paste(x$raw[start:end], collapse=" ", sep="")
  x$raw[start] = str_trim(p)
  p = paste(x$raw[first.com:nrow(x)], collapse=" ", sep="")
  x$raw[first.com] = str_trim(p)
  
  x = x[c(start, first.com), ]
  x
})

df = df[ order(df$uid), ]


df = ddply(.data=df, .(id), function(x){
  x$id.ind = 1:nrow(x)
  x
})

dat = df[ , c("raw", 'sess')]

stopifnot( !any(duplicated(dat)))


ddf = df[, c("raw", "sess", "id.ind", "sessid")]

m = reshape(ddf, direction ="wide", idvar=c("sess", "sessid"), 
            timevar="id.ind")
cn = colnames(m)
cn[cn == "raw.1"] = "Title"
cn[cn == "raw.2"] = "Authors"
colnames(m) = cn

ses = unique(df$sess)
ses = gsub("/", " ", ses)
ses = tolower(ses)
ses = unlist(strsplit(ses, " "))
ses = ses[ses != ""]

wordcloud(ses)


ses = unique(m$Title)
ses = gsub("/", " ", ses)
ses = gsub("-", "", ses)
ses = tolower(ses)
ses = unlist(strsplit(ses, " "))
ses = ses[ses != ""]

wordcloud(ses, random.order=FALSE)


# m = dcast(ddf, session + id.ind ~sessid , identity,
#           value.var= "raw")


