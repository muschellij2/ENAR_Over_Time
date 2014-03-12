rm(list=ls())
library(tm)
library(stringr)
library(zoo)
library(plyr)
library(wordcloud)
library(reshape2)
homedir = "~/Dropbox/database/ENAR_scraper"
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

txts = dir(path=txtdir, pattern="abstracts", full.names=TRUE)

raw = readLines(txts[11])
# ss   = strsplit(raw, "")

df = data.frame(raw=raw, stringsAsFactors=FALSE)
# sapply(LETTERS, grepl())
df$id = grepl("^\\d(|\\d)(|\\d)\\.", df$raw)
df$sess = df$session = NA
df$sess[df$id] = as.numeric(gsub("^(\\d(|\\d)(|\\d))\\.(.*)", "\\1", df$raw[df$id]))
df$sess = na.locf(df$sess, na.rm=FALSE)
df$session[df$id] = df$raw[df$id]
df$session = na.locf(df$session, na.rm=FALSE)
df$times = grepl("^\\d(|\\d):\\d\\d", df$raw)
df$day = df$time = NA
df$time[df$times] = gsub("^(\\d(|\\d):\\d\\d) (.*)", "\\1", 
                         df$raw[df$times])
df$time = na.locf(df$time, na.rm=FALSE)

df$isday = grepl("Sunday|Monday|Tuesday|Wednesday", df$raw)
df$day[df$isday] = df$raw[df$isday]
df$day = na.locf(df$day, na.rm=FALSE)

df$wh = grepl('(R|r)oom', df$raw)
df$where = NA
df$where[df$wh] = df$raw[df$wh]
wh = which(df$wh)
df$where = na.locf(df$where, na.rm=FALSE)

head(df)
sess = df[, c("session", "where", "sess", "day", "raw", "wh", "id")]
sess = sess[!is.na(sess$sess),]
sess = ddply(sess, .(sess), function(x){
  if (any(x$wh))  {
    wid = which(x$id)
    if (length(wid)  >1 ) {
      print(x[1,])
      stop("blah")
    }
    wiw = which(x$wh)[1]-1
    if (length(wiw)  >1 ) {
      print(x[1,])
      stop("blah2")
    }    
    ind = seq(from=wid, to=wiw)
  } else {
    ind = which(x$id)
  }
  x = x[ind,]
  raw = paste(x$raw, collapse =" ")
  x = x[1,]
  x$sessname = raw
  x
})
sess$wh = sess$id = sess$raw = NULL

sess = sess[complete.cases(sess),]
sess = unique(sess)
sess = ddply(sess, .(sess), function(x){
  x[1,]
})
head(sess)

df = df[ !is.na(df$sess),]
post = grepl("POSTER", df$session)
posters = df[post,]
posters$pid = grepl("^\\d(|\\d)(|\\d)[A-Z]\\.", posters$raw)

posters$psess = NA
posters$psess[posters$pid] = posters$raw[posters$pid]
posters$psess = na.locf(posters$psess, na.rm=FALSE)

posters = posters[ !is.na(posters$psess),]

posters = ddply(posters, .(psess), function(x){
  raw = paste(x$raw, collapse =" ")
  x = x[1,]
  x$raw = raw
  x
})

posters = merge(posters, sess, all.x=TRUE)

df = df[!post,]
head(df, 20)

table(df$session)
df$id = df$day = df$session = df$where = NULL
head(df, 20)

df = df[!grepl("\342\200\223", df$time), ]
head(df, 20)


df = ddply(df, .(sess), function(x){
  if (any(x$wh))  {
    wtime = which(x$times)[1]
    wiw = which(x$wh)[1]
    ind = seq(from=wiw, to=wtime)
    x = x[-c(ind),]
  }
  x
})


df = df[!df$times,]
df$uid = seq(nrow(df))
id = unique(df[, c("sess", "time")])

id$id = seq(nrow(id))
df = df[!df$wh, ]
df = merge(df, id, all.x=TRUE, sort=FALSE)
df = df[order(df$uid), ]
print(tail(df, 20))



df$times = df$isday = df$wh = df$sess = df$time  = NULL

n = ddply(df, .(id), function(x){
  x$visit = 1:nrow(x)
  x[, c("visit", "uid"), drop=FALSE]
})
n$id = NULL
df = merge(df, n, by="uid", sort=FALSE)
df = df[order(df$uid), ]

df$uid = NULL

df = df[!grepl("ENAR 2014 | SPRING MEETING", df$raw, fixed=TRUE), ]

df = df[!grepl("* = Presenter", df$raw, fixed=TRUE), ]


wide = reshape(df, direction="wide", v.names="raw", timevar="visit")
wide$whole = apply(wide[,2:ncol(wide)], 1, function(x) {
  x = x[!is.na(x)]
  paste(x, sep="", collapse=" ")
})
wide = wide[, c("id", "whole")]


end = merge(wide, id, all.x= TRUE, sort=FALSE)

end = merge(end, sess, all.x=TRUE, sort=FALSE, by="sess")

for (irow in seq(nrow(end))){
  end$whole = gsub(end$sessname[irow], "", end$whole, fixed=TRUE)
}

end$session = NULL
end$whole = str_trim(tolower(end$whole))
end$sessname = str_trim(tolower(end$sessname))
Encoding(end$whole) <- "latin1"  # (just to make sure)
end$whole = iconv(end$whole, "latin1", "ASCII", sub="")

Encoding(end$sessname) <- "latin1"  # (just to make sure)
end$sessname = iconv(end$sessname, "latin1", "ASCII", sub="")

# end = end[end$whole != "floor discussion",]
end$id = NULL

posters = posters[, c("sess", "raw", "time", "where", "day", "sessname")]
colnames(posters) = c("sess", "whole", "time", "where", "day", "sessname")

end = rbind(posters, end)

end$tt = as.numeric(gsub(":", "", end$time))
end = end[order(end$sess, end$tt),]
end$tt = NULL

save(end, file=file.path(dirname(homedir), "ENAR_2014.Rda"))
save(end, file=file.path("~/Dropbox/ShinyApps/ENAR_2014/ENAR_2014.Rda"), ascii=TRUE)

search = "hopkins"
search = str_trim(tolower(search))
end[grep(search, end$whole), c("whole", "sessname", "day")]
