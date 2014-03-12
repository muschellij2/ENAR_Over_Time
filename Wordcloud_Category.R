#######################################
# ENAR Scrape
#######################################
rm(list=ls())

wordcloud2 = function (words, freq, scale = c(4, 0.2), min.freq = 1, max.words = Inf, 
                       random.order = TRUE, random.color = FALSE, rot.per = 0.1, 
                       colors = "black", ordered.colors = FALSE, use.r.layout = FALSE, 
                       fixed.asp = TRUE, ...) 
{
  if (!fixed.asp && rot.per > 0) 
    stop("Variable aspect ratio not supported for rotated words. Set rot.per=0.")
  tails <- "g|j|p|q|y"
  last <- 1
  nc <- length(colors)
  if (missing(freq)) {
    if (!require("tm")) 
      stop("freq must either be non-missing, or the tm package must be available")
    if (is.character(words) || is.factor(words)) {
      corpus <- Corpus(VectorSource(words))
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, function(x) removeWords(x, 
                                                       stopwords()))
    }
    else corpus <- words
    tdm <- TermDocumentMatrix(corpus)
    freq <- slam::row_sums(tdm)
    words <- names(freq)
  }
  if (ordered.colors) {
    if (length(colors) != 1 && length(colors) != length(words)) {
      stop(paste("Length of colors does not match length of words", 
                 "vector"))
    }
  }
  if (min.freq > max(freq)) 
    min.freq <- 0
  overlap <- function(x1, y1, sw1, sh1) {
    if (!use.r.layout) 
      return(wordcloud:::.overlap(x1, y1, sw1, sh1, boxes))
    s <- 0
    if (length(boxes) == 0) 
      return(FALSE)
    for (i in c(last, 1:length(boxes))) {
      bnds <- boxes[[i]]
      x2 <- bnds[1]
      y2 <- bnds[2]
      sw2 <- bnds[3]
      sh2 <- bnds[4]
      if (x1 < x2) 
        overlap <- x1 + sw1 > x2 - s
      else overlap <- x2 + sw2 > x1 - s
      if (y1 < y2) 
        overlap <- overlap && (y1 + sh1 > y2 - s)
      else overlap <- overlap && (y2 + sh2 > y1 - s)
      if (overlap) {
        last <<- i
        return(TRUE)
      }
    }
    FALSE
  }
  ord <- rank(-freq, ties.method = "random")
  words <- words[ord <= max.words]
  freq <- freq[ord <= max.words]
  if (ordered.colors) {
    colors <- colors[ord <= max.words]
  }
  if (random.order) 
    ord <- sample.int(length(words))
  else ord <- order(freq, decreasing = TRUE)
  #   words <- words[ord]
  #   freq <- freq[ord]
  words <- words[freq >= min.freq]
  freq <- freq[freq >= min.freq]
  if (ordered.colors) {
    colors <- colors[ord][freq >= min.freq]
  }
  thetaStep <- 0.1
  rStep <- 0.05
  plot.new()
  op <- par("mar")
  par(mar = c(0, 0, 2, 0))
  if (fixed.asp) 
    plot.window(c(0, 1), c(0, 1), asp = 1)
  else plot.window(c(0, 1), c(0, 1))
  normedFreq <- freq/max(freq)
  size <- (scale[1] - scale[2]) * normedFreq + scale[2]
#   size = rep(1, length(freq))
  boxes <- list()
  for (i in 1:length(words)) {
    rotWord <- runif(1) < rot.per
    r <- 0
    theta <- runif(1, 0, 2 * pi)
    x1 <- 0.5
    y1 <- 0.5
    wid <- strwidth(words[i], cex = size[i], ...)
    ht <- strheight(words[i], cex = size[i], ...)
    if (grepl(tails, words[i])) 
      ht <- ht + ht * 0.2
    if (rotWord) {
      tmp <- ht
      ht <- wid
      wid <- tmp
    }
    isOverlaped <- TRUE
    while (isOverlaped) {
      if (!overlap(x1 - 0.5 * wid, y1 - 0.5 * ht, wid, 
                   ht) && x1 - 0.5 * wid > 0 && y1 - 0.5 * ht > 
            0 && x1 + 0.5 * wid < 1 && y1 + 0.5 * ht < 1) {
        if (!random.color) {
          if (ordered.colors) {
            cc <- colors[i]
          }
          else {
            cc <- ceiling(nc * normedFreq[i])
            cc <- colors[cc]
          }
        }
        else {
          cc <- colors[sample(1:nc, 1)]
        }
        text(x1, y1, words[i], cex = size[i], offset = 0, 
             srt = rotWord * 90, col = cc, ...)
        boxes[[length(boxes) + 1]] <- c(x1 - 0.5 * wid, 
                                        y1 - 0.5 * ht, wid, ht)
        isOverlaped <- FALSE
      }
      else {
        if (r > sqrt(0.5)) {
          warning(paste(words[i], "could not be fit on page. It will not be plotted."))
          isOverlaped <- FALSE
        }
        theta <- theta + thetaStep
        r <- r + rStep * thetaStep/(2 * pi)
        x1 <- 0.5 + r * cos(theta)
        y1 <- 0.5 + r * sin(theta)
      }
    }
  }
  par(mar = op)
  invisible()
}


library(stringr)
library(tm)
library(stringr)
library(zoo)
library(plyr)
library(wordcloud)
library(reshape2)
library(tools)
library(ggplot2)
library(RJSONIO)
wmhome= Sys.getenv("WNHOME")
if (wmhome == "") {
  Sys.setenv(WNHOME="/opt/local/bin")
}
library(wordnet)
homedir = "~/Dropbox/database"
homedir = path.expand(homedir)
pub.dir = "~/Dropbox/Public"
options(stringsAsFactors=FALSE)

removeWords2 = function (x, words) {
  gsub(sprintf("(*UCP)\\b(%s)\\b", paste(words, collapse = "|")), 
       " ", x, perl = TRUE)
}

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

# abs.tab = function(x, min.n = 0){
#   tab = table(word=x)
#   tab = as.data.frame(tab, responseName = "nword")
#   tab = tab[ !(tab$word %in% ""),]
# #   tab = tab[ order(tab$nword, decreasing=TRUE), ]
#   tab = tab[ order(tab$word, decreasing=TRUE), ]
#   tab = tab[tab$nword > min.n,]
#   return(tab)
# }


abs.tab = function(x, top=36, min.n = 0){
  tab = table(word=x)
  tab = as.data.frame(tab, responseName = "nword")
  tab = tab[ !(tab$word %in% ""),]
  #   tab = tab[ order(tab$nword, decreasing=TRUE), ]
  tab = tab[ order(tab$word, decreasing=TRUE), ]
#   tab$nword = .2
  tab = tab[seq(top),]
#   tab = tab[tab$nword > min.n,]
  return(tab)
}


abs.tab2 = function(x, y, min.n = 0){
  tab = table(word=x, year=y)
  tab = as.data.frame(tab, responseName = "nword")
  tab = tab[ !(tab$word %in% ""),]
  tab = tab[ order(tab$nword, decreasing=TRUE), ]
  tab = tab[tab$nword > min.n,]
  return(tab)
}

dftab = function(data, min.n = 0, change.u = FALSE){
  tab = table(affil=data$affil)
  tab = as.data.frame(tab, responseName = "nall")
  tab = tab[ !(tab$affil %in% ""),]
  tab = tab[ order(tab$nall, decreasing=TRUE), ]
  tab = tab[tab$nall > min.n,]
  if(change.u){
    tab$affil = gsub("university", "u", tab$affil)
  }
  return(tab)
}

load(file=file.path(homedir,"Clean_Collapsed_2007_2013.Rda"))
nyears = length(unique(data$year))
ryear = range(data$year)
#### rename columns
data = rename(data, c("presenter1affiliation" = "affil"))
data = data[ !(data$affil %in% ""),]
data = data[ order(data$affil, data$year), ]

data$FirstCategory = str_trim(tolower(data$FirstCategory))
data$FirstCategory = revalue(data$FirstCategory, 
        c("adaptive design/adaptive randomization"="adaptive design", 
          "biologics, pharmaceuticals, medical devices"="bio, pharma, med devices",
          "constrained estimation/order restricted inference"= "constrained estimation", 
          "environmental and ecological applications"="environmental/ecological" ,
          "joint models for longitudinal and survival data"="longitudinal/survival models",
          "pharmacokinetic/pharmacodynamics (pk/pd) modeling"="pharmacokinetic/pharmacodynamics", 
          "semi-parametric and non-parametric models"="semi-parametric/non-parametric models",
          "variable subset selection/model selection"="model selection",
          "data mining/massive data sets"= "data mining/massive data"))
data$FirstCategory = gsub(" and ", "/", data$FirstCategory)
data$FirstCategory = gsub(" / ", "/", data$FirstCategory)

cloud = function(wtab, year){
  wordcloud2(words = wtab$word, 
            freq = wtab$nword, random.order=FALSE, rot.per=0)
  title(main=paste0("Frequency of Abstract Words for Year: ", 
                    year)) 
}


n = 5
m = 5
df = as.data.frame(table(affil=data$affil))
df = df[order(df$Freq, decreasing=TRUE), ]
df = df[seq(n*m),]

top = data[ data$affil %in% df$affil, ]

top$affil = factor(top$affil, levels=unique(df$affil))

pdfname = file.path(homedir, 
                    paste0("Categories_Each_Year_wordcloud.pdf"))
pdf(pdfname, width=10, height=10)

wtabs = dlply(data, .(year), function(x) {
  # print(unique(x$year))
  tab = abs.tab(x$FirstCategory, min.n=0)
  rownames(tab) = NULL
  tab
})

mapply(cloud, wtabs, names(wtabs))

wtabs = dlply(data, .(year), function(x) {
  # print(unique(x$year))
  tab = abs.tab(x$FirstCategory, min.n=nyears)
  rownames(tab) = NULL
  tab
})

mapply(cloud, wtabs, names(wtabs))

dev.off()



pdfname = file.path(homedir, 
                    paste0("Top_", n*m, "_Cats.pdf"))
pdf(pdfname, width=n*3, height=m*3)
par(mfrow=c(n, m))

affil.tab = table(top$affil, top$FirstCategory)
affil.tab = affil.tab[levels(top$affil),]

ddf = as.data.frame(affil.tab)
# df$affil = factor(rownames(df), levels=rownames(df))

rn = rownames(ddf)
rn = gsub("university", "u", rn)
rn = gsub("school of public health", "sph", rn)
rn = revalue(rn,
             c("national institute of child health and human development"="nichd"))
rownames(ddf) = rn

cn = colnames(ddf)
a_ply(ddf, .margins=1, function(x){
#   print(colnames(x))
  rn = rownames(x)
  words = cn[ x > 0]
  x = x[ x > 0]
  wordcloud2(words = words, 
             freq = x, random.order=FALSE, 
             rot.per=0, scale=c(2, 0.5), fixed.asp=FALSE)
  title(main=paste0(rn)) 
#   cloud(words=words, freq=x)
})
ryear = range(data$year)
# mtext(paste0("Years: ", ryear[1], " - ", ryear[2]), side=3, outer=TRUE, line=-21)

for (iyear in seq(ryear[1], ryear[2])){
  tt = top[ top$year == iyear,]
  affil.tab = table(tt$affil, tt$FirstCategory)
  affil.tab = affil.tab[levels(top$affil),]
  
  ddf = as.data.frame(affil.tab)
  cn = colnames(ddf)
  ddf = cbind(ddf, 1)
  cn = c(cn, "")
  colnames(ddf) = cn  
  # df$affil = factor(rownames(df), levels=rownames(df))
  
  rn = rownames(ddf)
  rn = gsub("university", "u", rn)
  rn = gsub("school of public health", "sph", rn)
  rn = revalue(rn,
               c("national institute of child health and human development"="nichd"),
               warn_missing= FALSE)
  rownames(ddf) = rn
  
  cn = colnames(ddf)
  for (irow in seq(nrow(ddf))){
    x = ddf[irow,]
#   a_ply(ddf, .margins=1, function(x){
    #   print(colnames(x))
    rn = rownames(x)
    words = cn[ x > 0]
    x = x[ x > 0]

    wordcloud2(words = words, 
               freq = x, random.order=FALSE, 
               rot.per=0, scale=c(1.8, 0.5), fixed.asp=FALSE)
    title(main=paste0("\n",rn)) 
    if (irow == 3) {
      title(main=paste0("Year: ", iyear, "\n",rn)) 
    } else {
      title(main=paste0("\n",rn)) 
    }

# mtext(paste0("Year: ", iyear), side=3, outer=FALSE)

    #   cloud(words=words, freq=x)
#   })
  }
#    mtext(paste0("Year: ", iyear), side=3, outer=FALSE)
  
}


dev.off()




