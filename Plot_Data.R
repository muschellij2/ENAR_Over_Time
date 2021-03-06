#######################################
# ENAR Scrape
#######################################
rm(list=ls())
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

abs.tab = function(x, min.n = 0){
  tab = table(word=x)
  tab = as.data.frame(tab, responseName = "nword")
  tab = tab[ !(tab$word %in% ""),]
  tab = tab[ order(tab$nword, decreasing=TRUE), ]
  tab = tab[tab$nword > min.n,]
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

top30 = dftab(data, min.n=0, change.u=FALSE)
top30 = top30[ 50:1, ]
top30 = merge(top30, data, by="affil", all.x=TRUE, sort=FALSE)
top30$affil = factor(top30$affil, levels= unique(top30$affil))
# top30 = top30[ order(top30$affil, top30$year, decreasing=TRUE), ]
top30 = ddply(top30, .(affil, year), function(x) c(n= nrow(x)))
long30 = top30
top30 = dcast(data=top30, formula= affil ~ year, value.var="n")
top30$affil = gsub("university", "u", top30$affil)
top30[is.na(top30)] = 0
# top30 = top30[, rev(colnames(top30))]
top30$affil[which(nchar(top30$affil) > 35)]
long30 = melt(top30, variable.name= "year", value.name = "n")

colnames(top30)[2:ncol(top30)] = paste0("year.", colnames(top30)[2:ncol(top30)])

# top30$affil = revalue(top30$affil, c(
#   "national institute of environmental health sciences" = "niehs",
#   "national institute of child health and human development" = "nihchd",
#   "johns hopkins bloomberg school of public health" = "johns hopkins sph",
#   "memorial sloan kettering cancer center" = "memorial sloan kettering",
#   "st jude children's research hospital" = "st jude's",
#   "fred hutchinson cancer research center" = "fred hutchinson")
# )
write.csv(top30, file=file.path(pub.dir, "top.csv"), row.names=FALSE)

ryear = range(as.numeric(as.character(long30$year)))
long30$year = paste0(long30$year, "0101")

# long30 = dlply(long30, .(year), function(x){
years = unique(long30$year)
my.list = vector(mode="list", length=length(years))
iyear = 1

for (iyear in seq_along(years)){
  xx = x = long30[ long30$year == years[iyear], ]
  # words = vector(mode="list", length=nrow(x))
  # for (irow in seq(nrow(x))) {
  #   words[irow] = c(label=x$affil[irow], score=x$n[irow])
  # }
  # names(words) = NULL
  x = x[, c("affil", "n")]
  modified <- list(
    words = unname(apply(x, 1, function(xx) {
      xx = t(xx)
      colnames(xx) = c("label", "score")
      xx = as.data.frame(xx)
      xx[,"score"] = as.numeric(xx[,"score"])
      # xx$tfidf = xx$score
      xx
    }))
  )
  my.list[[iyear]] = c(date=xx$year[1], modified)
}

ryear = paste0(ryear, "0101")
json = toJSON(my.list, pretty=TRUE)
cat(json, file=file.path(pub.dir, "enar_test", "testdata.json"))


my.list = list(datemin= ryear[1], data=my.list, datemax = ryear[2])
json = toJSON(my.list, pretty=TRUE)
cat(json, file=file.path(pub.dir, "enar_test", "testdata.json"))



data$username = str_trim(tolower(data$username))
data$domain = gsub("(.*)@(.*)", "\\2", data$username)
data$end = gsub("(.*)\\.(.*)$", "\\2", data$domain)
data$end = gsub("??", "", data$end)
data$domain = gsub("(.*)\\.(.*)$", "\\1", data$domain)
# 
# data$affil[ grepl("new york at buffalo", data$affil) ] = 
#   "SUNY at buffalo"


uaffil = unique(data$affil)
### subset over 1 per year
xtab = tab = dftab(data, min.n=nyears, change.u=FALSE)
tab$nall = NULL


coll = ddply(data, .(affil, year), summarise,
             npeople = length(Talk_Title))
####### this is to subset the data
coll = merge(coll, tab, all.y=TRUE)
coll = coll[ order(coll$affil, coll$year), ]
coll = rename(coll, c("npeople" = "value"))

#### switch to wide to get all years, then switch back so that missing 
### years are set to 0
coll = melt(coll,id=c("affil", "year"), na.rm=FALSE)
wide = dcast(coll, formula = affil  ~ year + variable)
wide[is.na(wide)] = 0
coll = melt(wide, id="affil")
coll$variable = as.numeric(gsub("_value","", coll$variable))
coll = rename(coll, c("variable"="year"))
coll = coll[ order(coll$affil, coll$year), ]

coll = merge(coll, location, all.x=TRUE)
coll$location = factor(coll$location)
## make a line plot - eh
g = ggplot(coll, aes(x = year, y = value, color=affil)) + 
  geom_line() +
  guides(color=FALSE)

## bar -eh
gbar = ggplot(data=coll, aes(x=year, y=value, fill=affil)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  guides(fill=FALSE)


#### plotting different wordclouds for affiliations
pdfname = file.path(homedir, 
                    paste0("Affiliations_Over", 
                           nyears, "_wordcloud.pdf"))
pdf(pdfname, width=10, height=10)
### had to have at least 1 per year
wtab = dftab(data, min.n=nyears, change.u=TRUE)

  wordcloud(words = wtab$affil, 
            freq = wtab$nall, random.order=FALSE)
  title(main=paste0("Frequency of Affiliations for Years ", 
                    ryear[1], " to ", ryear[2]))

### had to have at least 1 per year
wtab = dftab(data, min.n=1, change.u=TRUE)

  wordcloud(words = wtab$affil, 
            freq = wtab$nall, random.order=FALSE)
  title(main=paste0("Frequency of Affiliations for Years ", 
                    ryear[1], " to ", ryear[2]))  
dev.off()


cloud = function(wtab, year){
  wordcloud(words = wtab$affil, 
            freq = wtab$nall, random.order=FALSE)
  title(main=paste0("Frequency of Affiliations for Year: ", 
                    year)) 
}

pdfname = file.path(homedir, 
                    paste0("Affiliation_Each_Year_wordcloud.pdf"))
pdf(pdfname, width=10, height=10)

wtabs = dlply(data, .(year), dftab,
              min.n=1, change.u=TRUE)

mapply(cloud, wtabs, names(wtabs))

wtabs = dlply(data, .(year), dftab,
              min.n=1, change.u=FALSE)

mapply(cloud, wtabs, names(wtabs))

dev.off()


#######################
# Abstracts
####################
data$abs = data$abstract
data$abs = gsub("-", " ", data$abs)
data$abs = removePunctuation(data$abs)
data$abs = stripWhitespace(data$abs)
ss = strsplit(data$abs, " ")
for (irow in seq(nrow(data))){
  ss[[irow]] = cbind(abs=ss[[irow]], year=data$year[irow])
}
word.abs = do.call("rbind", ss)
word.abs = data.frame(word.abs, stringsAsFactors=FALSE)
word.abs$year = as.numeric(word.abs$year)

word.abs$abs = gsub("????s", "", word.abs$abs)
word.abs$abs = gsub("??", "", word.abs$abs)
word.abs$abs = gsub("??s", "", word.abs$abs)
word.abs$abs = gsub("??", "", word.abs$abs)
word.abs$abs = gsub("??", "", word.abs$abs)
word.abs$abs = gsub("??", "-", word.abs$abs)
word.abs$abs = gsub("??", "", word.abs$abs)
word.abs$abs = gsub("??", "micro ", word.abs$abs)
word.abs$abs = gsub("??", "", word.abs$abs)
word.abs$abs = gsub("??", "", word.abs$abs)
word.abs$abs = gsub("??", "", word.abs$abs)
word.abs$abs = gsub("??", "fl", word.abs$abs)

word.abs$abs = removeWords2(word.abs$abs, 
                       words=c(tm::stopwords("english"), 
                        "using", 
                        "can", 
                        "may",
                        "used",
                        "use",
                        "will", 
                        "however",
                        "often", 
                        "also",
                        "many",
                        "among",
                        "non",
                        "eg",
                        "thus",
                        "therefore",
                        "might",
                        "ie"
                        ))
word.abs$abs = removeNumbers(word.abs$abs)

word.abs$abs = str_trim(word.abs$abs)

word.abs = word.abs[ !is.na(word.abs$abs), ]
word.abs = word.abs[ word.abs$abs != "", ]
xwords = word.abs

### top takes a little while
word.abs = xwords
trunc = function(data, x, ending){
  data[ data == paste0(x, ending) ] = x
  return(data)
}

quick.replace = function(data, x, replacement){
  data[ data == x ] = replacement
  return(data)
}
#### replacing stuff
word.abs$abs = revalue(word.abs$abs,
  c(studies = "study",
    analyses = "analysis",
    approaches = "approach",
    statistical = "statistic"
    )
  )

### strip trailing s's - could use gsub but not enough finesse
ses = c("model", "method", "propose", "effect", "statistic",
  "simulation", "number", "compare", "test", "set", "size",
  "time", "multiple", "sample", "consider", "function",
  "parameter", "estimate", "variable", "show", "trial", 
  "outcome", "covariate", "rate", "gene", "population", 
  "power", "present", "paper", "error", "performance", 
  "association", "correlation", "design", "disease", "group",
  "subgroup", "provide", "estimator", "patient", "genetic",
  "genomic", "example", "huber", "fisher", "framework",
  "result", "treatment", "problem", "response", "papers",
  "cancer", "illustrate", "likelihood", "develop", "analyze",
  "distribution", "structure", "researcher", "collaborate",
  "level", "point", "known", "exposure", "individual", "ratio",
  "prior", "normal", "procedure", "mixture", "dose", "measure",
  "work", "orders", "subject", "dose", "variance", "type",
  "evaluate", "algorithm", "case", "allow", "event",
  "measurement", "component", "assumption", "alternative",
  "include", "predictor", "application", "score",
  "network", "weight", "factor", "value", "mean", 
  "demonstrate", "dimension", "inference", "standard",
  "curve", "space", "characteristic", "condition", "snp",
  'setting', "pattern", "experiment", "hazard", "cost",
  "term", "coefficient", "scale", "interval", "observation",
  "interaction", "variant", "marginal", "change",
  "stage", "practice", "region", "marker", "form", "brain",
  "technique", "derive", "feature", "cluster", "genome", 
  "drug", "direction", "variation", "depend", "survey", 
  "situation", "quantile", "incorporate", "equation",
  "rank", "dataset", "inverse", "key", "risk", "situation",
  "forest", "tree", "location", "limit", "adjust", "difference",
  "year", "average", "finding", "pair", "shape", "area", "total", 
  "involve", "age", "relationship", "trait", "motivate", 
  "site", "advantage", "indicate", "scenario", "proteomic",
  "proteome", "answer", "respond", "hospital", "metric", 
  "answer", "aspect", "conclusion", "project", "tumor", 
  "yield", "way", "suggest", "original", "dynamic",
  "produce", "graph", "biomarker", "disorder", "field",
  "chip", "decision", "perform", "subset", "pathway", 
  "signal", "make", "strength", "purpose", "metabolite", 
  "biologic", "participant", "unknown", "implication", 
  "exist", "window", "play", "combination", "produce",
  "polymorphism", "mediator", "one", "follow", "constraint",
  "transformation", "subtype", "replace", "challenge", "issue",
  "use", "profile", "odd", "tool", "phenotype", "cell", "state",
  "require", "endpoint", "comparison", "question", "system", "unit",
  "lead", "square", "increase", "take", "image", "spline", 
  "rule", "thousand", "profile", "represent", "mechanism",
  "degree", "transition", "probe", "generalize"
  )
for (iword in ses) {
  word.abs$abs = trunc(word.abs$abs, iword, ending="s")
}

### ending d's
des = c("propose", "compare", "size", "sample", "observe",
  "provide", "estimate", "disease", "base", "analyze", "relate", 
  "correlate", "generalize", "evaluate", 
  "randomize", "measure", "illustrate", "dose", "include",
  "demonstrate", "change", "practice", "derive", "simulate", 
  "define", "involve", "motivate", "indicate", "regulate",
  "increase", "decrease", "induce", "introduce", 
  "calculate", "square", "complicate", "associate", "generate",
  "improve", "desire")
for (iword in des) {
  word.abs$abs = trunc(word.abs$abs, iword, ending="d")
}

### changing around gerunds
ing = c("model", "compare", "size", "sample", "observe",
  "provide", "estimate", "research", "standardize", "present",
  "process", "interest", "design", "estimate", "test", "analyze",
  "illustrate", "respond", "perform", "number", "regress", "study",
  "transition", "rate", "power", "type", "develop", "propose",
  "affect", "effect", "control", "perform", "select", "result",
  "consider", "show", "approach", "function", "number", "censor",
  "randomize", "include", "exist", "talk", "change", "cluster",
  "lead", "adjust", "increase", "motivate", "neighbor", 
  "respond", "demonstrate", "vary", "limit", "identify",
  "incorporate", "weight", "account", "monitor", "count", 
  "require", "image", "rest")
ing.rep = gsub("(.*)e$", "\\1", ing)
ing.rep = paste0(ing.rep, "ing")
for (iword in seq_along(ing)) {
  word.abs$abs = quick.replace(word.abs$abs, 
    ing.rep[iword], 
    replacement=ing[iword])
}

### ed endings
eds = c("present", "consider", "process", "form", "approach",
  "develop", "censor", "predict", "weight", "discuss", "select",
  "model", "conduct", "match", "repeat", "perform", "adjust",
  "cluster", "interest", "assess", "accomplish", "respond",
  "implement", "collect", "implement", "bias", "fix", "focus", 
  "link", "express", "mix", "employ", "test")
for (iword in eds) {
  word.abs$abs = trunc(word.abs$abs, iword, ending="ed")
}

### ed endings
ess = c("bias", "process", "fix", "focus", "mix", "class")
for (iword in ess) {
  word.abs$abs = trunc(word.abs$abs, iword, ending="es")
}

### abnormal changes
reps = c("studies", "approaches", "modelling", 
  "spatially", "controlling", "probabilities", 
  "bayesian", "selection", "prediction", "properties",
  "datum", "importance", "applied", "matrices", 
  "dimensional",  "hypotheses", "sequencing", "classfication",
  "classified", "predictive", "widely", "controlled",
  "increasingly", "decreasingly", "randomization", "aggregation",
  "characterization", "dependencies", "segmentation", 
  "opportunities", "verification", "verified", "fitting",
  "efficiently", "dimensionality", "extremely", "adequately",
  "applies", "evolving", "planned", "analytically", "mining",
  "separation", "frequently", "asymptotically", "carried",
  "locally", "strategies")
to = c("study", "approach", "model", 
  "spatial", "control", "probability", "bayes",
  "select", "predict", "property", "data", "important",
  "apply", "matrix", "dimension", "hypothesis",
  "sequence", "classify", "classify", 
  "predict", "wide", "control", "increase", "decrease", 
  "randomize", "aggregate", "characterize", 
  "dependency", "segment", "opportunity", "verify", "verify",
  "fit", "efficient", "dimension", "extreme", "adequate", 
  "apply", "evolve", "plan", "analytic", "mine", "separate",
  "frequent", "asymptotic", "carry", "local", "strategy")
x = cbind(reps, to)
for (iword in seq_along(to)) {
  word.abs$abs = quick.replace(word.abs$abs, 
    reps[iword], 
    replacement=to[iword])
}

tab = abs.tab(word.abs$abs, min.n=100)
x =showNonASCII(names(tab))

head(tab, 300)

tab = abs.tab(word.abs$abs, min.n=0)

#### plotting different wordclouds for affiliations
pdfname = file.path(homedir, 
                    paste0("Abstracts_Over", 
                           nyears, "_wordcloud.pdf"))
pdf(pdfname, width=10, height=10)

wtab = tab[ tab$nword > 100, ]
  wordcloud(words = wtab$word, 
            freq = wtab$nword, random.order=FALSE)
  title(main=paste0("Frequency of Abstract Words for Years ", 
                    ryear[1], " to ", ryear[2]))

wtab = tab[ tab$nword > 50, ]
  wordcloud(words = wtab$word, 
            freq = wtab$nword, random.order=FALSE)
  title(main=paste0("Frequency of Abstract Words for Years ", 
                    ryear[1], " to ", ryear[2]))

wtab = tab[ tab$nword > nyears, ]
  wordcloud(words = wtab$word, 
            freq = wtab$nword, random.order=FALSE)
  title(main=paste0("Frequency of Abstract Words for Years  ", 
                    ryear[1], " to ", ryear[2]))  
dev.off()


cloud = function(wtab, year){
  wordcloud(words = wtab$word, 
            freq = wtab$nword, random.order=FALSE)
  title(main=paste0("Frequency of Abstract Words for Year: ", 
                    year)) 
}

pdfname = file.path(homedir, 
                    paste0("Abstracts_Each_Year_wordcloud.pdf"))
pdf(pdfname, width=10, height=10)

wtabs = dlply(word.abs, .(year), function(x) {
  # print(unique(x$year))
  tab = abs.tab(x$abs, min.n=10)
  rownames(tab) = NULL
  tab
})

mapply(cloud, wtabs, names(wtabs))

wtabs = dlply(word.abs, .(year), function(x) {
  # print(unique(x$year))
  tab = abs.tab(x$abs, min.n=nyears)
  rownames(tab) = NULL
  tab
})

mapply(cloud, wtabs, names(wtabs))

dev.off()


