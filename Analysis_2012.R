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

dat = read.csv(file.path(homedir, "2k12 Full Dump 11.01.2012.csv"), na.strings="")
cn = colnames(dat)
pres = grep("^presenter.*ation", cn, value=TRUE)
data = dat[, c("UserID", "Talk_Title", "FirstCategory", "SecondCategory", 
              "category", "abstract", "presenter1affiliation")]



head(data)

data$presenter1affiliation = tolower(data$presenter1affiliation)
### remove double spaces
data$presenter1affiliation = gsub(" +", " ", data$presenter1affiliation)


#### take out dept
data$presenter1affiliation = gsub("dep(artmen|)t of biostatistics & (bioinformatics|medical informatics)(,|)", 
                                  "", 
                                  data$presenter1affiliation)

data$presenter1affiliation = gsub("dep(artmen|)t of biostatistics(,|)", 
                                  "", 
                            data$presenter1affiliation)

data$presenter1affiliation = gsub("dep(artmen|)t of statistics(,|)", 
                                  "", 
                                  data$presenter1affiliation)

data$presenter1affiliation = gsub("dep(artmen|)t of epidemiology and biostatistics(,|)", 
                                  "", 
                                  data$presenter1affiliation)
data$presenter1affiliation = gsub("\n", 
                                  " ", 
                                  data$presenter1affiliation)
data$presenter1affiliation = gsub("\n", 
                                  " ", 
                                  data$presenter1affiliation)

data$presenter1affiliation = gsub("^(bio|)statistics department", 
                                  "", 
                                  data$presenter1affiliation)
data$presenter1affiliation = str_trim(data$presenter1affiliation)
data$presenter1affiliation = gsub(",$", "", data$presenter1affiliation)
data$presenter1affiliation = gsub("^,", "", data$presenter1affiliation)
data$presenter1affiliation = gsub("^the", "", data$presenter1affiliation)
data$presenter1affiliation = gsub(" +", " ", data$presenter1affiliation)
data$presenter1affiliation = str_trim(data$presenter1affiliation)

### replace UW 
uwm = grepl("wisconsin|uw", data$presenter1affiliation) & 
  grepl("madison", data$presenter1affiliation)
uwm = uwm | 
  grepl("university of wisconsin", data$presenter1affiliation)
print(unique(data$presenter1affiliation[uwm]))
data$presenter1affiliation[uwm] = "university of wisconsin"

### replace UPenn
upenn = grepl("university of penn", data$presenter1affiliation) |
  grepl("upenn", data$presenter1affiliation) & 
  !grepl("indiana", data$presenter1affiliation)
print(unique(data$presenter1affiliation[upenn]))
data$presenter1affiliation[upenn] = "university of pennsylvania"

# university of minnesota
uminn = grepl("university of minnesota", data$presenter1affiliation)
print(unique(data$presenter1affiliation[uminn]))
data$presenter1affiliation[uminn] = "university of minnesota"

# emory
emo = grepl("emory", data$presenter1affiliation)
print(unique(data$presenter1affiliation[emo]))
data$presenter1affiliation[emo] = "emory university"

# columbia
colum = grepl("columbia uni", data$presenter1affiliation)
print(unique(data$presenter1affiliation[colum]))
data$presenter1affiliation[colum] = "columbia university"

# ariz
ariz = grepl("arizona state", data$presenter1affiliation)
print(unique(data$presenter1affiliation[ariz]))
data$presenter1affiliation[ariz] = "arizona state university"

# jhu - bloom
jhub = grepl("johns hopkins", data$presenter1affiliation) & 
  grepl("public health", data$presenter1affiliation) & 
  !grepl("kennedy", data$presenter1affiliation)
print(unique(data$presenter1affiliation[jhub]))
data$presenter1affiliation[jhub] = "johns hopkins bloomberg school of public health"

# jhu 
jhu = grepl("johns hopkins", data$presenter1affiliation) & 
  !grepl("public health", data$presenter1affiliation) & 
  !grepl("kennedy", data$presenter1affiliation)
print(unique(data$presenter1affiliation[jhu]))
data$presenter1affiliation[jhu] = "johns hopkins university"


# unc
unc = grepl("unc", data$presenter1affiliation) |
  grepl("chapel hill", data$presenter1affiliation) | 
  grepl("university of north carolina", data$presenter1affiliation)
print(unique(data$presenter1affiliation[unc]))
data$presenter1affiliation[unc] = "unc"

#university of south carolina
usc = grepl("university of south carolina", 
            data$presenter1affiliation)
print(unique(data$presenter1affiliation[usc]))
data$presenter1affiliation[usc] = "university of south carolina"

#university of waterloo
water = grepl("waterloo", 
            data$presenter1affiliation)
print(unique(data$presenter1affiliation[water]))
data$presenter1affiliation[water] = "university of waterloo"

#university of michigan
umich = grepl("of michigan", 
              data$presenter1affiliation)
print(unique(data$presenter1affiliation[umich]))
data$presenter1affiliation[umich] = "university of michigan"

# mich state
mstate = grepl("michigan state", 
              data$presenter1affiliation)
print(unique(data$presenter1affiliation[mstate]))
data$presenter1affiliation[mstate] = "michigan state university"


# ucla
ucla = grepl("ucla", 
               data$presenter1affiliation) |
  grepl("los angeles", data$presenter1affiliation)
print(unique(data$presenter1affiliation[ucla]))
data$presenter1affiliation[ucla] = "university of california, los angeles"

# missou
missou = grepl("missou", 
             data$presenter1affiliation) 
print(unique(data$presenter1affiliation[missou]))
data$presenter1affiliation[missou] = "university of missouri"

# vanderbilt
vandy = grepl("vanderbilt", 
               data$presenter1affiliation) 
print(unique(data$presenter1affiliation[vandy]))
data$presenter1affiliation[vandy] = "vanderbilt university"

# yale
yale = grepl("yale", 
              data$presenter1affiliation) 
print(unique(data$presenter1affiliation[yale]))
data$presenter1affiliation[yale] = "yale university"

# massach
umass = grepl("massach", 
             data$presenter1affiliation)|
  grepl("amherst", data$presenter1affiliation)
print(unique(data$presenter1affiliation[umass]))
data$presenter1affiliation[umass] = "university of massachusetts"

# eunice kennedy
ekenn = grepl("eunice", 
              data$presenter1affiliation) |
  grepl("kennedy shriver", data$presenter1affiliation)
ekenn = ekenn & !grepl("hopkins", data$presenter1affiliation)
print(unique(data$presenter1affiliation[ekenn]))
data$presenter1affiliation[ekenn] = 
  "eunice kennedy shriver national institute of child health and human development"


# cornell
corn = grepl("cornell", 
              data$presenter1affiliation)
print(unique(data$presenter1affiliation[corn]))
data$presenter1affiliation[corn] = 
  "cornell university"

# wayne state university 
wayne = grepl("wayne", 
             data$presenter1affiliation)
print(unique(data$presenter1affiliation[wayne]))
data$presenter1affiliation[wayne] = 
  "wayne state university"

# texas a&m university
am = grepl("a&m", 
              data$presenter1affiliation)
print(unique(data$presenter1affiliation[am]))
data$presenter1affiliation[am] = 
  "texas a&m university"


# texas a&m university
tex = grepl("of texas", 
           data$presenter1affiliation) & !am 
print(unique(data$presenter1affiliation[tex]))
data$presenter1affiliation[tex] = 
  "university of texas"

# pittsburg
pitt = grepl("pitt", 
            data$presenter1affiliation) |
  grepl("upmc", data$presenter1affiliation)
print(unique(data$presenter1affiliation[pitt]))
data$presenter1affiliation[pitt] = 
  "university of pittsburg"


# BU - do before harvard
bu = grepl("boston university", 
              data$presenter1affiliation)
print(unique(data$presenter1affiliation[bu]))
data$presenter1affiliation[bu] = 
  "boston university"

# harvard
hvpub = grepl("harvard", 
             data$presenter1affiliation) &
  grepl("public health", data$presenter1affiliation)
print(unique(data$presenter1affiliation[hvpub]))
data$presenter1affiliation[hvpub] = 
  "harvard school of public health"

# harvard
hvpub = grepl("harvard", 
              data$presenter1affiliation) &
  grepl("public health", data$presenter1affiliation)
print(unique(data$presenter1affiliation[hvpub]))
data$presenter1affiliation[hvpub] = 
  "harvard school of public health"

x = sort(table(data$presenter1affiliation))
uaffil = names(x)





##### get the affiliation data
pres = dat[, c("UserID", pres)]
pres = melt(pres, id.vars="UserID", variable.name="presenter", value.name="Affiliation")
pres$presenter = gsub("presenter", "", pres$presenter)
pres$presenter = gsub("affiliation", "", pres$presenter)
pres$presenter = as.numeric(pres$presenter)
pres = pres[ order(pres$UserID, pres$presenter),]
pres = pres[ !is.na(pres$Affiliation), ]
pres$Affiliation = str_trim(pres$Affiliation)


### could take first presenter
pres = pres[ pres$presenter ==1 ,]
######## need to clean this but we can do affilation here
affil.df = as.data.frame(table(Affiliation=pres$Affiliation))

affil.df = affil.df[ order(affil.df$Affiliation), ]
# affil.df = affil.df[ affil.df$Freq > 5,]

pdf(file.path(homedir, "2012_Affilations.pdf"), height=5, width=10)


wordcloud(affil.df$Affiliation, affil.df$Freq, 
          min.freq=5, random.order=FALSE, 
          scale=c(2, 0.5), 
          fixed.asp = FALSE, rot.per=0)

wordcloud(affil.df$Affiliation, affil.df$Freq, 
          min.freq=5, random.order=FALSE, 
          scale=c(2, 0.5))
dev.off()

table(table(dat$UserID))



