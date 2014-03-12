rm(list=ls())
library(tm)
library(stringr)
library(zoo)
library(plyr)
library(wordcloud)
library(reshape2)
library(tools)
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

load(file=file.path(homedir,"Collapsed_2007_2013.Rda"))

head(data)

data$abstract = gsub("\n", " ", data$abstract)
data$abstract = gsub("\t", " ", data$abstract)
data$abstract = gsub(" +", " ", data$abstract)
data$abstract = tolower(data$abstract)

table(data$category[is.na(data$Talk_Title)], useNA="always")
### keep if they had a talk title
data = data[ !is.na(data$Talk_Title), ]
### if no affiliation 
nas = is.na(data$presenter1affiliation)
data$presenter1affiliation[nas] = data$username[nas]
data$presenter1affiliation[nas][!grepl(".edu", data$presenter1affiliation[nas])] = NA
data$presenter1affiliation[nas] = gsub(".*@(.*)\\.edu", "\\1", data$presenter1affiliation[nas])
data$presenter1affiliation[nas] = gsub(".*\\.(.*)", "\\1", data$presenter1affiliation[nas])
head(data$presenter1affiliation[nas])

data$presenter1affiliation[
  grepl("Ecole Polytechnique", data$presenter1affiliation)] 

data$presenter1affiliation = tolower(data$presenter1affiliation)
### remove double spaces
data$presenter1affiliation = gsub(" +", " ", data$presenter1affiliation)

#### take out dept
data$presenter1affiliation = gsub("dept\\.", 
                                  "department", 
                                  data$presenter1affiliation)
data$presenter1affiliation = gsub("univ\\.", 
                                  "university", 
                                  data$presenter1affiliation)
data$presenter1affiliation = gsub("univ ", 
                                  "university ", 
                                  data$presenter1affiliation)
data$presenter1affiliation = gsub("dep(artmen|)t of biostatistics & (bioinformatics|medical informatics)(,|)", 
                                  "", 
                                  data$presenter1affiliation)

data$presenter1affiliation = gsub("dep(artmen|)t(s|) of biostatistics(,|)", 
                                  "", 
                                  data$presenter1affiliation)

data$presenter1affiliation = gsub("dep(artmen|)t(s|) of statistics(,|)", 
                                  "", 
                                  data$presenter1affiliation)

data$presenter1affiliation = gsub("dep(artmen|)t(s|) of epidemiology and biostatistics(,|)", 
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
data$presenter1affiliation = gsub("dep(artmen|)t(s|) of", 
                                  "", 
                                  data$presenter1affiliation)
data$presenter1affiliation = gsub("division(s|) of", 
                                  "", 
                                  data$presenter1affiliation)
data$presenter1affiliation = gsub("^department biostatistics", 
                                  "", 
                                  data$presenter1affiliation)
data$presenter1affiliation = str_trim(data$presenter1affiliation)
data$presenter1affiliation = gsub(",$", "", data$presenter1affiliation)
data$presenter1affiliation = gsub("^,", "", data$presenter1affiliation)
data$presenter1affiliation = gsub("^the", "", data$presenter1affiliation)
data$presenter1affiliation = gsub(" +", " ", data$presenter1affiliation)
data$presenter1affiliation = str_trim(data$presenter1affiliation)
data$presenter1affiliation = gsub("^u of", 
                                  "university of", 
                                  data$presenter1affiliation)
data$presenter1affiliation = gsub(" u of", 
                                  " university of", 
                                  data$presenter1affiliation)
data$presenter1affiliation = gsub("univerisity|univeristy", 
                                  "university", 
                                  data$presenter1affiliation)
data$presenter1affiliation = gsub("^mathematics (and|&) statistics(,|) ", 
                                  "", 
                                  data$presenter1affiliation)
data$presenter1affiliation = gsub("^mathematical sciences(,|) ", 
                                  "", 
                                  data$presenter1affiliation)
data$presenter1affiliation = gsub("^mathematics, ", 
                                  "", 
                                  data$presenter1affiliation)

############# 
# Site specific replacements
##############
##################
# Academic Inst
##################


### replace MCOW
mcow = grepl("medical college of wisconsin", 
             data$presenter1affiliation) 
## misspell
mcow = mcow | grepl('wiscosnin', data$presenter1affiliation)
print(unique(data$presenter1affiliation[mcow]))
data$presenter1affiliation[mcow] = "medical college of wisconsin"


### replace UWisc
uwm = grepl("wisconsin|uw", data$presenter1affiliation) & 
  grepl("madison", data$presenter1affiliation)
uwm = uwm | 
  grepl("university of wisconsin", data$presenter1affiliation)
## misspell
uwm = uwm | grepl("wisocnsin|wisconsion", data$presenter1affiliation)
print(unique(data$presenter1affiliation[uwm]))
data$presenter1affiliation[uwm] = "university of wisconsin"

### replace UW
uw = grepl("university of washington", data$presenter1affiliation)
print(unique(data$presenter1affiliation[uw]))
data$presenter1affiliation[uw] = "university of washington"



### replace UPenn
upenn = (grepl("university of penn", data$presenter1affiliation) |
  grepl("upenn", data$presenter1affiliation)) & 
  !grepl("indiana", data$presenter1affiliation)
print(unique(data$presenter1affiliation[upenn]))
data$presenter1affiliation[upenn] = "university of pennsylvania"

# university of minnesota
uminn = grepl(" of minnesota", data$presenter1affiliation) |
  (grepl("umn", data$presenter1affiliation) & 
     grepl("twin", data$presenter1affiliation))
print(sort(unique(data$presenter1affiliation[uminn])))
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

# nw 
nw = grepl("northwestern", data$presenter1affiliation) 
print(unique(data$presenter1affiliation[nw]))
data$presenter1affiliation[nw] = "northwestern university"

# uk 
ken = grepl("of kentucky", data$presenter1affiliation) 
print(unique(data$presenter1affiliation[ken]))
data$presenter1affiliation[ken] = "university of kentucky"

# einstein
ein = grepl("einstein", data$presenter1affiliation) 
print(unique(data$presenter1affiliation[ein]))
data$presenter1affiliation[ein] = "albert einstein college of medicine"


# unc
unc = grepl("unc", data$presenter1affiliation) |
  grepl("chapel hill", data$presenter1affiliation) | 
  grepl("university of north carolina", data$presenter1affiliation)
print(unique(data$presenter1affiliation[unc]))
data$presenter1affiliation[unc] = "university of north carolina"

# nc state
ncs = grepl("north carolina state", data$presenter1affiliation) |
  grepl("ncsu", data$presenter1affiliation) | 
  grepl("nc state", data$presenter1affiliation)
print(unique(data$presenter1affiliation[ncs]))
data$presenter1affiliation[ncs] = "north carolina state university"


#medical university of south carolina
musc = grepl("medical university of south carolina", 
         data$presenter1affiliation) 
musc = musc | grepl("musc", data$presenter1affiliation)
print(unique(data$presenter1affiliation[musc]))
data$presenter1affiliation[musc] = "medical university of south carolina"


#university of south carolina
usc = grepl("university of south carolina", 
            data$presenter1affiliation) &
  !musc
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

# mich tech
mtech = grepl("michigan tech", 
              data$presenter1affiliation)
print(unique(data$presenter1affiliation[mtech]))
data$presenter1affiliation[mtech] = "michigan tech university"

# mich state
mstate = grepl("michigan state", 
               data$presenter1affiliation)
print(unique(data$presenter1affiliation[mstate]))
data$presenter1affiliation[mstate] = "michigan state university"

# mcmaster
mcm = grepl("mcmaster", 
               data$presenter1affiliation)
print(unique(data$presenter1affiliation[mcm]))
data$presenter1affiliation[mcm] = "mcmaster university"

# ucla
ucla = grepl("ucla", 
             data$presenter1affiliation) |
  grepl("los angeles", data$presenter1affiliation)
print(unique(data$presenter1affiliation[ucla]))
data$presenter1affiliation[ucla] = "university of california, los angeles"

### berkeley
berk = grepl("berkeley", data$presenter1affiliation)
print(unique(data$presenter1affiliation[berk]))
data$presenter1affiliation[berk] = "university of california, berkeley"

### ucsd
ucsd = (grepl("of california", data$presenter1affiliation) & 
          grepl("diego", data$presenter1affiliation)) | 
  grepl("ucsd", data$presenter1affiliation)
print(unique(data$presenter1affiliation[ucsd]))
data$presenter1affiliation[ucsd] = "university of california, san diego"

### ucsc
ucsc = (grepl("of california", data$presenter1affiliation) & 
          grepl("cruz", data$presenter1affiliation)) | 
  grepl("ucsc", data$presenter1affiliation)
print(unique(data$presenter1affiliation[ucsc]))
data$presenter1affiliation[ucsc] = "university of california, santa cruz"

### ucsf
ucsf = (grepl("of california", data$presenter1affiliation) & 
          grepl("sf|francisco", data$presenter1affiliation)) | 
  grepl("ucsf", data$presenter1affiliation)
print(unique(data$presenter1affiliation[ucsf]))
data$presenter1affiliation[ucsf] = "university of california, san francisco"

### uc irvine
uci = (grepl("of california", data$presenter1affiliation) & 
          grepl("irvine", data$presenter1affiliation)) |
  grepl("uc(-| )irvine", data$presenter1affiliation)
print(unique(data$presenter1affiliation[uci]))
data$presenter1affiliation[uci] = "university of california, irvine"

### uc riverside
ucriv = grepl("riverside", data$presenter1affiliation)
print(unique(data$presenter1affiliation[ucriv]))
data$presenter1affiliation[ucriv] = "university of california, riverside"

### uc davis
dav = grepl("davis", data$presenter1affiliation)
print(unique(data$presenter1affiliation[dav]))
data$presenter1affiliation[dav] = "university california, davis"

uc = grep("university of california$", data$presenter1affiliation)
uc.info = data$presenter1affiliation[uc]
uc.user = data$username[uc]
uc.info[grepl("uci.edu", uc.user)] = "university of california, irvine"
uc.info[grepl("ucla.edu", uc.user)] = "university of california, los angeles"
uc.info[grepl("ucr.edu", uc.user)] = "university of california, riverside"
uc.info[grepl("ucsd.edu", uc.user)] = "university of california, san diego"
uc.info[grepl("ucsf.edu", uc.user)] = "university of california, san francisco"
uc.info[grepl("berkeley", uc.user)] = "university of california, berkeley"
data$presenter1affiliation[uc] = uc.info

# missou
missou = grepl("missou", 
               data$presenter1affiliation) 
print(unique(data$presenter1affiliation[missou]))
data$presenter1affiliation[missou] = "university of missouri"

## missisippi
olemiss = grepl("of mississippi", 
               data$presenter1affiliation) 
print(unique(data$presenter1affiliation[olemiss]))
data$presenter1affiliation[olemiss] = "university of mississippi"

## miami
miami = grepl("of miami", 
                data$presenter1affiliation) 
print(unique(data$presenter1affiliation[miami]))
data$presenter1affiliation[miami] = "university of miami"

## miami u
miamiu = grepl("miami u", 
              data$presenter1affiliation) 
print(unique(data$presenter1affiliation[miamiu]))
data$presenter1affiliation[miamiu] = "miami university"

#memph
mem = grepl("of memphis", 
              data$presenter1affiliation) &
  !grepl("jude", data$presenter1affiliation)
print(unique(data$presenter1affiliation[mem]))
data$presenter1affiliation[mem] = "university of memphis"

#UNLV
unlv = (grepl("of nevada", 
            data$presenter1affiliation)  & 
  grepl("vegas", data$presenter1affiliation)) |
  grepl("unlv", data$presenter1affiliation)
print(unique(data$presenter1affiliation[unlv]))
data$presenter1affiliation[unlv] = "university of nevada, las vegas"


# washu
washu = grepl("washington un", 
              data$presenter1affiliation) &
  !grepl("george", data$presenter1affiliation)
print(unique(data$presenter1affiliation[washu]))
data$presenter1affiliation[washu] = "washington university"


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

# BU - do before harvard and umass
bu = grepl("boston university", 
           data$presenter1affiliation)
print(unique(data$presenter1affiliation[bu]))
data$presenter1affiliation[bu] = 
  "boston university"


# massach
umass = (grepl("mass", 
              data$presenter1affiliation) &
        !grepl("general|harvard", data$presenter1affiliation) ) |
  grepl("amherst", data$presenter1affiliation) | 
  grepl("umass", data$presenter1affiliation)
print(sort(unique(data$presenter1affiliation[umass])))
data$presenter1affiliation[umass] = "university of massachusetts"

# eunice kennedy
ekenn = grepl("eunice", 
              data$presenter1affiliation) |
  grepl("kennedy shriver", data$presenter1affiliation) | 
  grepl("child health and human", data$presenter1affiliation) |
  grepl("nichd", data$presenter1affiliation)
ekenn = ekenn & 
  !grepl("hopkins", data$presenter1affiliation)
  !grepl("university", data$presenter1affiliation)
print(unique(data$presenter1affiliation[ekenn]))
# stop("here")
# data$presenter1affiliation[ekenn] = 
#   "eunice kennedy shriver national institute of child health and human development"
data$presenter1affiliation[ekenn] = 
  "national institute of child health and human development"


# cornell
corn = grepl("cornell", 
             data$presenter1affiliation)
print(unique(data$presenter1affiliation[corn]))
data$presenter1affiliation[corn] = 
  "cornell university"

# wayne state university 
wayne = grepl("wayne", 
              data$presenter1affiliation) &
  !grepl("fort wayne", data$presenter1affiliation)
print(unique(data$presenter1affiliation[wayne]))
data$presenter1affiliation[wayne] = 
  "wayne state university"

# texas tech university
ttech = grepl("texas tech", 
           data$presenter1affiliation)
print(unique(data$presenter1affiliation[ttech]))
data$presenter1affiliation[ttech] = 
  "texas tech university"

# texas a&m university
am = grepl("a&m", 
           data$presenter1affiliation)
print(unique(data$presenter1affiliation[am]))
data$presenter1affiliation[am] = 
  "texas a&m university"

# university of texas - may need to change different satellites
## like MD anderson
mda = grepl("anderson", 
            data$presenter1affiliation) &
  !grepl("rice", data$presenter1affiliation)
print(unique(data$presenter1affiliation[mda]))
data$presenter1affiliation[mda] = 
  "university of texas, md anderson cancer center"

### tease out
# university of texas - may need to change different satellites
## like MD anderson - this will rewrite md anderson
tex = grepl("of texa(s|x)", 
            data$presenter1affiliation) & !am 
& !mda
print(unique(data$presenter1affiliation[tex]))
data$presenter1affiliation[tex] = 
  "university of texas"

### UT variant for texas
tex = grepl("^ut(-| )", 
            data$presenter1affiliation) & 
  !grepl("anderson", 
         data$presenter1affiliation)
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

# maryland
umd = (grepl("of maryland", 
             data$presenter1affiliation) & 
         ! grepl("baltimore county", data$presenter1affiliation)) |
  (grepl("umd", data$presenter1affiliation) & 
     ! grepl("umdnj", data$presenter1affiliation))
print(unique(data$presenter1affiliation[umd]))
data$presenter1affiliation[umd] = 
  "university of maryland"

# umbc
umbc = (grepl("of maryland", 
             data$presenter1affiliation) & 
         grepl("baltimore county", data$presenter1affiliation)) |
  grepl("umbc", data$presenter1affiliation)
print(unique(data$presenter1affiliation[umbc]))
data$presenter1affiliation[umbc] = 
  "university of maryland, baltimore county"


# harvard
hvpub = grepl("ha(r|)vard", 
              data$presenter1affiliation) &
  grepl("public health", data$presenter1affiliation)
print(unique(data$presenter1affiliation[hvpub]))
data$presenter1affiliation[hvpub] = 
  "harvard school of public health"

# harvard
harv = grepl("harvard", 
              data$presenter1affiliation) &
  !grepl("public health", data$presenter1affiliation)
print(unique(data$presenter1affiliation[harv]))
data$presenter1affiliation[harv] = 
  "harvard university"

# iowa
iowa = grepl("university of iowa", 
             data$presenter1affiliation)
print(unique(data$presenter1affiliation[iowa]))
data$presenter1affiliation[iowa] = 
  "university of iowa"

# iowa state
iowas = grepl("iowa state", 
             data$presenter1affiliation)
print(unique(data$presenter1affiliation[iowas]))
data$presenter1affiliation[iowas] = 
  "iowa state university"

# rochester
roch = grepl("(o|p)f rochester", 
              data$presenter1affiliation)
print(unique(data$presenter1affiliation[roch]))
data$presenter1affiliation[roch] = 
  "university of rochester"

# drexel
drex = grepl("drexel", 
             data$presenter1affiliation)
print(unique(data$presenter1affiliation[drex]))
data$presenter1affiliation[drex] = 
  "drexel university"

# penn state
penn = grepl("penn(sylvania|) state university", 
             data$presenter1affiliation)
print(unique(data$presenter1affiliation[penn]))
data$presenter1affiliation[penn] = 
  "penn state university"

# indiana-purdue
ipurd = grepl("purdue", 
             data$presenter1affiliation) & 
  grepl("indiana", data$presenter1affiliation)
print(unique(data$presenter1affiliation[ipurd]))
data$presenter1affiliation[ipurd] = 
  "indiana-purdue university"

# purdue
purd = grepl("purdue", 
              data$presenter1affiliation) & 
  !grepl("indiana", data$presenter1affiliation)
print(unique(data$presenter1affiliation[purd]))
data$presenter1affiliation[purd] = 
  "purdue university"

#  indiana university
ind = grepl("indiana university", 
             data$presenter1affiliation)
print(unique(data$presenter1affiliation[ind]))
data$presenter1affiliation[ind] = 
  "indiana university"

#  o of georgia
geo = grepl("university of georgia", 
            data$presenter1affiliation) |
  grepl("uga", data$presenter1affiliation)
print(unique(data$presenter1affiliation[geo]))
data$presenter1affiliation[geo] = 
  "university of georgia"

#  georgia state
gsu = grepl("georgia state", 
            data$presenter1affiliation)
print(unique(data$presenter1affiliation[gsu]))
data$presenter1affiliation[gsu] = 
  "georgia state university"

#  georgia soutern
gsou = grepl("georgia southern", 
            data$presenter1affiliation)
print(unique(data$presenter1affiliation[gsou]))
data$presenter1affiliation[gsou] = 
  "georgia southern university"

#  georgia health sciences
ghs = grepl("georgia health science", 
             data$presenter1affiliation)
print(unique(data$presenter1affiliation[ghs]))
data$presenter1affiliation[ghs] = 
  "georgia health sciences university"

#  fox
fox = grepl("fox chase", 
            data$presenter1affiliation)
print(unique(data$presenter1affiliation[fox]))
data$presenter1affiliation[fox] = 
  "fox case cancer center"

#  georgia state
medg = grepl("medical college of georgia", 
            data$presenter1affiliation)
print(unique(data$presenter1affiliation[medg]))
data$presenter1affiliation[medg] = 
  "medical college of georgia"

#  leiden
lei = grepl("leiden", 
             data$presenter1affiliation)
print(unique(data$presenter1affiliation[lei]))
data$presenter1affiliation[lei] = 
  "leiden university medical center"

#  george mason 
gmu = grepl("mason", 
            data$presenter1affiliation)
print(unique(data$presenter1affiliation[gmu]))
data$presenter1affiliation[gmu] = 
  "george mason university"

#  GT 
gt = grepl("georgetown", 
            data$presenter1affiliation)
print(unique(data$presenter1affiliation[gt]))
data$presenter1affiliation[gt] = 
  "georgetown university"

#  Wake Forest 
wake = grepl("wake", 
           data$presenter1affiliation)
print(unique(data$presenter1affiliation[wake]))
data$presenter1affiliation[wake] = 
  "wake forest university"

#  uill 
uill = grepl("of illinois", 
            data$presenter1affiliation) & 
  ! grepl("urbana", data$presenter1affiliation)
print(unique(data$presenter1affiliation[uill]))
data$presenter1affiliation[uill] = 
  "university of illinois at chicago"

#  uill urbana
uillurb = grepl("of illinois", 
             data$presenter1affiliation) & 
  grepl("urbana", data$presenter1affiliation)
print(unique(data$presenter1affiliation[uillurb]))
data$presenter1affiliation[uillurb] = 
  "university of illinois at urbana champaign"


# GW
gw = grepl("george washington", data$presenter1affiliation) | 
  grepl("gwu", data$presenter1affiliation)
print(unique(data$presenter1affiliation[gw]))
data$presenter1affiliation[gw] = "george washington university"

# CMU
cmu = grepl("carnegie", data$presenter1affiliation) | 
  grepl("cmu", data$presenter1affiliation)
print(unique(data$presenter1affiliation[cmu]))
data$presenter1affiliation[cmu] = "carnegie mellon university"

# Baylor
bay = grepl("baylor", data$presenter1affiliation) & 
  !grepl("rice", data$presenter1affiliation)
print(unique(data$presenter1affiliation[bay]))
data$presenter1affiliation[bay] = "baylor university"


# Rice
rice = grepl("rice", data$presenter1affiliation) & 
  !grepl("baylor", data$presenter1affiliation) & 
  !grepl("anderson", data$presenter1affiliation)
print(unique(data$presenter1affiliation[rice]))
data$presenter1affiliation[rice] = "rice university"

# FSU
fsu = grepl("florida state", data$presenter1affiliation)
print(unique(data$presenter1affiliation[fsu]))
data$presenter1affiliation[fsu] = "george washington university"

# FSU
uoff = grepl("of florida", data$presenter1affiliation)
print(unique(data$presenter1affiliation[uoff]))
data$presenter1affiliation[uoff] = "university of florida"

# BYU
byu = grepl("brigham young", data$presenter1affiliation) | 
  grepl("byu", data$presenter1affiliation)
print(unique(data$presenter1affiliation[byu]))
data$presenter1affiliation[byu] = "brigham young university"

# brigham and women's hospita
byaw = grepl("brigham and", data$presenter1affiliation) 
print(unique(data$presenter1affiliation[byaw]))
data$presenter1affiliation[byaw] = "brigham and women's hospital"

# northern illinois
nil = grepl("northern ill", data$presenter1affiliation) 
print(unique(data$presenter1affiliation[nil]))
data$presenter1affiliation[nil] = "northern illinois university"

# VT
vt = grepl("virgin(i|)a tech", data$presenter1affiliation) |
  grepl("virginia polytechnic institute", data$presenter1affiliation)
print(unique(data$presenter1affiliation[vt]))
# data$presenter1affiliation[vt] = "virginia polytechnic institute and state university"
data$presenter1affiliation[vt] = "virginia tech"

# UVA
uva = grepl("university of virginia", 
            data$presenter1affiliation) |
  grepl("uva", data$presenter1affiliation)
print(unique(data$presenter1affiliation[uva]))
data$presenter1affiliation[uva] = "university of virginia"

# Case Western
case = grepl("case western", 
            data$presenter1affiliation) | 
  grepl("cleveland clinic", data$presenter1affiliation)
print(unique(data$presenter1affiliation[case]))
data$presenter1affiliation[case] = "case western reserve university"

# duke
duke = grepl("duke", data$presenter1affiliation)
print(unique(data$presenter1affiliation[duke]))
data$presenter1affiliation[duke] = "duke university"

# stanford
stan = grepl("stanford", data$presenter1affiliation)
print(unique(data$presenter1affiliation[stan]))
data$presenter1affiliation[stan] = "stanford university"

# princeton
pri = grepl("princeton", data$presenter1affiliation)
print(unique(data$presenter1affiliation[pri]))
data$presenter1affiliation[pri] = "princeton university"

# temple
temp = grepl("temple ", data$presenter1affiliation)
print(unique(data$presenter1affiliation[temp]))
data$presenter1affiliation[temp] = "temple university"

# mcgill
mcg = grepl("mcgill", data$presenter1affiliation)
print(unique(data$presenter1affiliation[mcg]))
data$presenter1affiliation[mcg] = "mcgill university"

# amgen
amg = grepl("amgen", data$presenter1affiliation)
print(unique(data$presenter1affiliation[amg]))
data$presenter1affiliation[amg] = "amgen"

### nebraska
neb = grepl("nebraska", data$presenter1affiliation)
print(unique(data$presenter1affiliation[neb]))
data$presenter1affiliation[neb] = "university of nebraska"

### arkansas
ark = grepl("arkansas for med", data$presenter1affiliation)
print(unique(data$presenter1affiliation[ark]))
data$presenter1affiliation[ark] = "university of arkansas for medical sciences"

### alaska
alas = grepl("alaska", data$presenter1affiliation)
print(unique(data$presenter1affiliation[alas]))
data$presenter1affiliation[alas] = "university of alaska"

### buffalo buffalo buffalo buffalo buffalo
data$presenter1affiliation = gsub("buff\valo", "buffalo", data$presenter1affiliation)
buff = grepl("buffalo", data$presenter1affiliation)
print(unique(data$presenter1affiliation[buff]))
data$presenter1affiliation[buff] = "state university of new york at buffalo"

### ohio
ohio = grepl("ohio", data$presenter1affiliation)
print(unique(data$presenter1affiliation[ohio]))
data$presenter1affiliation[ohio] = "ohio state university"

### ohio
ok = grepl("oklahoma", data$presenter1affiliation)
print(unique(data$presenter1affiliation[ok]))
data$presenter1affiliation[ok] = "oklahoma state university"

### rutges
rut = grepl("rutgers", data$presenter1affiliation)
print(unique(data$presenter1affiliation[rut]))
data$presenter1affiliation[rut] = "rutgers university"

### utah
utah = grepl("utah", data$presenter1affiliation)
print(unique(data$presenter1affiliation[utah]))
data$presenter1affiliation[utah] = "utah state university"

### U west onto
onta = grepl("western ontario", data$presenter1affiliation)
print(unique(data$presenter1affiliation[onta]))
data$presenter1affiliation[onta] = "university of western ontario"


### vcu
vcu = grepl("vcu", data$presenter1affiliation) |
  grepl("virginia common", data$presenter1affiliation)
print(unique(data$presenter1affiliation[vcu]))
data$presenter1affiliation[vcu] = "virginia commonwealth university"


# Hasselt
ibio = grepl("i-biostat", 
             data$presenter1affiliation) |
  grepl("hasselt", data$presenter1affiliation)
print(unique(data$presenter1affiliation[ibio]))
data$presenter1affiliation[ibio] = 
  "hasselt university"

# saint judes
jude = grepl("jude", data$presenter1affiliation)
print(unique(data$presenter1affiliation[jude]))
data$presenter1affiliation[jude] = "st jude children's research hospital"

# h le moffitt
mof = grepl("moffitt", data$presenter1affiliation)
print(unique(data$presenter1affiliation[mof]))
data$presenter1affiliation[mof] = "university of south florida"

# usf
usf = grepl("of south florida", data$presenter1affiliation)
print(unique(data$presenter1affiliation[usf]))
data$presenter1affiliation[usf] = "university of south florida"


# fred hutchinson
hut = grepl("hu(t|)chinson", data$presenter1affiliation)
print(unique(data$presenter1affiliation[hut]))
data$presenter1affiliation[hut] = "fred hutchinson cancer research center"

#  Uniformed Services U
uni = grepl("uniformed services", 
            data$presenter1affiliation) 
print(unique(data$presenter1affiliation[uni]))
data$presenter1affiliation[uni] = 
  "uniformed services university"

# bama
bama = grepl("university of alabama", 
            data$presenter1affiliation) |
  grepl("uab", data$presenter1affiliation)
print(unique(data$presenter1affiliation[bama]))
data$presenter1affiliation[bama] = 
  "university of alabama"

# Jeff
jeff = grepl("jeff", 
             data$presenter1affiliation) & 
  ! grepl("personalized", data$presenter1affiliation)
print(unique(data$presenter1affiliation[jeff]))
data$presenter1affiliation[jeff] = 
  "thomas jefferson university"

#Brown
brown = grepl("brown", 
             data$presenter1affiliation) 
print(unique(data$presenter1affiliation[brown]))
data$presenter1affiliation[brown] = 
  "brown university"

#UConn
uconn = grepl("of connecticut", 
              data$presenter1affiliation) |
  grepl("uconn", data$presenter1affiliation)
print(unique(data$presenter1affiliation[uconn]))
data$presenter1affiliation[uconn] = 
  "university of connecticut"

## tufts
tufts = grepl("tufts", 
              data$presenter1affiliation) 
print(unique(data$presenter1affiliation[tufts]))
data$presenter1affiliation[tufts] = 
  "tufts university"

## tulane
tul = grepl("tulane", 
              data$presenter1affiliation) 
print(unique(data$presenter1affiliation[tul]))
data$presenter1affiliation[tul] = 
  "tulane university"

## tufts
cle = grepl("clemson", 
              data$presenter1affiliation) 
print(unique(data$presenter1affiliation[cle]))
data$presenter1affiliation[cle] = 
  "clemson university"

## louisville
lou = grepl("louisville", 
              data$presenter1affiliation) | 
  grepl("lousiville", data$presenter1affiliation)
print(unique(data$presenter1affiliation[lou]))
data$presenter1affiliation[lou] = 
  "university of louisville"

## colorado
col = grepl("of colorado", 
            data$presenter1affiliation)
print(unique(data$presenter1affiliation[col]))
data$presenter1affiliation[col] = 
  "university of colorado"

## kansas
kans = grepl("of kans", 
            data$presenter1affiliation) 
print(unique(data$presenter1affiliation[kans]))
data$presenter1affiliation[kans] = 
  "university of kansas"


## u of lousiana
ulou = grepl("of louisiana", 
            data$presenter1affiliation) 
print(unique(data$presenter1affiliation[ulou]))
data$presenter1affiliation[ulou] = 
  "university of louisiana"

## nyu
nyu = grepl("new york univ", 
             data$presenter1affiliation) |
  grepl("nyu", data$presenter1affiliation)
print(unique(data$presenter1affiliation[nyu]))
data$presenter1affiliation[nyu] = 
  "new york university"

## ghent
ghent = grepl("ghent u", 
            data$presenter1affiliation)
print(unique(data$presenter1affiliation[ghent]))
data$presenter1affiliation[ghent] = 
  "ghent university"


## lsu
lsu = grepl("lsu", 
            data$presenter1affiliation) |
  grepl("louisiana state", data$presenter1affiliation)
print(unique(data$presenter1affiliation[lsu]))
data$presenter1affiliation[lsu] = 
  "louisiana state university"

## uchicago
chi = grepl("of chicago", 
            data$presenter1affiliation)
print(unique(data$presenter1affiliation[chi]))
data$presenter1affiliation[chi] = 
  "university of chicago"

# southern methodist
smeth = grepl("southern methodist", 
              data$presenter1affiliation) 
print(unique(data$presenter1affiliation[smeth]))
data$presenter1affiliation[smeth] = 
  "southern methodist university"

# aalborg u
aal = grepl("aalborg", 
              data$presenter1affiliation) 
print(unique(data$presenter1affiliation[aal]))
data$presenter1affiliation[aal] = 
  "aalborg university"

# williams u
will = grepl("williams", 
            data$presenter1affiliation) 
print(unique(data$presenter1affiliation[will]))
data$presenter1affiliation[will] = 
  "williams college"

# buchholz HS
buch = grepl("buchholz", 
             data$presenter1affiliation) 
print(unique(data$presenter1affiliation[buch]))
data$presenter1affiliation[buch] = 
  "buchholz high school"

# Memorial Sloan
mslo = grepl("memorial", 
             data$presenter1affiliation) & 
  grepl("sloan", data$presenter1affiliation)
print(unique(data$presenter1affiliation[mslo]))
data$presenter1affiliation[mslo] = 
  "memorial sloan kettering cancer center"

# Central Connecticut
cct = grepl("central (connecticut|ct)", 
             data$presenter1affiliation) 
print(unique(data$presenter1affiliation[cct]))
data$presenter1affiliation[cct] = 
  "central connecticut state university"

# Dartmouth
dart = grepl("dartmouth", 
            data$presenter1affiliation) 
print(unique(data$presenter1affiliation[dart]))
data$presenter1affiliation[dart] = 
  "dartmouth medical school" 

# duquesne
duq = grepl("duquesne", 
             data$presenter1affiliation) 
print(unique(data$presenter1affiliation[duq]))
data$presenter1affiliation[duq] = 
  "duquesne university" 

# erasmus
era = grepl("erasmus", 
            data$presenter1affiliation) 
print(unique(data$presenter1affiliation[era]))
data$presenter1affiliation[era] = 
  "erasmus university medical center" 

# korea
kor = grepl("korea u", 
            data$presenter1affiliation) 
print(unique(data$presenter1affiliation[kor]))
data$presenter1affiliation[kor] = 
  "korea university" 

# korea
pus = grepl("pusan", 
            data$presenter1affiliation) 
print(unique(data$presenter1affiliation[pus]))
data$presenter1affiliation[pus] = 
  "pusan national university" 

# university of north texas health science center
uont = grepl("of north tex", 
            data$presenter1affiliation) 
print(unique(data$presenter1affiliation[uont]))
data$presenter1affiliation[uont] = 
  "university of north texas health science center" 


#################
## industry
##############
# drohan
dro = grepl("drohan", 
            data$presenter1affiliation) 
print(unique(data$presenter1affiliation[dro]))
data$presenter1affiliation[dro] = 
  "drohan"

# berry
ber = grepl("berry", 
            data$presenter1affiliation) 
print(unique(data$presenter1affiliation[ber]))
data$presenter1affiliation[ber] = 
  "berry consultants"

### sas
sas = grepl("sas inst", data$presenter1affiliation)
print(unique(data$presenter1affiliation[sas]))
data$presenter1affiliation[sas] = "sas institute"

#eli lilly
eli = grepl("lilly", data$presenter1affiliation)
print(unique(data$presenter1affiliation[eli]))
data$presenter1affiliation[eli] = "eli lilly"

### glaxo
gsk = grepl("glaxo", data$presenter1affiliation) | 
  grepl("gsk", data$presenter1affiliation)
print(unique(data$presenter1affiliation[gsk]))
data$presenter1affiliation[gsk] = "glaxosmithkline"

# mayo
mayo = grepl("mayo", 
             data$presenter1affiliation)
print(unique(data$presenter1affiliation[mayo]))
data$presenter1affiliation[mayo] = 
  "mayo clinic"

#  merck
merck = grepl("merck", 
            data$presenter1affiliation) 
print(unique(data$presenter1affiliation[merck]))
data$presenter1affiliation[merck] = 
  "merck"

#  pfizer
pfi = grepl("pfizer", 
              data$presenter1affiliation) 
print(unique(data$presenter1affiliation[pfi]))
data$presenter1affiliation[pfi] = 
  "pfizer"

#  bristol-myers
bms = grepl("squib", 
            data$presenter1affiliation) 
print(unique(data$presenter1affiliation[bms]))
data$presenter1affiliation[bms] = 
  "bristol-myers squibb"

#  j&j
jj = grepl("johnson &", 
            data$presenter1affiliation) 
print(unique(data$presenter1affiliation[jj]))
data$presenter1affiliation[jj] = 
  "johnson & johnson"

#  janssen
jan = grepl("janssen", 
           data$presenter1affiliation) 
print(unique(data$presenter1affiliation[jan]))
data$presenter1affiliation[jan] = 
  "janssen"

# wyeth
wyeth = grepl("wyeth", 
           data$presenter1affiliation) 
print(unique(data$presenter1affiliation[wyeth]))
data$presenter1affiliation[wyeth] = 
  "wyeth"

# rti
rti = grepl("rti ", 
              data$presenter1affiliation) 
print(unique(data$presenter1affiliation[rti]))
data$presenter1affiliation[rti] = 
  "rti international"

# novartis
nov = grepl("novartis", 
            data$presenter1affiliation) 
print(unique(data$presenter1affiliation[nov]))
data$presenter1affiliation[nov] = 
  "novartis pharmacueticals"

# medimmune
mum = grepl("medimmune", 
            data$presenter1affiliation) 
print(unique(data$presenter1affiliation[mum]))
data$presenter1affiliation[mum] = 
  "medimmune"

# sanofi
son = grepl("sanofi", 
            data$presenter1affiliation) 
print(unique(data$presenter1affiliation[son]))
data$presenter1affiliation[son] = 
  "sanofi"

# medtron
medt = grepl("medtronic", 
            data$presenter1affiliation) 
print(unique(data$presenter1affiliation[medt]))
data$presenter1affiliation[medt] = 
  "medtronic"

# medtron
abo = grepl("abbott", 
             data$presenter1affiliation) 
print(unique(data$presenter1affiliation[abo]))
data$presenter1affiliation[abo] = 
  "abbott laboratories"

# celgene
cel = grepl("celgene", 
            data$presenter1affiliation) 
print(unique(data$presenter1affiliation[cel]))
data$presenter1affiliation[cel] = 
  "celgene"

# celgene
cyt = grepl("cytel", 
            data$presenter1affiliation) 
print(unique(data$presenter1affiliation[cyt]))
data$presenter1affiliation[cyt] = 
  "cytel"

# PPD
ppd = grepl("ppd", 
            data$presenter1affiliation) 
print(unique(data$presenter1affiliation[ppd]))
data$presenter1affiliation[ppd] = 
  "ppd"

# cephalon
ceph = grepl("cephalon", 
            data$presenter1affiliation) 
print(unique(data$presenter1affiliation[ceph]))
data$presenter1affiliation[ceph] = 
  "cephalon"

# christiana
chr = grepl("christiana", 
             data$presenter1affiliation) 
print(unique(data$presenter1affiliation[chr]))
data$presenter1affiliation[chr] = 
  "christiana care health system"

# christiana
far = grepl("farber|data", 
            data$presenter1affiliation) 
print(unique(data$presenter1affiliation[far]))
data$presenter1affiliation[far] = 
  "dana-farber cancer institute"

#################
## government
##############
#  FDA
fda = grepl("fda", 
            data$presenter1affiliation) |
  grepl("food and", data$presenter1affiliation)
print(unique(data$presenter1affiliation[fda]))
data$presenter1affiliation[fda] = 
  "food and drug administration"

#  nci
nci = grepl("nci(,| |$)", 
            data$presenter1affiliation) |
  grepl("national cancer institute", data$presenter1affiliation)
print(unique(data$presenter1affiliation[nci]))
data$presenter1affiliation[nci] = 
  "national cancer institute"

#  nih
nih = grepl("nih", 
            data$presenter1affiliation) |
  grepl("national institute(s|) of health", 
        data$presenter1affiliation)
print(unique(data$presenter1affiliation[nih]))
data$presenter1affiliation[nih] = 
  "national institutes of health"

#niehs
nie = grepl("national institute of environ", 
            data$presenter1affiliation) |
  grepl("niehs", data$presenter1affiliation)
print(unique(data$presenter1affiliation[nie]))
data$presenter1affiliation[nie] = 
  "national institute of environmental health sciences"

#  nhl
nhl = grepl("heart", 
            data$presenter1affiliation) &
  grepl("lung", data$presenter1affiliation)
print(unique(data$presenter1affiliation[nhl]))
data$presenter1affiliation[nhl] = 
  "national heart, lung and blood institute"


#  cdc
cdc = grepl("center(s|) for disease control", 
            data$presenter1affiliation) |
  grepl("cdc", data$presenter1affiliation)
print(unique(data$presenter1affiliation[cdc]))
data$presenter1affiliation[cdc] = 
  "centers for disease control"

#  cdp
cdp = grepl("center(s|) for disease prevention", 
            data$presenter1affiliation) 
print(unique(data$presenter1affiliation[cdp]))
data$presenter1affiliation[cdp] = 
  "center for disease prevention and health interventions in diverse populations"

#  cdp
epa = grepl("protection agency", 
            data$presenter1affiliation) |
  grepl("(us |)epa$", data$presenter1affiliation)
print(unique(data$presenter1affiliation[epa]))
data$presenter1affiliation[epa] = 
  "us environmental protection agency"


#  creal
creal = grepl("center(s|) for research in environmental epidemiology", 
            data$presenter1affiliation) |
  grepl("creal", data$presenter1affiliation)
print(unique(data$presenter1affiliation[creal]))
data$presenter1affiliation[creal] = 
  "center for research in environmental epidemiology"


#####################
### odd cases
######
univ = which(data$presenter1affiliation %in% "university")
info= data$presenter1affiliation[univ]
user = data$username[univ]
info[grepl("umich.edu", user)] = "university of michigan"
data$presenter1affiliation[univ] = info

amp = grepl("&", 
            data$presenter1affiliation)
print(sort(unique(data$presenter1affiliation[amp])))



# locations = data.frame(year = 1994:2013)
location = c("1994"="Cleveland, OH", 
              "1995"="Birmingham, AL",
              "1996"="Richmond, VA",
              "1997"="Memphis, TN",
              "1998"="Pittsburg, PA",
              "1999"="Atlanta, GA",
              "2000"="Chicago, IL",
              "2001"="Charlotte, NC",
              "2002"="Arlington, VA",
              "2003"="Tampa, FL",
              "2004"="Pittsburgh, PA",
              "2005"="Austin, TX",
              "2006"="Tampa, FL",
              "2007"="Atlanta, GA",
              "2008"="Arlington, VA",
              "2009"="San Antonio, TX",
              "2010"="New Orleans, LA",
              "2011"="Miami, FL",
              "2012"="Washington, DC",
              "2013"="Orlando, FL")
location = data.frame(location, stringsAsFactors=FALSE)
# location$location = factor(location$location, levels = location$location)
location$year = as.numeric(rownames(location))

data = merge(data, location, by="year", all.x=TRUE)

save(data, location,
     file=file.path(homedir,"Clean_Collapsed_2007_2013.Rda"))
