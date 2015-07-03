######################################################################
### R code mimicking the analysis performed in
### http://www.morgenpost.de/berlin/article137810495/Wie-sich-die-Masern-in-Berlin-ausbreiten.html
###
### Author: Michael Höhle
######################################################################

library("surveillance")
library("ISOweek")
library("rgdal")

###Read survstat measles data for Berlin (queried from https://survstat.rki.de/)
survstat <- read.csv("survstat/Data.csv",fileEncoding="UCS-2LE",sep="\t")
###massage data
dates <- ISOweek2date(paste0(gsub("w","W",as.character(survstat[,1])),"-1"))
names <- gsub("City\\.of\\.Berlin\\.","",names(survstat)[-1])
names <- gsub("\\.","-",names)
names(survstat)[-1] <- names

### Get a shape with the bezirke
###Why doesn't the raw reading from github work?
###check: http://stackoverflow.com/questions/14441729/read-a-csv-from-github-into-r
###Alternative: use Rcurl
if (!file.exists("berliner-bezirke.kml")) {
  download.file("https://raw.githubusercontent.com/m-hoerz/berlin-shapes/master/berliner-bezirke.kml",
                destfile = "berliner-bezirke.kml", method = "curl")
}
bezirke <- readOGR("berliner-bezirke.kml","berliner_bezirke")

##Look at result
plot(bezirke)
as(bezirke,"data.frame")

###Match row.names & order in survstat and shape file
order <- pmatch(names, as.character(as(bezirke,"data.frame")$Name))
bezirke <- bezirke[order,]
###Sanity check
all(as.character(as(bezirke,"data.frame")$Name) == names)
row.names(bezirke) <- names


###Make sts object
measles <- new("sts",observed=survstat[,-1],epoch=as.numeric(dates),epochAsDate=TRUE,map=bezirke)

###Illustrate overall series
plot(aggregate(measles,by="unit"),legend.opts=NULL)

###Create animation -- for sake of exposition we only take everything from 2011 and onwards
measles2011ao <- measles[epoch(measles) > as.Date("2011-01-01"),]
library("animation")
saveHTML(animate(measles2011ao))
