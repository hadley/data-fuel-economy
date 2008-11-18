setwd("/Users/erhv/Data_Sets/Fuel Economy/csv")
library(reshape)
options(stringsAsFactors = FALSE)

csv <- dir(".", pattern="[0-9]{4}\\.csv$")
years <- as.numeric(gsub("\\.csv", "", csv))

files <- lapply(csv, read.csv)
files <- mapply(function(df, y) transform(df, year = y), files, years, SIMPLIFY= FALSE)

cols <- sapply(files, function(x) names(x)[1:30])
write.table(cols, "col-names.csv", sep=",", quote=F, row=F)

mapping <- read.csv("col-mapping.csv")
newnames <- mapping$abbr
names(newnames) <- mapping$full

files <- lapply(files, rename, newnames)
names(files[[10]]) [30] <- "filler"
names(files[[11]]) [30] <- "filler"
mpg <- do.call("rbind.fill", files)



names(mpg) <- c("class", "manufacturer", "model", "displ", "cyl", "trans", 
"drv", "cty", "hwy", "cmb", "ucty", "uhwy", "ucmb", "fl", "G", 
"T", "S", "X2pv", "X2lv", "X4pv", "X4lv", "hpv", "hlv", "fcost", 
"eng.dscr", "trans.dscr", "year", "vpc", "cls", "bidx", "x5c"
)

chr <- sapply(mpg, is.character)
mpg[chr] <- lapply(mpg[chr], tolower)
mpg$class <- gsub("spec purp veh - ", "", mpg$class)
mpg$class <- gsub("special purpose vehicle", "s.u.v. -", mpg$class)

mpg$class[grep("minivan", mpg$class)] <- "minivan"

# Convert old mpg to new (post-2008) mpg --------------------------------
new_city <- function(mpg, year) {
  ifelse(year < 2008, round(1 / (0.003259 + (1.1805 / round(mpg / 0.9)))), mpg)
}
new_highway <- function(mpg, year) {
  ifelse(year < 2008, round(1 / (0.001376 + (1.3466 / round(mpg / 0.78)))), mpg)
}

mpg <- transform(mpg,
  cty = new_city(cty, year),
  hwy = new_highway(hwy, year)
)

# Remove inconsistent variables ----------------------------------------
mpg$eng.dscr.1 <- NA
mpg$eng.dscr.2 <- NA


mpg$cmb <- NULL
mpg$fcost <- NULL
mpg$cls <- NULL
mpg$ucty <- NULL
mpg$uhwy <- NULL
mpg$ucmb <- NULL


#mpg$X2pv  <- NULL
#mpg$X2lv  <- NULL
#mpg$X4pv  <- NULL
#mpg$X4lv <- NULL

# Fix boolean labels -

mpg$T[is.na(mpg$T)] <- FALSE
mpg$G <- ifelse(mpg$G == "g", T, F)
mpg$S <- ifelse(mpg$S == "s", T, F)


#Create variables to match 1978-1983
names(mpg) [14:19] <-c( "twodoor.p", "twodoor.l", "fourdoor.p", "fourdoor.l", "hatch.p", "hatch.l")

# Remove bad rows

mpg <- subset(mpg, model != "")

write.table(mpg, "mpg.csv", sep=",", row=F)
write.table(mpg, "mpg.txt", sep="\t", row=F)
