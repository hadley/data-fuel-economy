setwd("/Users/erhv/Data_Sets/Fuel Economy/1985-1997")
library(reshape)
options(stringsAsFactors = FALSE)

#FWF reported in data formats----------------------------------------------------------------------
widths <- c(4,1,2,3,28,4,2,6,2,12,6,3,5,5,32,1,6,5,5,5,5,4,6,8,10,10,10,10,10,10,1,4,3,2,15)
paths <- dir(pattern = "\\.DAT$")
paths <- paths[paths != "95MFGUI.DAT"]


#read in data files--------------------------------------------------------------------------------
d1995 <- read.fwf(file="95MFGUI.DAT", widths=c(12,4,1,2,3,28,4,2,6,2,12,6,3,5,5,32,1,6,5,5,5,5,4,6,8,10,10,10,10,10,10,1,4,3,2,15))
str(d1995)
d1995$V1<- NULL
write.table(d1995, file="1995.CSV",sep=",", col.names=FALSE, row.names=FALSE)
d1995 <- read.csv(file="1995.csv")
files <- lapply(paths, read.fwf, width=widths, as.is = TRUE)


# rbind(files[[1]], files[[2]], files[[3]],    )---------------------------------------------------
data <- do.call(rbind, files)


#Rename names of d1995  to match names of files----------------------------------------------------
datanames<- names(data)
names(d1995) <- datanames


# Combine d1995 and data---------------------------------------------------------------------------
mpg <- rbind(data, d1995)
colmap<- read.csv(file="col-map.csv", header=TRUE)


# Rename the names of mpg to match names of data frame create by Hadley for the years 1998-2007 ---
colmap<- read.csv(file="col-map.csv", header=TRUE)
names(mpg) <- colmap$ABBR


# Strip out extra whitespace on character columns--------------------------------------------------
nakedmpg<- lapply(mpg, function(x) gsub("^ +| +$", "", x))
nakedmpg<- as.data.frame(nakedmpg)


#Create Guzzler Vector-----------------------------------------------------------------------------
nakedmpg$G  <- F
nakedmpg$G[grep("GUZZLER", nakedmpg$eng.dscr.1)] <- T
nakedmpg$G[grep("GUZZLER", nakedmpg$eng.dscr.2)] <- T
nakedmpg$G[grep("GUZZLER", nakedmpg$eng.dscr.3)] <- T
table(nakedmpg$G, exclude=NULL)


#Create TURBO Vector: for different years turbocharged was entered as TRBO, TURBO, and TC*--------
nakedmpg$T  <- F
nakedmpg$T[grep("TRBO", nakedmpg$eng.dscr.1)] <- T
nakedmpg$T[grep("TRBO", nakedmpg$eng.dscr.2)] <- T
nakedmpg$T[grep("TRBO", nakedmpg$eng.dscr.3)] <- T

nakedmpg$T[grep("TURBO", nakedmpg$eng.dscr.1)] <- T
nakedmpg$T[grep("TURBO", nakedmpg$eng.dscr.2)] <- T
nakedmpg$T[grep("TURBO", nakedmpg$eng.dscr.3)] <- T

nakedmpg$T[grep("TC*", nakedmpg$eng.dscr.1)] <- T
nakedmpg$T[grep("TC*", nakedmpg$eng.dscr.2)] <- T
nakedmpg$T[grep("TC*", nakedmpg$eng.dscr.3)] <- T
table(nakedmpg$T, exclude=NULL)


#Clean up and recreate class variable--------------------------------------------------------------
table(nakedmpg$year, nakedmpg$class)
classname <- read.csv(file="classname.csv")
names(nakedmpg)[3] <- "cls"
nakedmpg <- merge(nakedmpg, classname,by="cls")

# Convert old mpg to new (post-2008) mpg ----------------------------------------------------------
nakedmpg$year <- as.numeric(nakedmpg$year)
nakedmpg$cty <- as.numeric(nakedmpg$cty)
nakedmpg$hwy <- as.numeric(nakedmpg$hwy)

new_city <- function(mpg, year) {
  ifelse(year < 2008, round(1 / (0.003259 + (1.1805 / round(mpg / 0.9)))), mpg)
}
new_highway <- function(mpg, year) {
  ifelse(year < 2008, round(1 / (0.001376 + (1.3466 / round(mpg / 0.78)))), mpg)
}

nakedmpg <- transform(nakedmpg,
  cty = new_city(cty, year),
  hwy = new_highway(hwy, year)
)


#remove uneeded and inconsistent variables---------------------------------------------------------
nakedmpg$cmb <- NULL
nakedmpg$cls <- NULL
nakedmpg$ucty <- NULL
nakedmpg$uhwy <- NULL
nakedmpg$ucmb <- NULL
nakedmpg$carline.Manufacturer.code <- NULL
nakedmpg$state.code <- NULL
nakedmpg$vi.Manufacturer.code <- NULL
nakedmpg$date <- NULL
nakedmpg$r.date <- NULL
nakedmpg$pr <- NULL
nakedmpg$b.eng.id <- NULL
nakedmpg$sc <- NULL
nakedmpg$c <- NULL
nakedmpg$fl.system <- NULL
nakedmpg$fcost<- NULL
nakedmpg$opt.disp <- NULL
nakedmpg$od <- NULL

nakedmpg$S <- NA
nakedmpg$bidx <- NA
nakedmpg$x5c <- NA
nakedmpg$vpc <- NA
nakedmpg$trans.code.1 <- NA
nakedmpg$trans.code.2 <- NA
nakedmpg$sale.mtot <- NA

#Rename m.trans into trans new format
nakedmpg$m.trans<- gsub("A3", "auto(3)",nakedmpg$m.trans)
nakedmpg$m.trans<- gsub("A4", "auto(4)",nakedmpg$m.trans)
nakedmpg$m.trans<- gsub("A5", "auto(5)",nakedmpg$m.trans)
nakedmpg$m.trans<- gsub("AV", "auto(V)",nakedmpg$m.trans)
nakedmpg$m.trans<- gsub("M3", "manual(m3)",nakedmpg$m.trans)
nakedmpg$m.trans<- gsub("M3/M4C", "manual(m3/m4C)",nakedmpg$m.trans)
nakedmpg$m.trans<- gsub("M4", "manual(m4)",nakedmpg$m.trans)
nakedmpg$m.trans<- gsub("M4C", "manual(m4C)",nakedmpg$m.trans)
nakedmpg$m.trans<- gsub("M5", "manual(m5)",nakedmpg$m.trans)
nakedmpg$m.trans<- gsub("M5C", "manual(m5C)",nakedmpg$m.trans)
nakedmpg$m.trans<- gsub("M6", "manual(m6)",nakedmpg$m.trans)
nakedmpg$m.trans<- gsub("S4", "semi-automatic(s4)",nakedmpg$m.trans)
nakedmpg$m.trans<- gsub("S5", "semi-automatic(s5)",nakedmpg$m.trans)
nakedmpg$m.trans<- gsub("C5", "creeper(C5)",nakedmpg$m.trans)
nakedmpg$m.trans<- gsub("L3", "lock-up(s3)",nakedmpg$m.trans)
nakedmpg$m.trans<- gsub("L4", "lock-up(s4)",nakedmpg$m.trans)
nakedmpg$m.trans<- gsub("L5", "lock-up(s5)",nakedmpg$m.trans)
names(nakedmpg) [4] <- "trans"
names(nakedmpg) [9] <- "eng.dscr"
names(nakedmpg) [10] <- "eng.dscr.1"
names(nakedmpg) [11] <- "eng.dscr.2"

nakedmpg$bt.2 <- as.character(nakedmpg$bt.2)
nakedmpg$bt.4 <- as.character(nakedmpg$bt.4)
nakedmpg$bt.hbk <- as.character(nakedmpg$bt.hbk)

nakedmpg$twodhpv <- substr(nakedmpg$bt.2,6,7)
nakedmpg$twolpv <- substr(nakedmpg$bt.2,9,10)
nakedmpg$fourhpv<- substr(nakedmpg$bt.4,6,7)
nakedmpg$fourlpv<- substr(nakedmpg$bt.4,9,10)
nakedmpg$hbkhpv<- substr(nakedmpg$bt.hbk,6,7)
nakedmpg$hbklpv<- substr(nakedmpg$bt.hbk,9,10)

names(nakedmpg) [25] <- "twodoor.p"
names(nakedmpg) [26] <- "twodoor.l"
names(nakedmpg) [27] <- "fourdoor.p"
names(nakedmpg) [28] <- "fourdoor.l"
names(nakedmpg) [29] <- "hatch.p"
names(nakedmpg) [30] <- "hatch.l"

nakedmpg$bt.2 <- NULL
nakedmpg$bt.4 <- NULL
nakedmpg$bt.hbk <- NULL

names(nakedmpg) [2]  <- "model"
nakedmpg$hbkhpv <- NULL
nakedmpg$hbklpv <- NULL
nakedmpg$fourlpv <- NULL


#Re-order the variables by year, ------------------------------------------------------------------
nakedmpg <- nakedmpg[order(nakedmpg[,"manufacturer"]),]
nakedmpg <- nakedmpg[order(nakedmpg[,"carline.name"]),]
nakedmpg <- nakedmpg[order(nakedmpg[,"class"]),]
nakedmpg <- nakedmpg[order(nakedmpg[,"year"]),]


# Save as csv--------------------------------------------------------------------------------------
write.table(nakedmpg, file="clean85-97.csv",sep=",", col.names=TRUE, row.names=FALSE)
  




#other code fit it in later to tell the story


