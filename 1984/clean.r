widths84 <- c(4,1,2,3,28,4,2,6,2,12,6,3,5,5,32,1,15,4,4,5,8,8,10,10,10,10,10,10,1,4)

d1984 <- read.fwf(file="84MFGUI.DAT", widths=widths84)

colmap84<- read.csv(file="colnames84.csv", header=TRUE)
names(d1984) <- colmap84$abbr

d1984$T  <- F
d1984$T[grep("TURBO", d1984$eng.dscr.1)] <- T
d1984$T[grep("TURBO", d1984$eng.dscr.2)] <- T
d1984$T[grep("TRBO", d1984$eng.dscr.1)] <- T
d1984$T[grep("TRBO", d1984$eng.dscr.2)] <- T


d1984$G <- F
d1984$G[grep("GUZZLER",d1984$eng.dscr.1)] <- T
d1984$G[grep("GUZZLER", d1984$eng.dscr.2)] <- T

d1984$m.trans<- gsub("A3", "auto(3)",d1984$m.trans)
d1984$m.trans<- gsub("A4", "auto(4)",d1984$m.trans)
d1984$m.trans<- gsub("M3", "manual(m3)",d1984$m.trans)
d1984$m.trans<- gsub("M3/M4C", "manual(m3/m4C)",d1984$m.trans)
d1984$m.trans<- gsub("M4", "manual(m4)",d1984$m.trans)
d1984$m.trans<- gsub("M4C", "manual(m4C)",d1984$m.trans)
d1984$m.trans<- gsub("M4X2", "manual(m4X2)",d1984$m.trans)
d1984$m.trans<- gsub("M5", "manual(m5)",d1984$m.trans)


d1984$bt.2 <- as.character(d1984$bt.2)
d1984$bt.4 <- as.character(d1984$bt.4)
d1984$bt.hbk <- as.character(d1984$bt.hbk)

d1984$twodhpv <- substr(d1984$bt.2,6,7)
d1984$twolpv <- substr(d1984$bt.2,9,10)
d1984$fourhpv<- substr(d1984$bt.4,5,7)
d1984$fourlpv<- substr(d1984$bt.4,9,10)
d1984$hbkhpv<- substr(d1984$bt.hbk,6,7)
d1984$hbklpv<- substr(d1984$bt.hbk,9,10)

d1984$S<- FALSE

names(d1984) [5] <-  "model"
names(d1984) [8] <-  "trans"
names(d1984) [23] <-  "eng.dscr"
names(d1984) [24] <-  "eng.dscr.1"
names(d1984) [33:38] <-c( "twodoor.p", "twodoor.l", "fourdoor.p", "fourdoor.l", "hatch.p", "hatch.l")

# 
# 
# d1984 <- d1984[c("year", "class", "carline.name", "displ", "m.trans", "cyl", 
# "manufacturer", "cty", "hwy", "eng.dscr.1", "eng.dscr.2", "T", "G", "twodhpv", "twolpv", "fourhpv", "fourlpv", "hbkhpv", "hbklpv")]
# 
# 
d1984$cmb <- NULL
d1984$carline.Manufacturer.code <- NULL
d1984$state.code <- NULL
d1984$vi.Manufacturer.code <- NULL
d1984$date <- NULL
d1984$r.date <- NULL
d1984$pr <- NULL
d1984$b.eng.id <- NULL
d1984$sc <- NULL                     
d1984$f1 <- NULL
d1984$f2 <- NULL
d1984$f3 <- NULL
d1984$fl.system <- NULL
d1984$fcost <- NULL
d1984$opt.disp <- NULL
d1984$od <- NULL
d1984$bt.2 <- NULL
d1984$bt.4 <- NULL
d1984$bt.hbk <- NULL