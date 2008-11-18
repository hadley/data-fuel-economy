setwd("/Users/erhv/Data_Sets/Fuel Economy/1978-1984")


#FWF for files ending in CG.DAT, these apply to years 1978-1983.
widthsCGFG <- c(1,1,1,2,5,5,3,2,3,3,3,6,4,2,1,1,2,2,2,1,1,8,8,8,8,6,2,3,6,15,28,4,10,10)
widths84 <- c(4,1,2,3,28,4,2,6,2,12,6,3,5,5,32,1,15,4,4,5,8,8,10,10,10,10,10,10,1,4)
widthscarlin <- c(37,3,3,3,3,3,5,3,3,3,3,10,21,8)


pathscg <- dir(pattern = "CG\\.DAT$")
pathsfg <- dir(pattern = "FG\\.DAT$")
pathscarlin <- dir(pattern = "CARLIN\\.DAT$")
filecg <- lapply(pathscg, read.fwf, width=widthsCGFG, as.is = TRUE)
filefg <- lapply(pathsfg, read.fwf, width=widthsCGFG, as.is = TRUE)
filecarlin <- lapply(pathscarlin, read.fwf, skip=19,width=widthscarlin, as.is = TRUE)

calidata <- do.call(rbind, filecg)
statedata <- do.call(rbind, filefg)
carlindata<- do.call(rbind, filecarlin)

d1984 <- read.fwf(file="84MFGUI.DAT", widths=widths84)



#Rename the names of mpg to match names of data frame create by Hadley for the years 1998-2007 ---
colmap78.83<- read.csv(file="colnames78-83.csv", header=TRUE)
names(calidata) <- colmap78.83$abbr
names(statedata) <- colmap78.83$abbr

colmap84<- read.csv(file="colnames84.csv", header=TRUE)
names(d1984) <- colmap84$abbr

#Reorganize carline, carlindata, data frame
names(carlindata) <- c("manufacturer", "class","f1","f2","f3", "twodoor.p", "twodoor.l", "fourdoor.p", "fourdoor.l","f4", "hatch.p", "hatch.l", "model", "codesymm")
carlindata$manufacturer <- sapply(carlindata$manufacturer, function(x) gsub("^ +| +$", "", x))
carlindata$model <- sapply(carlindata$model, function(x) gsub("^ +| +$", "", x))



#Add year variable
calidata$year <- rep(c(1978,1979,1980,1981,1982,1983),c(463,535,631,613,699,854))
statedata$year <-rep(c(1978,1979,1980,1981,1982,1983),c(695,773,956,872,981,1172))
carlindata$year<-rep(c(1978,1979,1980,1981,1982,1983),c(330,350,375,371,382,431))

#check year variable
length(calidata$f6)==length(calidata$year)
length(statedata$f6)==length(statedata$year)
length(carlindata$class)==length(carlindata$year)

#Create mfgcode and new carline.code varabiles from carline.code
calidata$carline.code <- as.character(calidata$carline.code)
calidata$carline.code<- sapply(calidata$carline.code, function(b)  ifelse((length(strsplit(b,"")[[length(b)]])==4), paste("0",b,sep=""),paste("",b,sep="")))
calidata$mfg.code <- substr(calidata$carline.code,1,3)
calidata$c.code <- substr(calidata$carline.code,4,5)
calidata$mfg.code<- ifelse(calidata$carline.name=="MODEL A ROADSTER REPRODUCTIO",gsub(288,289,calidata$mfg.code), calidata$mfg.code)
mfgcodelist <- read.csv(file="mfgcodes.csv",header=TRUE)
mfgcodelist$mfg.code <- as.character(mfgcodelist$mfg.code)
mfgcodelist$mfg.code <- sapply(mfgcodelist$mfg.code, function(b)  ifelse((length(strsplit(b,"")[[length(b)]])==2), paste("0",b,sep=""),paste("",b,sep="")))
calidata <- merge(calidata,mfgcodelist, by="mfg.code")

statedata$carline.code <- as.character(statedata$carline.code)
statedata$carline.code<- sapply(statedata$carline.code, function(b)  ifelse((length(strsplit(b,"")[[length(b)]])==4), paste("0",b,sep=""),paste("",b,sep="")))
statedata$mfg.code <- substr(statedata$carline.code,1,3)
statedata$c.code <- substr(statedata$carline.code,4,5)
statedata$mfg.code<- ifelse(statedata$carline.name=="MODEL A ROADSTER REPRODUCTIO",gsub(288,289,statedata$mfg.code), statedata$mfg.code)
statedata <- merge(statedata,mfgcodelist, by="mfg.code")

#Create combined california and state dataframe
cali.state <- rbind(calidata, statedata)

#Create merge variable for carline and cali.state data frames.  This allows to get the passenger and luggage volume for these years. 
d1978 <- subset(cali.state, year==1978)
d1978$code.2 <- paste("78", d1978$carline.code, sep="")
d1979 <- subset(cali.state, year==1979)
d1979$code.2 <- paste("79", d1979$carline.code, sep="")
d1980 <- subset(cali.state, year==1980)
d1980$code.2 <- paste("80", d1980$carline.code, sep="")
d1981 <- subset(cali.state, year==1981)
d1981$code.2 <- paste("81", d1981$carline.code, sep="")
d1982 <- subset(cali.state, year==1982)
d1982$code.2 <- paste("82", d1982$carline.code, sep="")
d1983 <- subset(cali.state, year==1983)
d1983$code.2 <- paste("83", d1983$carline.code, sep="")
cali.state <- rbind(d1978, d1979, d1980, d1981, d1982,d1983)

carlindata <- carlindata[order(carlindata[,"codesymm"]),]
cali.state <- cali.state[order(new.cali.state[,"code.2"]),]
carlindata$code.2 <- substr(carlindata$codesymm,1,7)

carlindata$manufacturer <- NULL
carlindata$f1 <- NULL
carlindata$f2 <- NULL
carlindata$f3 <- NULL
carlindata$f4 <- NULL
carlindata$year <- NULL
carlindata$codesymm <- NULL
carlindata$model <- NULL

cali.state  <- merge(cali.state, carlindata, by="code.2")

#Create TURBO Vector and GUZZLER vector.
cali.state$T  <- F
cali.state$T[grep("TURBO", cali.state$eng.dscr.1)] <- T
cali.state$T[grep("TURBO", cali.state$eng.dscr.2)] <- T

cali.state$G <- F
cali.state$G[grep("GUZZLER", cali.state$eng.dscr.1)] <- T
cali.state$G[grep("GUZZLER", cali.state$eng.dscr.2)] <- T

cali.state$fl <- gsub(6, "Unleaded Gasoline",cali.state$fl)
cali.state$fl <- gsub(7, "Unleaded Gasoline",cali.state$fl)
cali.state$fl <- gsub(1, "Leaded Gasoline",cali.state$fl)
cali.state$fl <- gsub(17, "Leaded Gasoline",cali.state$fl)

cali.state$trans.code.2 <- gsub("M5", "manual(m5)",cali.state$trans.code.2)
cali.state$trans.code.2 <- gsub("M4", "manual(m4)",cali.state$trans.code.2)
cali.state$trans.code.2 <- gsub("M3", "manual(m3)",cali.state$trans.code.2)
cali.state$trans.code.2 <- gsub("A3", "auto(3)",cali.state$trans.code.2)
cali.state$trans.code.2 <- gsub("A4", "auto(5)",cali.state$trans.code.2)
cali.state$trans.code.2 <- gsub("S2", "semi-auto(2)",cali.state$trans.code.2)
cali.state$trans.code.2 <- gsub("S4", "semi-auto(4)",cali.state$trans.code.2)
cali.state$trans.code.2 <- gsub("C4", "creeper(2)",cali.state$trans.code.2)


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


#Create vectors to match 1998-2007 dataframe
cali.state$drv <- NA
cali.state$vpc <- NA
cali.state$bidx <- NA
cali.state$x5c <- NA
cali.state$hpv <- NA
cali.state$hlv <- NA
cali.state$S <- FALSE
cali.state$displ <- NA

d1984$drv <- NA
d1984$vpc <- NA
d1984$bidx <- NA
d1984$x5c <- NA
d1984$fl <- NA
d1984$S<- FALSE
d1984$trans.dscr<- NA


str(cali.state)
str(d1984)

#Renname names vectors to match 1998-2007 and add vectors 
names(cali.state) [10] <- "class"
names(cali.state) [11] <- "cty"
names(cali.state) [12] <-  "hwy"
names(cali.state) [16] <-  "trans"
names(cali.state) [22] <-  "trans.dscr"
names(cali.state) [33] <-  "model"
names(cali.state) [35] <-  "eng.dscr"
names(cali.state) [36] <-  "eng.dscr.1"
names(cali.state) [41] <-  "manufacturer"

cali.state$eng.dscr.2 <- NA

names(d1984) [5] <-  "model"
names(d1984) [8] <-  "trans"
names(d1984) [23] <-  "eng.dscr"
names(d1984) [24] <-  "eng.dscr.1"
names(d1984) [33:38] <-c( "twodoor.p", "twodoor.l", "fourdoor.p", "fourdoor.l", "hatch.p", "hatch.l")



d1984$eng.dscr.2 <- NA

#Strip Whitespace
cali.state$model <- sapply(cali.state$model, function(x) gsub("^ +| +$", "", x))
cali.state$manufacturer <- sapply(cali.state$manufacturer, function(x) gsub("^ +| +$", "", x))

#Remove Unneeded Variables
calidata$f1 <- NULL
calidata$f2 <- NULL
calidata$f3 <- NULL
calidata$f4 <- NULL
calidata$f5 <- NULL
calidata$f6 <- NULL
calidata$low.alt <- NULL
calidata$high.alt <- NULL
calidata$both.alt <- NULL

statedata$f1 <- NULL
statedata$f2 <- NULL
statedata$f3 <- NULL
statedata$f4 <- NULL
statedata$f5 <- NULL
statedata$f6 <- NULL
statedata$low.alt <- NULL
statedata$high.alt <- NULL
statedata$both.alt <- NULL

cali.state$f1 <- NULL
cali.state$f2 <- NULL
cali.state$f3 <- NULL
cali.state$f4 <- NULL
cali.state$f5 <- NULL
cali.state$f6 <- NULL
cali.state$low.alt <- NULL
cali.state$high.alt <- NULL
cali.state$both.alt <- NULL
cali.state$f.type <- NULL
cali.state$mfg.code <- NULL
cali.state$sc <- NULL
cali.state$b.eng.id <- NULL
cali.state$carline.code <- NULL
cali.state$est.cmb <- NULL
cali.state$fcost <- NULL
cali.state$od <- NULL
cali.state$catalyst <- NULL
cali.state$cmb <- NULL
cali.state$opt.disp <- NULL
cali.state$sale.mtot<- NULL
cali.state$sale.mtotc<- NULL
cali.state$V14 <- NULL
cali.state$V1 <- NULL
cali.state$c.code <- NULL
cali.state$code.2 <- NULL
cali.state$mfg.name <- NULL
cali.state$cty <- NULL
cali.state$hwy <- NULL
cali.state$est.hwy <- NULL
cali.state$hpv <- NULL
cali.state$hlv <- NULL
cali.state$class.y<-NULL


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


bind<- rbind(cali.state, d1984)
bind<- bind[order(bind[,"year"]),]

#Write data as a CSV file                       
write.table(bind, file="1978-1984mpg.csv", sep=",", col.names=TRUE, row.names=FALSE)



#The below wite.table calls are to write the other data sets to a csv as needed
#write.table(d1984, file="1984mpg.csv", sep=",", col.names=TRUE, row.names=FALSE)
#write.table(statedata, file="1978-1983statempg.csv", sep=",", col.names=TRUE, row.names=FALSE)
#write.table(calidata, file="1978-1983calimpg.csv", sep=",", col.names=TRUE, row.names=FALSE)
#write.table(cali.state, file="comb-state-calimpg.csv", sep=",", col.names=TRUE, row.names=FALSE)


