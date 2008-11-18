widths84 <- c(4,1,2,3,28,4,2,6,2,12,6,3,5,5,32,1,15,4,4,5,8,8,10,10,10,10,10,10,1,4)

d1984 <- read.fwf("84MFGUI.DAT", widths = widths84, stringsAsFactors = FALSE)

colmap84 <- read.csv(file="colnames.csv", header=TRUE)
names(d1984) <- colmap84$abbr


trim_all <- function(df) {
  char <- sapply(df, is.character)
  df[char] <- lapply(df[char], function(x) gsub("^ +| +$", "", x))
  df
}
d1984 <- trim_all(d1984)


d1984$T  <- F
d1984$T[grep("TU?RBO", d1984$eng.dscr.1)] <- T
d1984$T[grep("TU?RBO", d1984$eng.dscr.2)] <- T

d1984$G <- F
d1984$G[grep("GUZZLER",d1984$eng.dscr.1)] <- T
d1984$G[grep("GUZZLER", d1984$eng.dscr.2)] <- T

d1984$S <- F

trans <- c(
  "A3" =     "auto(3)",
  "A4" =     "auto(4)",
  "M3" =     "manual(m3)",
  "M3/M4C" = "manual(m3/m4C)",
  "M4" =     "manual(m4)",
  "M4C" =    "manual(m4C)",
  "M4X2" =   "manual(m4X2)",
  "M5" =     "manual(m5)"
)
d1984$trans <- unname(trans[d1984$trans])

d1984$eng.dscr.1[is.na(d1984$eng.dscr.1)] <- ""
d1984$eng.dscr.2[is.na(d1984$eng.dscr.2)] <- ""
d1984$eng.dscr <- paste(d1984$eng.dscr.1, d1984$eng.dscr.2)

# Extract passenger and luggage volumes
d1984$twodoor.p <-   as.numeric(substr(d1984$bt.2,6,7))
d1984$twodoor.l <-   as.numeric(substr(d1984$bt.2,9,10))
d1984$fourdoor.p <-  as.numeric(substr(d1984$bt.4,5,7))
d1984$fourdoor.l <-  as.numeric(substr(d1984$bt.4,9,10))
d1984$hatch.p <-     as.numeric(substr(d1984$bt.hbk,6,7))
d1984$hatch.l <-     as.numeric(substr(d1984$bt.hbk,9,10))

d1984 <- d1984[c("year", "class", "model", "displ", "trans", "cyl", "manufacturer", "cty", "hwy", "eng.dscr", "T", "G", "S", "twodoor.p", "twodoor.l", "fourdoor.p", "fourdoor.l", "hatch.p", "hatch.l"
)]

write.table(d1984, file = "mpg.csv", sep = ",", row = F)