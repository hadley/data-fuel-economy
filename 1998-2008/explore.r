library(ggplot2)

mpg <- read.csv("mpg.csv", stringsAsFactors = FALSE)

sentra <- 
sentra$trans <- factor(sentra$trans)

qplot(year, cty, data=sentra)

qplot(year, cty, data=subset(mpg, manufacturer == "toyota"), colour=displ, group=interaction(model, trans,displ), geom=c("jitter","line"), facets = . ~ class)



mpgm <- melt(mpg, id = c("class","manufacturer", "model", "fl", "displ", "trans", "cyl", "year"), measure = c("cty", "hwy"))

cast(mpgm, class + year ~ variable, mean)


qplot(year, hwy, data=cast(mpgm, class + year ~ variable, mean), group=class, geom="line", colour=reorder(factor(class), hwy))


ggplot(mpg, aes(displ, hwy)) + geom_point()
ggplot(mpg, aes(displ, hwy)) + geom_point(colour = alpha("black", 1/10))
ggplot(mpg, aes(displ, hwy)) + geom_jitter(colour = alpha("black", 1/10))
ggplot(mpg, aes(displ, hwy)) + geom_jitter(colour = alpha("black", 1/50))

ggplot(mpg, aes(1 / displ, hwy)) + geom_point()

ggplot(mpg, aes(displ, hwy)) + geom_point() + xlim()
ggplot(mpg, aes(displ, hwy)) + geom_point(colour = alpha)



mpgt <- cast(mpgm, ... ~ trans, mean)