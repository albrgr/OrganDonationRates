
library("plm")
library("gplots")
library("ggplot2")
library("MASS")
library("stargazer")
library("lmtest")

##Inputting, merging, prepping data
setwd("~/Documents/Stuff/Nephrology/organ donor narratives")
Papers <- read.csv("rawregistrydataquarterly.csv", header = T )
names(Papers)
Papers$X = NULL
Papers$X.1 = NULL
Papers$X.2 = NULL
Papers=Papers[-56,]
Papers=Papers[-56,]
Papers=Papers[-3,]

recovPapers = Papers[c(T,F)]
recovPapers = recovPapers[,18:47]
desigPapers = Papers[c(F,T)]
desigPapers = desigPapers[,17:46]
names(Papers)
Papers = Papers[,1:33]
Papers = cbind(Papers, desigPapers, recovPapers)
names(Papers)
Papers = Papers[ ,-3:-8]
Papers$NA1 <- NA
Papers$NA2 <- NA
Papers = Papers[ -1:-2,]
names(Papers)
l <- reshape(Papers,
             idvar = "State",
             varying = list(
               c(3:27,7,11,15,19,23,27), 
               c(88,28:57),
               c(89,58:87)), 
             v.names = c("uniqueDD","DesigDonors","RecovDonors"),
             timevar = "quarter", 
             times = c(0:24,"2007","2008","2009","2010","2011","2012"), 
             direction = "long")
names(l)

statepops <- read.csv("Statepopulationscensus.csv", header = T )
names(statepops)
fillquarters <- function(x){
  k <- length(x)
  for(i in 1:(k-1)) {
    q4 <- rowMeans(cbind(x[i],x[i+1]))
    q3 <- rowMeans(cbind(x[i],q4))
    q1 <- rowMeans(cbind(q4,x[i+1]))
    x <- cbind(x,q3)
    x <- cbind(x,q4)
    x <- cbind(x,q1)
    l <- length(names(x))
    colnames(x)[(l-2):l] <- 
      cbind(paste(i+2005,"q3", sep=""),paste(i+2005,"q4", sep=""),paste(i+2006,"q1", sep=""))
  }
  x$"2012q3" <- x$X2012-x$"2012q1" + x$X2012
  x$"2012q4" <- 2*(x$X2012-x$"2012q1") + x$X2012
  colnames(x)[1:7] <- 
    cbind("2006q2","2007q2","2008q2","2009q2","2010q2","2011q2","2012q2")
  return(x)
}
test <- fillquarters(statepops[,2:8])
statepops <- cbind(statepops[,1],test)
colnames(statepops)[1] <- "State"
statepops$"2006q2" <- NULL
statepops$"2006q3" <- NULL
statepops <- statepops[,order(names(statepops))]
pops <- reshape(statepops,
           idvar = "State",
           varying = c(1:25,5,9,13,17,21,25), 
           v.names = c("population"),
           timevar = "quarter", 
           times = c(0:24,2007:2012), 
           direction = "long")

v <- merge(l,pops,by.x=c("State.2","quarter"),by.y=c("State","quarter"), all = TRUE)

eligibledeaths <- read.csv("EligibleDeathsfromLatestMonthly.csv", header = T )
elg <- reshape(eligibledeaths,
               idvar = "State",
               varying = c(2:20), 
               v.names = c("eligibledeaths"),
               timevar = "quarter", 
               times = c(9:24,0,2007,2008), 
               direction = "long")

deceaseddonors <- read.csv("DeceasedDonorsfromLatestMonthly.csv", header = T )
dd <- reshape(deceaseddonors,
               idvar = "State",
               varying = c(2:20), 
               v.names = c("deceaseddonors"),
               timevar = "quarter", 
               times = c(9:24,0,2007,2008), 
               direction = "long")

adtldeceaseddonors <- read.csv("AdtlDeceasedDonorsfromLatestMonthly.csv", header = T )
add <- reshape(adtldeceaseddonors,
               idvar = "State",
               varying = c(2:20), 
               v.names = c("adtldeceaseddonors"),
               timevar = "quarter", 
               times = c(9:24,0,2007,2008), 
               direction = "long")

Latestmonthlydata <- cbind(elg,dd[3],add[3])

aggdata <- v
aggdata[,4:7] <- data.frame(lapply(aggdata[,4:7], as.character), stringsAsFactors=FALSE)
aggdata[,4:7] <- data.frame(lapply(aggdata[,4:7], as.numeric), stringsAsFactors=FALSE)
aggdata[is.na(aggdata)] <- 0
quarterlytotals <- aggregate(aggdata[,4:7], by=list(aggdata$quarter), FUN=sum, na.rm=TRUE)

analysisdata <- merge(v,Latestmonthlydata,by=c("State","quarter"), all = TRUE)
analysisdata$quarter <- as.numeric(as.character(analysisdata$quarter))
analysisdata <- subset(analysisdata, analysisdata["quarter"]>8 | analysisdata["quarter"]<1)

analysisdata$quarter[analysisdata$quarter == 2007] <- 4

analysisdata$quarter[analysisdata$quarter == 2008] <- 8

analysisdata[!complete.cases(analysisdata[3]),]


analysisdata <- analysisdata[!is.na(analysisdata$State.2),]
analysisdata[is.na(analysisdata$eligibledeaths),]
analysisdata <- subset(analysisdata, analysisdata["quarter"]<1000)
analysisdata[is.na(analysisdata$RecovDonors),]
analysisdata[,4:10] <- data.frame(lapply(analysisdata[,4:10], as.character), stringsAsFactors=FALSE)
analysisdata[,4:10] <- data.frame(lapply(analysisdata[,4:10], as.numeric), stringsAsFactors=FALSE)
analysisdata$propregistered <- analysisdata$uniqueDD / analysisdata$population
analysisdata$propdonors <- analysisdata$deceaseddonors / analysisdata$eligibledeaths
analysisdata$totaldonors <- analysisdata$deceaseddonors + analysisdata$adtldeceaseddonors
summary(analysisdata)
analysisdata[!complete.cases(analysisdata$State),]
analysisdata[analysisdata$propregistered > .4 & analysisdata$quarter == 0,]

##Figures 1 and 2

aggdata <- analysisdata
aggdata[is.na(aggdata)] <- 0
str(aggdata)
quarterlytotals <- aggregate(aggdata[,4:13], by=list(aggdata$quarter), FUN=sum, na.rm=TRUE)
str(quarterlytotals)
quarterlytotals$propregistered <- quarterlytotals$uniqueDD / quarterlytotals$population
quarterlytotals$propdonors <- quarterlytotals$deceaseddonors / quarterlytotals$eligibledeaths
quarterlytotals$totaldonors <- quarterlytotals$deceaseddonors + quarterlytotals$adtldeceaseddonors
quarterlytotals$proptotaldonors <- quarterlytotals$totaldonors / quarterlytotals$eligibledeaths
quarterlytotals$uniqueDD <- quarterlytotals$uniqueDD / 1000000
quarterlytotals$totaldonors[quarterlytotals$Group.1 < 9] <- quarterlytotals$totaldonors[quarterlytotals$Group.1 < 9] / 4
quarterlytotals$Group.1 <- quarterlytotals$Group.1/4 + 2006.5

png(filename="OrganDonor1.png")
par(mar=c(5,4,4,5)+.1)
plot(quarterlytotals$Group.1, quarterlytotals$totaldonors, type="l", lwd = 3,
     col="red", xlab="", ylab="Total Deceased Organ Donors per Quarter", ylim=c(0,2100))
par(new=TRUE)
plot(quarterlytotals$Group.1, quarterlytotals$uniqueDD, type="l", lwd = 3,
     col="green",xaxt="n",yaxt="n",xlab="",ylab="", ylim=c(0,110))

axis(4)
mtext("Millions of Registered Organ Donors in the US",side=4,line=3)
legend("bottomleft",col=c("red","green"),lty=1, lwd=3,
       legend=c("# of Deceased Organ Donors per Quarter (left axis)",
                "Millions of Registered Organ Donors (right axis)"))
dev.off()

png(filename="OrganDonor2.png")
par(mar=c(5,4,4,5)+.1)
plot(quarterlytotals$Group.1, quarterlytotals$propdonors, type="l", lwd = 3, ylim=c(0,1),
     col="red", xlab="", ylab="Proportion of Eligible Donors Actually Donating")
par(new=TRUE)
plot(quarterlytotals$Group.1, quarterlytotals$uniqueDD, type="l", lwd = 3,
     col="green",xaxt="n",yaxt="n",xlab="",ylab="", ylim=c(0,110))

axis(4)
mtext("Millions of Registered Organ Donors in the US",side=4,line=3)
legend("bottomleft",col=c("red","green"),lty=1, lwd=3,
       legend=c("Proportion of Eligible Donors Donating (left axis)",
                "Millions of Registered Organ Donors (right axis)"))
dev.off()

##Figures 3-5

analysisdata$quarter <- analysisdata$quarter/4 + 2006.5

png(filename="OrganDonor3.png", width = 618, height = 656, units = 'px')
p <- ggplot(analysisdata, aes(quarter, propregistered)) + xlab("") +
  ylab("Registered Donors as a Proportion of State Population") +
  ggtitle("Registered Donors as a Proportion of State Populations since 2006") +
  geom_line(aes(color=State)) + facet_wrap(~State, ncol = 6) + theme(legend.position = "none")
print(p)
dev.off()

png(filename="OrganDonor4.png", width = 618, height = 656, units = 'px')
p1 <- ggplot(analysisdata, aes(quarter, propdonors))  + xlab("") +
  ylab("Proprtion of Eligible Deceased Donors Actually Donating") +
  ggtitle("Eligible Donor Conversion Rate by State since 2006") +
  geom_line(aes(color=State)) + facet_wrap(~State, ncol = 6) + theme(legend.position = "none")
print(p1)
dev.off()

png(filename="OrganDonor5.png", width = 618, height = 656, units = 'px')
q <- ggplot(analysisdata, aes(propregistered, propdonors)) + 
  xlab("Registered Donors as a Proportion of State Population") +
  ylab("Proprtion of Eligible Deceased Donors Actually Donating") +
  ggtitle("Quarterly Data since 2006") +
  geom_point(aes(color=State)) + facet_wrap(~State, ncol = 6) + 
  theme(legend.position = "none") 
print(q)
dev.off()

## Regressions

# Convert annual figures for 2006, 2007, and 2008 to their quarterly equivalents

analysisdata$deceaseddonors[analysisdata$quarter < 2008.7] <- 
  analysisdata$deceaseddonors[analysisdata$quarter < 2008.7] / 4

analysisdata$eligibledeaths[analysisdata$quarter < 2008.7] <- 
  analysisdata$eligibledeaths[analysisdata$quarter < 2008.7] / 4

# Divide population figures by a million

analysisdata$population <- analysisdata$population/1000000
analysisdata$uniqueDD <- analysisdata$uniqueDD/1000000

# Create a panel

quarterlydata <- pdata.frame(analysisdata, c("State","quarter"))

summary(quarterlydata)

# Run regressions

m1 <- lm(deceaseddonors ~ eligibledeaths + population + uniqueDD,
         data=analysisdata)
summary(m1)
m1se <- coeftest(m1, vcov.=vcovHC(m1, method="white2", type="HC1"))[,2]

m2 <- plm(deceaseddonors ~ eligibledeaths + population + uniqueDD,
         data=quarterlydata, effect="twoways", model="within")
summary(m2)
m2se <- coeftest(m2, vcov.=vcovHC(m2, method="white2", type="HC1"))[,2]

m3 <- plm(propdonors ~ propregistered, 
         data=quarterlydata, effect="twoways", model="within")
summary(m3)
m3se <- coeftest(m3, vcov.=vcovHC(m3, method="white2", type="HC1"))[,2]

mtable123 <- stargazer(m1,m2,m3, se=list(m1se, m2se, m3se), 
                       star.cutoffs=c(0.05,0.01,0.001), 
                       covariate.labels = c("# of Eligible Deaths",
                                            "State Population (millions)",
                                            "# of Designated Donors (millions)",
                                            "Proportion Reg Donors "),
                       dep.var.labels = c("Number of Deceased Donors per Quarter",
                                          "Conversion Rate"))
##Add to table:
##State and quarter fixed effects & No & Yes & Yes \\ 
##& & & \\ 
##
