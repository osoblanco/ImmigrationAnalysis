popData <- read.csv("/Users/Erik/Desktop/Topics In Cinema Project/TC_PROJ/2013-american-community-survey/pums/ss13pusa.csv")[,c('SEX','RAC1P','SCHL','WAGP','NATIVITY','FLANXP','AGEP','LANP')] 
popData2<- read.csv("/Users/Erik/Desktop/Topics In Cinema Project/TC_PROJ/2013-american-community-survey/pums/ss13pusb.csv")[,c('SEX','RAC1P','SCHL','WAGP','NATIVITY','FLANXP','AGEP','LANP')] 

print(names(popData))

popData <- rbind(popData,popData2)
popData2 <-NULL


##Get 2nd Gens only
dim(popData)
popData <- popData[which(popData$AGEP > 25),]
dim(popData)
nativeData <- popData[which(popData$NATIVITY==1),]


genTwoData <- nativeData[which(nativeData$RAC1P==6),]

print("genTwoData")
dim(genTwoData)

##Employed Only

genTwoData <- genTwoData[which(genTwoData$WAGP > 0),]

boxplot(WAGP~FLANXP,data=genTwoData,xlab="1 English spoken at home, 0 other language spoken at home",main="Annual Income")
boxplot(SCHL~FLANXP,data=genTwoData,xlab="1 English spoken at home, 0 other language spoken at home",main="Educational Attainment, where 16=High School and 21=B.S.")

aggregate(NATIVITY~FLANXP,FUN=sum,data=genTwoData)

t.test(WAGP~FLANXP,data=genTwoData)
t.test(SCHL~FLANXP,data=genTwoData)

aggregate(WAGP~FLANXP,FUN=median,data=genTwoData)
aggregate(SCHL~FLANXP,FUN=median,data=genTwoData)