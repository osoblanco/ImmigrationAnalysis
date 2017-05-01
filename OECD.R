library(caret)
library(caTools)
library(ROCR)
library(class)
library(MASS)
library(e1071)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)
library(party)
library(kernlab)
library(ggplot2)


Data_OECD<-read.csv("MIG_30042017005105545.csv")
str(Data_OECD)
Data_OECD["YEA"]<-NULL
Data_OECD<-na.omit(Data_OECD)
Data_OECD["Flag.Codes"]<-NULL
Data_OECD["Flags"]<-NULL
str(Data_OECD)

X <- split(Data_OECD, Data_OECD$VAR)
str(X)
Data1<-data.frame(X[1]) #inflow of foreign population by nationality
Data2<-data.frame(X[2]) #outflow of foreign population by nationality
Data3<-data.frame(X[3]) #inflow of asylum seekers by nationality (Refugees)
Data4<-data.frame(X[4]) #stock of foreign-born population by the country of birth
Data5<-data.frame(X[5]) #stock of foreign-born population by nationality
Data6<-data.frame(X[6]) # NA
Data7<-data.frame(X[7]) #stock of foreign-born labour by the country of birth
Data8<-data.frame(X[8]) #stock of foreign-born labour by nationality

Data1<-Data1[Data1$B11.Country.of.birth.nationality != "Total",]
Data2<-Data2[Data2$B12.Country.of.birth.nationality != "Total",]
Data3<-Data3[Data3$B13.Country.of.birth.nationality != "Total",]
Data4<-Data4[Data4$B14.Country.of.birth.nationality != "Total",]
Data5<-Data5[Data5$B15.Country.of.birth.nationality != "Total",]
Data6<-Data6[Data6$B16.Country.of.birth.nationality != "Total",]
Data7<-Data7[Data7$B23.Country.of.birth.nationality != "Total",]
Data8<-Data8[Data8$B24.Country.of.birth.nationality != "Total",]


#____________________________________________________________________#____________________________________________________________________

averagePerCountryImmigrationYears<-aggregate(Data1$B11.Value, by=list(Data1$B11.Year), FUN="mean", na.rm=TRUE)
d1<-data.frame(averagePerCountryImmigrationYears)
ggplot(data=d1, 
    aes(x=Group.1,y = x,fill=factor(Group.1), col=I("grey")))+geom_histogram(stat = "identity")+
    labs(x = "Countries",y = "Immigrant Average Per country")


AllImmigrationInYears<-aggregate(Data1$B11.Value, by=list(Data1$B11.Year), FUN="sum", na.rm=TRUE)
d1<-data.frame(AllImmigrationInYears)
ggplot(data=d1, 
    aes(x=Group.1,y = x,fill=factor(Group.1), col=I("grey")))+geom_histogram(stat = "identity")+
    labs(x = "Countries",y = "Immigrant ammount per year in the world")


#____________________________________________________________________#____________________________________________________________________

# Intake from all the others averaged
averageImmigrationToCountriesFromAllOthers<-aggregate(Data1$B11.Value, by=list(Data1$B11.Country), FUN="mean", na.rm=TRUE)
d1<-data.frame(averageImmigrationToCountries)
str(d1)

ggplot(data=d1, 
    aes(x=Group.1,y = x,fill=factor(Group.1), col=I("grey")))+geom_histogram(stat = "identity")+
    labs(x = "Countries",y = "Immigrant Average intake from all others Per country")

Top10<-d1[rev(order(d1$x)),"Group.1"][1:10]
d_s <- subset(d1, Group.1 %in% Top10)

#d_s[8,]<-c("RandomGen",0)
#d_s<-na.omit(d_s)

ggplot(data=d_s, 
    aes(x=Group.1,y = x,fill=factor(Group.1), col=I("grey")))+geom_histogram(stat = "identity")+
    labs(x = "Countries",y = "Immigrant Average intake from all others Per country")

#____________________________________________________________________#____________________________________________________________________

#Total Intake Per Country
averageImmigrationToCountriesFromAllOthers<-aggregate(Data1$B11.Value, by=list(Data1$B11.Country), FUN="sum", na.rm=TRUE)
d1<-data.frame(averageImmigrationToCountries)
str(d1)

ggplot(data=d1,
    aes(x=Group.1,y = x,fill=factor(Group.1), col=I("grey")))+
    geom_histogram(stat = "identity")+labs(xlab = "Countries",ylab = "Immigrant Total Intake Per country")

Top10<-d1[rev(order(d1$x)),"Group.1"][1:10]
d_s <- subset(d1, Group.1 %in% Top10)

#d_s[8,]<-c("RandomGen",0)
#d_s<-na.omit(d_s)

ggplot(data=d_s,
    aes(x=Group.1,y = x,fill=factor(Group.1), col=I("grey")))+
    geom_histogram(stat = "identity")+labs(xlab = "Countries",ylab = "Immigrant Total Intake Per country")

#____________________________________________________________________#____________________________________________________________________

StockOfFereignPeopleByPlaceOfBirth<-aggregate(Data4$B14.Value, by=list(Data4$B14.Country), FUN="sum", na.rm=TRUE)
d1<-data.frame(StockOfFereignPeopleByPlaceOfBirth)
ggplot(data=d1, 
    aes(x=Group.1,y = x,fill=factor(Group.1), col=I("grey")))+
    geom_histogram(stat = "identity")+labs(x = "Countries",y = "Foreigners By birth place ammount")


Top10<-d1[rev(order(d1$x)),"Group.1"][1:10]
d_s <- subset(d1, Group.1 %in% Top10)
ggplot(data=d_s, 
    aes(x=Group.1,y = x,fill=factor(Group.1), col=I("grey")))+
    geom_histogram(stat = "identity")+labs(x = "Countries",y = "Foreigners By birth place ammount")

#____________________________________________________________________#____________________________________________________________________

StockOfFereignPeopleByPlaceOfBirth<-aggregate(Data7$B23.Value, by=list(Data7$B23.Country), FUN="sum", na.rm=TRUE)
d1<-data.frame(StockOfFereignPeopleByPlaceOfBirth)
ggplot(data=d1, 
    aes(x=Group.1,y = x,fill=factor(Group.1), col=I("grey")))+ 
    geom_histogram(stat = "identity")+labs(x = "Countries",y= "Foreigners By birth place ammount")


Top10<-d1[rev(order(d1$x)),"Group.1"][1:10]
d_s <- subset(d1, Group.1 %in% Top10)
ggplot(data=d_s ,
    aes(x=Group.1, y = x,fill=factor(Group.1),col=I("grey"))) +
    geom_histogram(stat = "identity") +labs(x = "Countries",y= "Foreigners By birth place ammount")

#____________________________________________________________________#____________________________________________________________________

