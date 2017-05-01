suppressMessages({  
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(RColorBrewer)
  library(maps)
})

cols <- c("COW", "POBP", "ANC1P", "RAC2P", "DECADE", "ST", "SEX", "YOEP", "AGEP", "INDP", "SCHL", "WKHP", "PINCP")
pusa <- fread("/Users/Erik/Desktop/Topics In Cinema Project/TC_PROJ/2013-american-community-survey/pums/ss13pusa.csv", select = cols)
pusb <- fread("/Users/Erik/Desktop/Topics In Cinema Project/TC_PROJ/2013-american-community-survey/pums/ss13pusb.csv", select = cols)


#binding together
pus <- bind_rows(pusa, pusb)
rm(pusa, pusb)
gc()

str(pus)

# COW(Class of worker)
pus$COW <- factor(pus$COW)
levels(pus$COW) <- c("Private profit", "Private non-profit", "Local government", "State government", "Federal government", "Self-employed", "Self-employed", "Working without pay", "Unemployed")

# DECADE(Decade of entry)
pus$DECADE <- factor(pus$DECADE)
levels(pus$DECADE) <- c("~1950's", "1950's", "1960's", "1970's", "1980's", "1990's", "2000's~")

str(pus)

# ST(State Code)
pus$ST <- as.factor(pus$ST)
levels(pus$ST) <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut",
                    "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois",
                    "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts",
                    "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", 
                    "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
                    "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota",
                    "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia",
                    "Wisconsin", "Wyoming", "Puerto Rico")

# SEX
pus$SEX <- factor(pus$SEX)
levels(pus$SEX) <- c("Male", "Female")

# INDP(Industry recode for 2013 and later based on 2012 IND codes)
pus$INDP <- ifelse(pus$INDP >= 170 & pus$INDP <= 290, 170, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 370 & pus$INDP <= 490, 370, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 570 & pus$INDP <= 770, 570, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 1070 & pus$INDP <= 3990, 1070, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 4070 & pus$INDP <= 6390, 4070, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 6470 & pus$INDP <= 6780, 6470, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 6870 & pus$INDP <= 7190, 6870, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 7270 & pus$INDP <= 7790, 7270, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 7860 & pus$INDP <= 7890, 7860, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 7970 & pus$INDP <= 8290, 7970, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 8370 & pus$INDP <= 8470, 8370, pus$INDP)
pus$INDP <- ifelse(pus$INDP %in% c(8660, 8680, 8690), 8370, pus$INDP) 
pus$INDP <- ifelse(pus$INDP >= 8770 & pus$INDP <= 9290, 8370, pus$INDP)
pus$INDP <- ifelse(pus$INDP %in% c(8560, 8570, 8580, 8590, 8670), 8560, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 9370 & pus$INDP <= 9590, 9370, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 9670 & pus$INDP <= 9870, 9670, pus$INDP)
pus$INDP <- ifelse(pus$INDP >= 9920, 9920, pus$INDP)
pus$INDP <- factor(pus$INDP)
levels(pus$INDP) <- c("Agriculture, Forestry, Fishing, Hunting", "Mining", "Utilities, Construction", 
                      "Manufacturing", "Trade, Logistic", "Information, Communications", "Finance",
                      "Professional", "Education", "Health", "Other Services",
                      "Arts, Entertainment", "Public Administration", "Military", "Unemployed"
)

# SCHL(Educational attainment)
pus$SCHL <- ifelse(pus$SCHL <= 16, 16, pus$SCHL)
pus$SCHL <- ifelse(pus$SCHL >= 17 & pus$SCHL <= 19, 19, pus$SCHL)
pus$SCHL <- factor(pus$SCHL)
levels(pus$SCHL) <- c("High school or lower", "Some college", "Associate", "Bachelor", "Master", "Professional", "Doctorate")



#working Hours Analysis
WH <- pus[, c("COW", "POBP", "ANC1P", "WKHP","PINCP")]
WH$NATION <- rep(NA)
WH$NATION <- ifelse(WH$POBP == 217 & WH$ANC1P ==750, "Korea", WH$NATION)
WH$NATION <- ifelse(WH$POBP == 214 & WH$ANC1P == 419, "Israel", WH$NATION)
WH$NATION <- ifelse(WH$POBP == 110 & WH$ANC1P == 32, "German", WH$NATION)
WH$NATION <- ifelse(WH$POBP == 303 & WH$ANC1P == 210, "Mexico", WH$NATION)
WH$NATION <- ifelse(WH$POBP == 136 & WH$ANC1P == 89, "Sweden", WH$NATION)


#average work hours per immigration group
ggplot(filter(WH) , aes(x=NATION, y=WKHP)) + 
  geom_boxplot(aes(fill=NATION), size=0.3) +
  stat_summary(fun.y=mean, geom="point", shape=23, size=3) + 
  ggtitle("Working hours per week")


WH %>% 
  group_by(NATION) %>%
  select(WKHP) %>% 
  summarise(Avg.Work.Hour=mean(WKHP, na.rm=T))


str(WH$NATION)
#average salary per immigration group

averageImmigrantSalleries<-aggregate(WH$PINCP, by=list(WH$NATION), FUN="mean", na.rm=TRUE)
d1<-data.frame(averageImmigrantSalleries)
str(d1)

ggplot(data=d1, 
       aes(x=Group.1,y = x,fill=factor(Group.1), col=I("grey")))+geom_histogram(stat = "identity")+
  labs(x = "Immigrant Nationalities",y = "Average Yearly Income")


#____________________________________________________________________#____________________________________________________________________

#working Hours of self employed groups

WHSE <- filter(WH, COW == "Self-employed")

ggplot(filter(WHSE), aes(x= NATION, y= WKHP)) + 
  geom_boxplot(aes(fill = NATION), alph= 0.5) +
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) + 
  ggtitle("Working hours of self-employer per week")


#____________________________________________________________________#____________________________________________________________________

#study particualr groups of immigrants
#to understand the trends surrounding the phenomenon lets look at the Koreans

pus.kor <- filter(pus, POBP == 217 & RAC2P == 49) # POBP : 217(Korea), RAC3P : 49(Korean)

# DECADE (Age distribution in immigrants during different immigration years)
ggplot(pus.kor, aes(AGEP, group=DECADE)) + 
  geom_bar(aes(colour=DECADE, fill=DECADE), binwidth=1, alpha=0.9) +
  xlab("Age") + ylab("Count") + ggtitle("Age by Decade")

table(pus.kor$DECADE)

#____________________________________________________________________#____________________________________________________________________

all_state <- map_data("state")
data <- as.data.frame(prop.table(table(pus.kor$ST)))
data$state <- sort(tolower(c("district of columbia","Puerto Rico", state.name)))
all_state$freq <- data$Freq[match(all_state$region, data$state)]*100

p <- ggplot(all_state, aes(x=long, y=lat, group=group)) + 
  geom_polygon(aes(fill=freq), colour="gray78") + 
  scale_fill_gradient(name="Proportion", low="white", high="blueviolet")
p <- p + theme(strip.background = element_blank(),
               strip.text.x     = element_blank(),
               axis.text.x      = element_blank(),
               axis.text.y      = element_blank(),
               axis.ticks       = element_blank(),
               axis.line        = element_blank(),
               panel.background = element_blank(),
               panel.border     = element_blank(),
               panel.grid       = element_blank(),
               legend.position  = "right") +
  xlab("") + ylab("") + ggtitle("Avg. Number of Korean by State")
p

#funy Fact look at working hours inn the most common state law
#Dicrimination laws regarding salary.

st.df <- pus.kor %>% group_by(ST) %>% summarise(Count=n()) %>% arrange(desc(Count))
st.df$Prop <- round(st.df$Count / sum(st.df$Count) * 100, 1)
head(st.df, 20)

#____________________________________________________________________#____________________________________________________________________

pus.kor$AGEY <- pus.kor$AGEP - (2013-pus.kor$YOEP) + 2
pus.kor$AGEG <- rep(0, nrow(pus.kor))
pus.kor$AGEG <- ifelse(pus.kor$AGEY >= 0 & pus.kor$AGEY < 10, 0, pus.kor$AGEG)
pus.kor$AGEG <- ifelse(pus.kor$AGEY >= 10 & pus.kor$AGEY < 20, 1, pus.kor$AGEG)
pus.kor$AGEG <- ifelse(pus.kor$AGEY >= 20 & pus.kor$AGEY < 30, 2, pus.kor$AGEG)
pus.kor$AGEG <- ifelse(pus.kor$AGEY >= 30 & pus.kor$AGEY < 40, 3, pus.kor$AGEG)
pus.kor$AGEG <- ifelse(pus.kor$AGEY >= 40 & pus.kor$AGEY < 50, 4, pus.kor$AGEG)
pus.kor$AGEG <- ifelse(pus.kor$AGEY >= 50 & pus.kor$AGEY < 60, 5, pus.kor$AGEG)
pus.kor$AGEG <- ifelse(pus.kor$AGEY >= 60, 6, pus.kor$AGEG)
pus.kor$AGEG <- factor(pus.kor$AGEG)
levels(pus.kor$AGEG) <- c("0's", "10's", "20's", "30's", "40's", "50's", "60's~")

ggplot(pus.kor, aes(x=DECADE)) + 
  geom_bar(aes(fill=AGEG), position="fill") +
  ggtitle("Age Group at Immigrant Year")

#____________________________________________________________________#____________________________________________________________________

color <- scales::alpha(c(brewer.pal(n = 9, name = "Pastel1"), brewer.pal(n = 6, name = "Set3")), 0.9)
color[2:3] <- c("plum1", "purple")
ggplot(filter(pus.kor, is.na(INDP) == F), aes(x = DECADE)) + 
  geom_bar(aes(fill = INDP), position="fill") +
  scale_fill_manual(values = color) +
  ylab("RATIO") + ggtitle("Industry Distribution by Decade")

#____________________________________________________________________#____________________________________________________________________

color <- scales::alpha(brewer.pal(n = 7, name = "Pastel1"), 0.8)
color[6:7] <- c("orange", "firebrick1")
ggplot(filter(pus.kor, is.na(SCHL) == F & AGEP >= 35), aes(x=DECADE)) + 
  geom_bar(aes(fill=SCHL), position="fill") + 
  scale_fill_manual(values = color) +
  ylab("RATIO") + ggtitle("Educational Degree")


#____________________________________________________________________#____________________________________________________________________

income.df <- pus.kor %>% filter(is.na(INDP) == F) %>% group_by(DECADE, INDP) %>% summarise(INCOME = mean(PINCP))

income.df$INDP <- factor(income.df$INDP, levels = levels(income.df$INDP)[length(levels(income.df$INDP)):1])
ggplot(income.df, aes(x=DECADE)) + geom_point(aes(y=INDP, size=INCOME, colour=INDP)) +
  ggtitle("Avg. Income by Industry and Immigrant Period") +
  guides(colour=FALSE) + theme_minimal()

income.df$INDP <- factor(income.df$INDP, levels = levels(income.df$INDP)[length(levels(income.df$INDP)):1])
data.frame(spread(income.df, DECADE, INCOME))
