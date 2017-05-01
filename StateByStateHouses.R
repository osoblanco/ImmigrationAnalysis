
library(data.table)
library(choroplethr)
library(dplyr)
 

colsToKeep <- c("TOIL", "ST", "NP", "VACS") #"REFR", "SINK", "STOV", "BATH", "TEL", "LAPTOP",

houseDataA <- fread("/Users/Erik/Desktop/Topics In Cinema Project/TC_PROJ/2013-american-community-survey/pums/ss13husa.csv", select = colsToKeep)
houseDataB <- fread("/Users/Erik/Desktop/Topics In Cinema Project/TC_PROJ/2013-american-community-survey/pums/ss13husb.csv", select = colsToKeep)

houseData <- rbind(houseDataA, houseDataB)
rm(houseDataA, houseDataB)


# only look at housing units that are vacant for migrant workers
mwHouseData <- filter(houseData,VACS == 7)
mwHouseData <- na.omit(mwHouseData)
  
?group_by
group1 <-group_by(houseData, ST)
?summarize
houseCounts = summarize(group1, count = n(), st = ST)  
mwHouseCounts = summarise(group_by(mwHouseData, ST), count = n(), st = ST)

perHousesForMigrantWorkers <- mwHouseCounts$count / houseCounts$count * 100

mwDataByState = data.table(cbind( st=mwHouseCounts$ST, perHousesForMigrantWorkers=perHousesForMigrantWorkers))

mwDataByState

stateCodeCSV = "st,region
001,alabama
002,alaska
004,arizona
005,arkansas
006,california
008,colorado
009,connecticut
010,delaware
011,district of columbia
012,florida
013,georgia
015,hawaii
016,idaho
017,illinois
018,indiana
019,iowa
020,kansas
021,kentucky
022,louisiana
023,maine
024,maryland
025,massachusetts
026,michigan
027,minnesota
028,mississippi
029,missouri
030,montana
031,nebraska
032,nevada
033,new hampshire
034,new jersey
035,new mexico
036,new york
037,north carolina
038,north dakota
039,ohio
040,oklahoma
041,oregon
042,pennsylvania
044,rhode island
045,south carolina
046,south dakota
047,tennessee
048,texas
049,utah
050,vermont
051,virginia
053,washington
054,west virginia
055,wisconsin
056,wyoming"
stateCodes <- fread(stateCodeCSV)

perHousesMigrantWorkersByState <- right_join(mwDataByState, stateCodes, by.x=c("st"))
perHousesMigrantWorkersByState <- mutate(perHousesMigrantWorkersByState, value=perHousesForMigrantWorkers)

state_choropleth(perHousesMigrantWorkersByState, title = "Percentage of Vacant Housing Units for Migrant Workers by State", num_colors=9)