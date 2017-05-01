# Examine Domestic (48-contiguous state) Migration patterns
library(ggplot2)
library(Hmisc)
library(scales)
library(plyr)
library(ggthemes)
library(maps)
library(data.table)


getStateNames <- function(x) {
  chr.states <- c('alabama','alaska','arizona','arkansas','california','colorado','connecticut','delaware','district of columbia','florida','georgia','hawaii','idaho','illinois','indiana','iowa','kansas','kentucky','louisiana','maine','maryland','massachusetts','michigan','minnesota','mississippi','missouri','montana','nebraska','nevada','new hampshire','new jersey','new mexico','new york','north carolina','north dakota','ohio','oklahoma','oregon','pennsylvania','rhode island','south carolina','south dakota','tennessee','texas','utah','vermont','virginia','washington','west virginia','wisconsin','wyoming','puerto rico')
  names(chr.states) <- as.character(c(1,2,4,5,6,8,9,10,11,12,13,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,44,45,46,47,48,49,50,51,53,54,55,56,72))
  return(mapData(x, chr.states))
}
mapData <- function(x, chr.map) {
  chr.map[as.character(x)]
}

if.na  <- function(x,y=0) ifelse(is.na(x), y, x)

getPersonData <- function(chr.dir = '/Users/Erik/Desktop/Topics In Cinema Project/TC_PROJ/2013-american-community-survey/pums/') {
  dt.person <- rbind(
    fread(paste0(chr.dir, 'ss13pusa.csv')),
    fread(paste0(chr.dir, 'ss13pusb.csv'))
  )
  # easier to write w/ lower case
  setnames(dt.person, tolower(names(dt.person)))
  dt.person[, st:=getStateNames(st)]
  dt.person[, adjinc:=adjinc/1e6]
  dt.person[, pobp:=getPlaceOfBirth(pobp)]
  return(dt.person)
}


getPlaceOfBirth <- function(x) {
  chr.place <- c(`1` = 'Alabama',
                 `2` = 'Alaska',
                 `4` = 'Arizona',
                 `5` = 'Arkansas',
                 `6` = 'California',
                 `8` = 'Colorado',
                 `9` = 'Connecticut',
                 `10` = 'Delaware',
                 `11` = 'District of Columbia',
                 `12` = 'Florida',
                 `13` = 'Georgia',
                 `15` = 'Hawaii',
                 `16` = 'Idaho',
                 `17` = 'Illinois',
                 `18` = 'Indiana',
                 `19` = 'Iowa',
                 `20` = 'Kansas',
                 `21` = 'Kentucky',
                 `22` = 'Louisiana',
                 `23` = 'Maine',
                 `24` = 'Maryland',
                 `25` = 'Massachusetts',
                 `26` = 'Michigan',
                 `27` = 'Minnesota',
                 `28` = 'Mississippi',
                 `29` = 'Missouri',
                 `30` = 'Montana',
                 `31` = 'Nebraska',
                 `32` = 'Nevada',
                 `33` = 'New Hampshire',
                 `34` = 'New Jersey',
                 `35` = 'New Mexico',
                 `36` = 'New York',
                 `37` = 'North Carolina',
                 `38` = 'North Dakota',
                 `39` = 'Ohio',
                 `40` = 'Oklahoma',
                 `41` = 'Oregon',
                 `42` = 'Pennsylvania',
                 `44` = 'Rhode Island',
                 `45` = 'South Carolina',
                 `46` = 'South Dakota',
                 `47` = 'Tennessee',
                 `48` = 'Texas',
                 `49` = 'Utah',
                 `50` = 'Vermont',
                 `51` = 'Virginia',
                 `53` = 'Washington',
                 `54` = 'West Virginia',
                 `55` = 'Wisconsin',
                 `56` = 'Wyoming',
                 `60` = 'American Samoa',
                 `66` = 'Guam',
                 `69` = 'Commonwealth of the Northern Mariana Islands',
                 `72` = 'Puerto Rico',
                 `78` = 'US Virgin Islands',
                 `100` = 'Albania',
                 `102` = 'Austria',
                 `103` = 'Belgium',
                 `104` = 'Bulgaria',
                 `105` = 'Czechoslovakia',
                 `106` = 'Denmark',
                 `108` = 'Finland',
                 `109` = 'France',
                 `110` = 'Germany',
                 `116` = 'Greece',
                 `117` = 'Hungary',
                 `118` = 'Iceland',
                 `119` = 'Ireland',
                 `120` = 'Italy',
                 `126` = 'Netherlands',
                 `127` = 'Norway',
                 `128` = 'Poland',
                 `129` = 'Portugal',
                 `130` = 'Azores Islands',
                 `132` = 'Romania',
                 `134` = 'Spain',
                 `136` = 'Sweden',
                 `137` = 'Switzerland',
                 `138` = 'United Kingdom',
                 `139` = 'England',
                 `140` = 'Scotland',
                 `147` = 'Yugoslavia',
                 `148` = 'Czech Republic',
                 `149` = 'Slovakia',
                 `150` = 'Bosnia and Herzegovina',
                 `151` = 'Croatia',
                 `152` = 'Macedonia',
                 `154` = 'Serbia',
                 `156` = 'Latvia',
                 `157` = 'Lithuania',
                 `158` = 'Armenia',
                 `159` = 'Azerbaijan',
                 `160` = 'Belarus',
                 `161` = 'Georgia',
                 `162` = 'Moldova',
                 `163` = 'Russia',
                 `164` = 'Ukraine',
                 `165` = 'USSR',
                 `168` = 'Montenegro',
                 `169` = 'Other Europe',
                 `200` = 'Afghanistan',
                 `202` = 'Bangladesh',
                 `203` = 'Bhutan',
                 `205` = 'Myanmar',
                 `206` = 'Cambodia',
                 `207` = 'China',
                 `208` = 'Cyprus',
                 `209` = 'Hong Kong',
                 `210` = 'India',
                 `211` = 'Indonesia',
                 `212` = 'Iran',
                 `213` = 'Iraq',
                 `214` = 'Israel',
                 `215` = 'Japan',
                 `216` = 'Jordan',
                 `217` = 'Korea',
                 `218` = 'Kazakhstan',
                 `222` = 'Kuwait',
                 `223` = 'Laos',
                 `224` = 'Lebanon',
                 `226` = 'Malaysia',
                 `229` = 'Nepal',
                 `231` = 'Pakistan',
                 `233` = 'Philippines',
                 `235` = 'Saudi Arabia',
                 `236` = 'Singapore',
                 `238` = 'Sri Lanka',
                 `239` = 'Syria',
                 `240` = 'Taiwan',
                 `242` = 'Thailand',
                 `243` = 'Turkey',
                 `245` = 'United Arab Emirates',
                 `246` = 'Uzbekistan',
                 `247` = 'Vietnam',
                 `248` = 'Yemen',
                 `249` = 'Asia',
                 `253` = 'South Central Asia',
                 `254` = 'Other Asia',
                 `300` = 'Bermuda',
                 `301` = 'Canada',
                 `303` = 'Mexico',
                 `310` = 'Belize',
                 `311` = 'Costa Rica',
                 `312` = 'El Salvador',
                 `313` = 'Guatemala',
                 `314` = 'Honduras',
                 `315` = 'Nicaragua',
                 `316` = 'Panama',
                 `321` = 'Antigua & Barbuda',
                 `323` = 'Bahamas',
                 `324` = 'Barbados',
                 `327` = 'Cuba',
                 `328` = 'Dominica',
                 `329` = 'Dominican Republic',
                 `330` = 'Grenada',
                 `332` = 'Haiti',
                 `333` = 'Jamaica',
                 `339` = 'St. Lucia',
                 `340` = 'St. Vincent & the Grenadines',
                 `341` = 'Trinidad & Tobago',
                 `343` = 'West Indies',
                 `344` = 'Caribbean',
                 `360` = 'Argentina',
                 `361` = 'Bolivia',
                 `362` = 'Brazil',
                 `363` = 'Chile',
                 `364` = 'Colombia',
                 `365` = 'Ecuador',
                 `368` = 'Guyana',
                 `369` = 'Paraguay',
                 `370` = 'Peru',
                 `372` = 'Uruguay',
                 `373` = 'Venezuela',
                 `374` = 'South America',
                 `399` = 'Americas',
                 `400` = 'Algeria',
                 `407` = 'Cameroon',
                 `408` = 'Cape Verde',
                 `412` = 'Congo',
                 `414` = 'Egypt',
                 `416` = 'Ethiopia',
                 `417` = 'Eritrea',
                 `420` = 'Gambia',
                 `421` = 'Ghana',
                 `423` = 'Guinea',
                 `427` = 'Kenya',
                 `429` = 'Liberia',
                 `430` = 'Libya',
                 `436` = 'Morocco',
                 `440` = 'Nigeria',
                 `444` = 'Senegal',
                 `447` = 'Sierra Leone',
                 `448` = 'Somalia',
                 `449` = 'South Africa',
                 `451` = 'Sudan',
                 `453` = 'Tanzania',
                 `454` = 'Togo',
                 `457` = 'Uganda',
                 `459` = 'Democratic Republic of Congo',
                 `460` = 'Zambia',
                 `461` = 'Zimbabwe',
                 `462` = 'Africa',
                 `463` = 'Eastern Africa',
                 `464` = 'Northern Africa',
                 `467` = 'Western Africa',
                 `468` = 'Other Africa',
                 `501` = 'Australia',
                 `508` = 'Fiji',
                 `511` = 'Marshall Islands',
                 `512` = 'Micronesia',
                 `515` = 'New Zealand',
                 `523` = 'Tonga',
                 `527` = 'Samoa',
                 `554` = 'Other US Island Areas')
  chr.place <- tolower(chr.place)
  mapData(x, chr.place)
}

dt.people <- getPersonData()

titleCase <- function(x) gsub("\\b([a-z])([a-z]+)", "\\U\\1\\L\\2" ,x, perl=TRUE)

getCentroid <- function(num.lat, num.long, num.order) {
  x <- num.lat[order(num.order)]
  y <- num.long[order(num.order)]
  N <- length(x)
  
  num.signed.area <- 1/2 * sum(x[-N] * y[-1] - x[-1] * y[-N])
  list(lat = 1/(6*num.signed.area) * sum((x[-N] + x[-1])*(x[-N]*y[-1] - x[-1]*y[-N])),
       long = 1/(6*num.signed.area) * sum((y[-N] + y[-1])*(x[-N]*y[-1] - x[-1]*y[-N])))
}

dt.immigration <- dt.people[st != pobp, list(population = sum(pwgtp)), by=list(state = st)]
dt.emmigration <- dt.people[st != pobp, list(population = sum(pwgtp)), by=list(state = pobp)]
dt.net <- rbind(
  dt.immigration,
  dt.emmigration[, list(state, population = -population)]
)
setnames(dt.net, 'population', 'migration')
dt.net <- dt.net[, list(migration = sum(migration)), by=list(state)]
dt.map <- data.table(map_data('state'))
setnames(dt.map, 'region', 'state')
dt.plot <- merge(dt.net, dt.map, 'state')

num.max <- max(dt.plot$migration)
num.digits <- floor(log(num.max,10))-1
num.max <- ceiling(num.max/10^num.digits) * 10^num.digits
num.breaks <- signif(seq(-num.max, num.max,by=10^num.digits*2),2)
ggplot(dt.plot, aes(x = long, y = lat, fill = migration, group = group)) + 
  geom_polygon(colour = 'black') + 
  coord_map() + 
  scale_fill_gradientn(colours = c('red','white','green'), 
                       values = num.breaks, 
                       breaks = num.breaks, 
                       guide = 'legend', 
                       limits = range(num.breaks),
                       rescaler = function(x,...) x,
                       oob = identity,
                       label = function(x) prettyNum(x, big.mark = ',', scientific = FALSE)) + 
  theme_bw() + 
  labs(x = '', y = '', colour = 'Net Immigration', title = 'Net Immigration by State')
ggsave('Overview.png', width = 10, height = 5)

getStateMigrationPlot <- function(chr.state = 'california', dt.states = data.table(map_data('state')), dt.person = dt.people) {
  
  dt.centroid <- dt.states[is.na(subregion) | subregion %in% c('main','south'), getCentroid(lat, long, order), by=list(region, subregion)]
  
  # All migrations?
  dt.plot <- dt.person[, list(n = sum(pwgtp)), by=list(from = tolower(pobp), to = st)]
  dt.plot[, population:=sum(n), by=list(to)]
  dt.plot <- dt.plot[from != to]
  # For the moment, only domestic
  dt.plot <- dt.plot[from %in% dt.states$region & to %in% dt.states$region]
  
  # Net migrations?
  dt.plot <- rbind(
    dt.plot[from >= to, list(from, to, n)],
    dt.plot[from < to, list(to = from, from = to, n = -n)]
  )
  dt.plot <- dt.plot[, list(n = sum(n)), by=list(from, to)]
  dt.plot <- rbind(
    dt.plot[n >= 0, list(from, to, n)],
    dt.plot[n < 0, list(to = from, from = to, n = -n)]
  )
  dt.plot <- merge(dt.plot, dt.centroid[, list(from = region, lat = lat, long = long)], 'from', all.x = TRUE)
  dt.plot <- merge(dt.plot, dt.centroid[, list(to = region, to_lat = lat, to_long = long)], 'to', all.x = TRUE)
  
  
  chr.state <- tolower(chr.state)
  dt.plot <- dt.plot[from == chr.state | to == chr.state ]
  dt.plot[, type:=ifelse(from == chr.state, 'Emmigration','Immigration')]
  a <- ggplot(dt.plot, aes(x = long, y = lat, xend = to_long, yend = to_lat, alpha = n, colour = type, size = n)) + 
    geom_polygon(data = dt.states[], aes(group = group, xend = NA_real_, yend = NA_real_, colour = NA_character_, alpha = NA_real_), fill = 'grey95', colour = 'black', size = 0.1) + 
    geom_segment(arrow = arrow(length = unit(0.3, 'cm'), type = 'closed')) + 
    # coord_map() + # Can't use coord_map because of a bug in the arrow()
    coord_equal(ratio = 1.4) + 
    theme_bw() + 
    scale_alpha_continuous(range = c(0.5,1), labels = function(x) paste0(round(x/1e3,1),'k')) + 
    scale_size_continuous(range = c(0.5,1.5), labels = function(x) paste0(round(x/1e3,1),'k')) + 
    labs(x = '', y = '', colour = '', size = 'Number of people [net]', alpha = 'Number of people [net]', title = paste0('Migration to/from ', titleCase(chr.state))) + 
    expand_limits(alpha = range(dt.plot$n), size = range(dt.plot$n))
  return(a)
}

ggsave(plot = getStateMigrationPlot(chr.state = 'california'), file = 'california.png', width = 10, height = 5)
ggsave(plot = getStateMigrationPlot('new york'), file = 'new york.png', width = 10, height = 5)
ggsave(plot = getStateMigrationPlot('Alabama'), file = 'alabama.png', width = 10, height = 5)
