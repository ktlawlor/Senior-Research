# Installation of necessary libraries.
install.packages("twitteR")
install.packages("ROAuth")
install.packages("streamR")
install.packages("data.table")
install.packages("ggplot2")
install.packages("maps")
install.packages("ggmap")
library("twitteR")
library("ROAuth")
library("streamR")
library("data.table")
library("ggplot2")
library("maps")
library("ggmap")

# Secret Information
consumer_key <- "SUPER SECRET"
consumer_secret <- "SUPER SECRET"
access_token <- "SUPER SECRET"
access_secret <- "SUPER SECRET"
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)

# Create a dataframe of 5000 tweets with Cats, cats, felines, or #cats
catTweets <- searchTwitter("Cats OR cats OR 'felines' OR #cats", n=5000, lang="en", since="2017-02-21")
catTweets.df <- twListToDF(catTweets) #TUES

catTweets2 <- searchTwitter("Cats OR cats OR 'felines' OR #cats", n=5000, lang="en", since="2017-02-23")
catTweets2.df <- twListToDF(catTweets2) #THUR

# Read the first tweet of the dataframe.
catTweets[1]

#Extract the user name of the all the tweets
sapply(catTweets, function(x) x$getScreenName())

# Extract the text
sapply(catTweets, function(x) x$getText())

catTweetUsers = lookupUsers(catTweets.df$screenName)
catTweetUserLocation = sapply(catTweetUsers, location)

catTweetUsers2 = lookupUsers(catTweets2.df$screenName)
catTweetUsers2Location = sapply(catTweetUsers2, location)

# Remove NAs
#empty.id <- which(catTweetUserLocation[,1] == "")
#catTweetUserLocationDFFilter <- catTweetUserLocation[-empty.id,]

#which(grepl(x = catTweetUserLocation[,1], pattern = "%"))

s = strsplit(catTweetUserLocation, ",")
# returns the 2nd element of a vector
get2nd <-function(x) {
  x[2]
}

# extract the states
states = sapply(s, get2nd)

# remove any blank spaces
states = gsub(" ", "", states)
states = gsub("Alabama", "AL", states)
states = gsub("Alaska", "AK", states)
states = gsub("Arizona", "AZ", states)
states = gsub("Arkansas", "AR", states)
states = gsub("California", "CA", states)
states = gsub("Colorado", "CO", states)
states = gsub("Connecticut", "CT", states)
states = gsub("Delaware", "DE", states)
states = gsub("Florida", "FL", states)
states = gsub("Georgia", "GA", states)
states = gsub("Hawaii", "HI", states)
states = gsub("Idaho", "ID", states)
states = gsub("Illinois", "IL", states)
states = gsub("Indiana", "IN", states)
states = gsub("Iowa", "IA", states)
states = gsub("Kansas", "KS", states)
states = gsub("Kentucky", "KY", states)
states = gsub("Louisana", "LA", states)
states = gsub("Maine", "ME", states)
states = gsub("Maryland", "MD", states)
states = gsub("Massachussets", "MA", states)
states = gsub("Michigan", "MI", states)
states = gsub("Minnesota", "MN", states)
states = gsub("Mississippi", "MS", states)
states = gsub("Missouri", "MO", states)
states = gsub("Montana", "MT", states)
states = gsub("Nebraska", "NE", states)
states = gsub("Nevada", "NV", states)
states = gsub("NewHampshire", "NH", states)
states = gsub("NewJersey", "NJ", states)
states = gsub("NewMexico", "NM", states)
states = gsub("NewYork", "NY", states)
states = gsub("NorthCarolina", "NC", states)
states = gsub("NorthDakota", "ND", states)
states = gsub("Ohio", "OH", states)
states = gsub("Oklahoma", "OK", states)
states = gsub("Oregon", "OR", states)
states = gsub("Pennsylvania", "PA", states)
states = gsub("RhodeIsland", "RI", states)
states = gsub("SouthCarolina", "SC", states)
states = gsub("SouthDakota", "SD", states)
states = gsub("Tennessee", "TN", states)
states = gsub("Texas", "TX", states)
states = gsub("Utah", "UT", states)
states = gsub("Vermont", "VT", states)
states = gsub("Virigina", "VA", states)
states = gsub("Washington", "WA", states)
states = gsub("WestVirginia", "WV", states)
states = gsub("Wisconsin", "WI", states)
states = gsub("Wyoming", "WY", states)
na.omit(states)
states <- na.omit(states)
statesDF= rbindlist(lapply(states,as.data.frame))

#https://www.dol.gov/whd/programs/dbra/regions.htm#regioninfo

#WEST = 153
#Arizona, California, Nevada, Hawaii, Oregon, Idaho, Washington
AZ <- grepl("AZ", states) 
CA <- grepl("CA", states) 
ID <- grepl("ID", states) 
NV <- grepl("NV", states) 
WA <- grepl("WA", states) 
OR <- grepl("OR", states) 
sum(AZ,CA,ID,NV,WA,OR)

#MIDWEST = 145
#Illinois, Indiana, Iowa, Kansas, Michigan, Minnesota, Missouri, Nebraska, Ohio and Wisconsin.
IL <- grepl("IL", states) 
IN <- grepl("IN", states) 
IA <- grepl("IA", states) 
KS <- grepl("KS", states) 
MI <- grepl("MI", states) 
MN <- grepl("MN", states) 
MO <- grepl("MO", states) 
NE <- grepl("NE", states) 
OH <- grepl("OH", states) 
WI <- grepl("WI", states) 
sum(IL,IN,IA,KS,MI,MN,MO,NE,OH,WI)

#SOUTHWEST = 98
#Arkansas, Colorado, Louisiana, Montana, New Mexico, North Dakota, 
#Oklahoma, South Dakota, Texas, Utah and Wyoming
AR <- grepl("AR", states) 
CO <- grepl("CO", states) 
LA <- grepl("LA", states) 
MT <- grepl("MT", states) 
NM <- grepl("NM", states) 
ND <- grepl("ND", states) 
OK <- grepl("OK", states) 
SD <- grepl("SD", states) 
TX <- grepl("TX", states) 
UT <- grepl("UT", states) 
WY <- grepl("WY", states) 
sum(AR,CO,LA,MT,NM,ND,OK,SD,TX,UT,WY)

#SOUTHEAST = 124
#Alabama, Florida, Georgia, Kentucky, Mississippi, North Carolina, South Carolina and Tennessee.
AL <- grepl("AL", states) 
FL <- grepl("FL", states) 
GA <- grepl("GA", states) 
KY <- grepl("KY", states) 
MS <- grepl("MS", states) 
NC <- grepl("NC", states) 
SC <- grepl("SC", states) 
TN <- grepl("TN", states) 


#NORTHEAST = 136
#Connecticut, Delaware, Maine, Maryland, Massachusetts, New Hampshire, New Jersey, New York, 
#Pennsylvania,Rhode Island, Vermont, Virginia and West Virginia.
CT <- grepl("CT", states) 
DE <- grepl("DE", states) 
ME <- grepl("ME", states) 
MD <- grepl("MD", states) 
MA <- grepl("MA", states) 
NH <- grepl("NH", states) 
NJ <- grepl("NJ", states) 
NY <- grepl("NY", states) 
PA <- grepl("PA", states) 
RI <- grepl("RI", states) 
VT <- grepl("vT", states) 
VA <- grepl("VA", states) 
WV <- grepl("WV", states) 
sum(CT,DE,ME,MD,MA,NH,NJ,NY,PA,RI,VT,VA,WV)
#total = 656

########## SECOND DATAFRAME ############# 
#########################################

s2 = strsplit(catTweetUsers2Location, ",")
# returns the 2nd element of a vector
get2nd <-function(x) {
  x[2]
}

# extract the states
states2 = sapply(s2, get2nd)

#Remove blank spaces from states2
# remove any blank spaces
states2 = gsub(" ", "", states2)
states2 = gsub("Alabama", "AL", states2)
states2 = gsub("Alaska", "AK", states2)
states2 = gsub("Arizona", "AZ", states2)
states2 = gsub("Arkansas", "AR", states2)
states2 = gsub("California", "CA", states2)
states2 = gsub("Colorado", "CO", states2)
states2 = gsub("Connecticut", "CT", states2)
states2 = gsub("Delaware", "DE", states2)
states2 = gsub("Florida", "FL", states2)
states2 = gsub("Georgia", "GA", states2)
states2 = gsub("Hawaii", "HI", states2)
states2 = gsub("Idaho", "ID", states2)
states2 = gsub("Illinois", "IL", states2)
states2 = gsub("Indiana", "IN", states2)
states2 = gsub("Iowa", "IA", states2)
states2 = gsub("Kansas", "KS", states2)
states2 = gsub("Kentucky", "KY", states2)
states2 = gsub("Louisana", "LA", states2)
states2 = gsub("Maine", "ME", states2)
states2 = gsub("Maryland", "MD", states2)
states2 = gsub("Massachussets", "MA", states2)
states2 = gsub("Michigan", "MI", states2)
states2 = gsub("Minnesota", "MN", states2)
states2 = gsub("Mississippi", "MS", states2)
states2 = gsub("Missouri", "MO", states2)
states2 = gsub("Montana", "MT", states2)
states2 = gsub("Nebraska", "NE", states2)
states2 = gsub("Nevada", "NV", states2)
states2 = gsub("NewHampshire", "NH", states2)
states2 = gsub("NewJersey", "NJ", states2)
states2 = gsub("NewMexico", "NM", states2)
states2 = gsub("NewYork", "NY", states2)
states2 = gsub("NorthCarolina", "NC", states2)
states2 = gsub("NorthDakota", "ND", states2)
states2 = gsub("Ohio", "OH", states2)
states2 = gsub("Oklahoma", "OK", states2)
states2 = gsub("Oregon", "OR", states2)
states2 = gsub("Pennsylvania", "PA", states2)
states2 = gsub("RhodeIsland", "RI", states2)
states2 = gsub("SouthCarolina", "SC", states2)
states2 = gsub("SouthDakota", "SD", states2)
states2 = gsub("Tennessee", "TN", states2)
states2 = gsub("Texas", "TX", states2)
states2 = gsub("Utah", "UT", states2)
states2 = gsub("Vermont", "VT", states2)
states2 = gsub("Virigina", "VA", states2)
states2 = gsub("Washington", "WA", states2)
states2 = gsub("WestVirginia", "WV", states2)
states2 = gsub("Wisconsin", "WI", states2)
states2 = gsub("Wyoming", "WY", states2)
na.omit(states2)
states2 <- na.omit(states2)

states2DF= rbindlist(lapply(states2,as.data.frame))

#https://www.dol.gov/whd/programs/dbra/regions.htm#regioninfo

#WEST = 155
#Alaska, Arizona, California, Nevada, Hawaii, Oregon, Idaho, Washington
AZ2 <- grepl("AZ", states2) 
CA2 <- grepl("CA", states2) 
ID2 <- grepl("ID", states2) 
NV2 <- grepl("NV", states2) 
WA2 <- grepl("WA", states2) 
OR2 <- grepl("OR", states2) 
sum(AZ2,CA2,ID2,NV2,WA2,OR2)

#MIDWEST = 99
#Illinois, Indiana, Iowa, Kansas, Michigan, Minnesota, Missouri, Nebraska, Ohio and Wisconsin.
IL2 <- grepl("IL", states2) 
IN2 <- grepl("IN", states2) 
IA2 <- grepl("IA", states2) 
KS2 <- grepl("KS", states2) 
MI2 <- grepl("MI", states2) 
MN2 <- grepl("MN", states2) 
MO2 <- grepl("MO", states2) 
NE2 <- grepl("NE", states2) 
OH2 <- grepl("OH", states2) 
WI2 <- grepl("WI", states2) 
sum(IL2,IN2,IA2,KS2,MI2,MN2,MO2,NE2,OH2,WI2)

#SOUTHWEST = 99
#Arkansas, Colorado, Louisiana, Montana, New Mexico, North Dakota, 
#Oklahoma, South Dakota, Texas, Utah and Wyoming
AR2 <- grepl("AR", states2) 
CO2 <- grepl("CO", states2) 
LA2 <- grepl("LA", states2) 
MT2 <- grepl("MT", states2) 
NM2 <- grepl("NM", states2) 
ND2 <- grepl("ND", states2) 
OK2 <- grepl("OK", states2) 
SD2 <- grepl("SD", states2) 
TX2 <- grepl("TX", states2) 
UT2 <- grepl("UT", states2) 
WY2 <- grepl("WY", states2) 
sum(AR2,CO2,LA2,MT2,NM2,ND2,OK2,SD2,TX2,UT2,WY2)

#SOUTHEAST = 108
#Alabama, Florida, Georgia, Kentucky, Mississippi, North Carolina, South Carolina and Tennessee.
AL2 <- grepl("AL", states2) 
FL2 <- grepl("FL", states2) 
GA2 <- grepl("GA", states2) 
KY2 <- grepl("KY", states2) 
MS2 <- grepl("MS", states2) 
NC2 <- grepl("NC", states2) 
SC2 <- grepl("SC", states2) 
TN2 <- grepl("TN", states2) 
sum(AL2,FL2,GA2,KY2,MS2,NC2,SC2,TN2)

#NORTHEAST = 106
#Connecticut, Delaware, Maine, Maryland, Massachusetts, New Hampshire, New Jersey, New York, 
#Pennsylvania,Rhode Island, Vermont, Virginia and West Virginia.
CT2 <- grepl("CT", states2) 
DE2 <- grepl("DE", states2) 
ME2 <- grepl("ME", states2) 
MD2 <- grepl("MD", states2) 
MA2 <- grepl("MA", states2) 
NH2 <- grepl("NH", states2) 
NJ2 <- grepl("NJ", states2) 
NY2 <- grepl("NY", states2) 
PA2 <- grepl("PA", states2) 
RI2 <- grepl("RI", states2) 
VT2 <- grepl("vT", states2) 
VA2 <- grepl("VA", states2) 
WV2 <- grepl("WV", states2) 
sum(CT2,DE2,ME2,MD2,MA2,NH2,NJ2,NY2,PA2,RI2,VT2,VA2,WV2)
#total 567

########## TABLES AND BARPLOTS ########## 
#########################################

# Declaration of Region Colors: West, Midwest, Southwest, Southeast, Northeast
regionColors <- c("#f76f6a","#f59846", "#C7E363", "#f9dc6d", "#58b2c3")

# Table 1 Set up
stateTable <- matrix(c(153, 145, 98, 124, 136), ncol=5, byrow=TRUE)
colnames(stateTable) <- c("West", "Midwest", "Southwest", "Southeast", "Northeast")
rownames(stateTable) <- c("Proportion")
stateTable <- as.table(stateTable)
stateTableProp <- prop.table(stateTable)*100
t.mat <- t(stateTableProp)

# Prototype Barplot
barplot(height = t.mat, beside = T, main="Proportion of Cat-related Tweets", xlab="Regions", ylab = "Percentages", col=regionColors,
        names.arg=c("West","Midwest", "Southwest", "Southeast", "Northeast"), ylim = c(0,30), space = c(0.5,0.5,0.5,0.5,0.5))

tableProps <- read.csv("tableProps.csv", header=TRUE)
tableProps$Region <- factor(tableProps$Region, as.character(tableProps$Region))

# Barplot 1
ggplot(data=tableProps, aes(x=Region, y=Proportion, fill=Region)) + 
  labs(title="Proportion of Cat-Related Tweets by Region" ,subtitle = ("February 21, 2017")) +
  geom_bar(stat="identity", position="dodge", color="#999999") + scale_fill_manual(values=regionColors) +
  scale_y_continuous(limits=c(0,30), breaks=seq(0,30,5.0)) + theme_minimal() + theme(text = element_text(size=16)) +
  geom_text(aes(label=Proportion), vjust=1.5, size = 5) + theme(plot.title = element_text(size=24, face="bold", hjust=0.5),
                                                                plot.subtitle=element_text(hjust=0.5, face="italic"),
                                                                axis.title.x = element_text(size=14),
                                                                axis.title.y = element_text(size=14))

# Table 2 Set up
state2Table <- matrix(c(155, 99, 107, 99, 106), ncol=5, byrow=TRUE)
colnames(state2Table) <- c("West", "Midwest", "Southwest", "Southeast", "Northeast")
rownames(state2Table) <- c("Proportion")
state2Table <- as.table(state2Table)
stateTable2Prop <- prop.table(state2Table)*100
t.mat2 <- t(stateTable2Prop)

# Prototype Barplot 2
barplot(height = t.mat2, beside = T, main="Proportion of Cat-related Tweets", xlab="Regions", ylab="Percentages", col=regionColors,
        names.arg=c("West","Midwest", "Southwest", "Southeast", "Northeast"), ylim = c(0,30), space = c(0.5,0.5,0.5,0.5,0.5))

# Barplot 2
ggplot(data=tableProps, aes(x=Region, y=Proportion2, fill=Region)) + 
  labs(title="Proportion of Cat-Related Tweets by Region" ,subtitle = ("February 23, 2017")) +
  geom_bar(stat="identity", position="dodge", color="#999999") + scale_fill_manual(values=regionColors) +
  scale_y_continuous(limits=c(0,30), breaks=seq(0,30,5.0)) + theme_minimal() + theme(text = element_text(size=16)) +
  geom_text(aes(label=Proportion2), vjust=1.5, size = 5) + theme(plot.title = element_text(size=16, face="bold", hjust=0.5 ),
                                                                 plot.subtitle=element_text(hjust=0.5, face="italic"),
                                                                 axis.title.x = element_text(size=14),
                                                                 axis.title.y = element_text(size=14))

# Comparison of Proportions
# Import CSV file with proportions
pTableCSV <- read.csv("proportionTable.csv", header=TRUE)

# Keep the levels in order of appearance in the data frame.
pTableCSV$Region <- factor(pTableCSV$Region, as.character(pTableCSV$Region))

ggplot(pTableCSV, aes(Region, Proportion, fill = Date)) + ggtitle("Proportion of Cat-Related Tweets by Day") + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(limits=c(0,30), breaks=seq(0,30,5.0)) +
  scale_fill_brewer(palette = "Set2")

########## MAPPING TWEETS TO ############
########## A REGION/STATE    ############
#########################################

# Import state coords
stateCoords <- read.csv("stateCoords.csv", header=TRUE)

# Generate US Map
# Retrieve map of the U.S. (Alaska and Hawaii excluded)
map.data <- map_data("state")
# Create the map
m =  ggplot(map.data) + geom_map(aes(map_id = region), map = map.data, fill = "white", color = "grey20", size = 0.25) +
  expand_limits(x = map.data$long, y = map.data$lat) + 
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(),
        panel.grid.major = element_blank(), plot.background = element_blank(),
        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) 
# plot the map
plot(m)

# generate points based on latitude and longitude
# (points are near IL and CA)
long1 = rnorm(5, mean = -90)
long2 = rnorm(20, mean = -120)
lat = rnorm(40, mean = 40)

points = data.frame(x = c(long1, long2), y = lat)

# color the first 20 points (IL) blue and the 2nd 20 (CA) red
col = c(rep("blue", 20), rep("red", 20))

# add the points to the map
m2 = m + geom_point(data = points,  aes(x = x, y = y), size = 1, alpha = 1, color = col)

# plot the map with the points
plot(m2)

##### REFERENCES#######################
#######################################

# https://kohske.wordpress.com/2010/12/29/faq-how-to-order-the-factor-variables-in-ggplot2/
# GGPLOT, reordering the Regions so they exist in order that they do in the DF.
# http://yatani.jp/teaching/doku.php?id=hcistats:chisquare
# Chi-squared test example.