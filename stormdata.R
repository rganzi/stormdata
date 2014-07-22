## Reproducible Research - Peer Assessment 2: Storm Data

## DATA PROCESSING
library(lubridate)
library(stats)
library(Hmisc)
library(ggplot2)

# download storm data
fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
filename <- "repdata-data-StormData.csv.bz2"
downloadData <- function() {
        
        if (!file.exists(filename)) {
                download.file(fileurl, dest=filename, method="curl")
        }
        con <- bzfile(filename, open = "r")
        close(con)
        data <- read.csv(con <- bzfile(filename, open = "r"), header = TRUE, stringsAsFactors = FALSE)
        close(con)
        names(data) <- tolower(names(data))
        data
}
data <- downloadData()
data <- data[, c(2, 7:8, 23:28)]

# download official NOAA storm data event types
file2url <- "https://raw.githubusercontent.com/rganzi/stormdata1/master/evtype_off.csv"
file2name <- "evtype_off.csv"
evtype.download <- function() {
        if (!file.exists(file2name)) {
                download.file(file2url, dest = file2name, method = "curl")
        }
        evtype.off <- read.csv(file2name, header = TRUE, colClasses = "character", na.strings = "")
}
evtype.off <- evtype.download()

sum(grepl("LOW.TIDE", data$evtype, perl=TRUE))
sum(grepl("LOW TIDE", stormdata$evtype, perl=TRUE)) #0

sum(grepl("AVALANCHE", data$evtype, perl=TRUE))
sum(grepl("AVALANCHE", stormdata$evtype, perl=TRUE)) #-1

sum(grepl("BLIZZARD", data$evtype, perl=TRUE))
sum(grepl("BLIZZARD.", data$evtype, perl=TRUE))
sum(grepl("BLIZZARD", stormdata$evtype, perl=TRUE)) #-13

sum(grepl("COASTAL FLOOD", data$evtype, perl=TRUE))
sum(grepl("COASTAL FLOOD.", data$evtype, perl=TRUE))
sum(grepl("COASTAL FLOOD", stormdata$evtype, perl=TRUE)) #-3

grep("DEBRIS", data$evtype, perl=TRUE, value=T) #0 matches
sum(grepl("DEBRIS FLOW.", data$evtype, perl=TRUE))
sum(grepl("DEBRIS FLOW", stormdata$evtype, perl=TRUE))

sum(grepl("DENSE FOG", data$evtype, perl=TRUE))
sum(grepl("DENSE FOG", stormdata$evtype, perl=TRUE)) #-3

sum(grepl("SMOKE", data$evtype, perl=TRUE)) # <- "DENSE SMOKE"
#grep("SMOKE", data$evtype, perl=TRUE, value=T)
sum(grepl("DENSE SMOKE", stormdata$evtype, perl=TRUE))

sum(grepl("DROUGHT", data$evtype, perl=TRUE))
sum(grepl("DROUGHT.", data$evtype, perl=TRUE))
sum(grepl("DROUGHT", stormdata$evtype, perl=TRUE)) #-11

sum(grepl("DUST DEVIL", data$evtype, perl=TRUE))
sum(grepl("DUST DEVIL.", data$evtype, perl=TRUE))
sum(grepl("DUST DEVIL", stormdata$evtype, perl=TRUE)) #0

sum(grepl("DUST STORM", data$evtype, perl=TRUE))
sum(grepl("DUST STORM.", data$evtype, perl=TRUE))
sum(grepl("DUST STORM", stormdata$evtype, perl=TRUE)) #-1

sum(grepl("EXTREME COLD", data$evtype, perl=TRUE)) # <- EXTREME COLD/WIND CHILL +655
sum(grepl("EXTREME COLD/WIND CHILL", stormdata$evtype, perl=TRUE))

sum(grepl("FLASH FLOOD", data$evtype, perl=TRUE))
sum(grepl("FLASH.FLOOD.", data$evtype, perl=TRUE))
sum(grepl("FLASH FLOOD", stormdata$evtype, perl=TRUE)) #-1

# sum(grepl("FLOOD", data$evtype, perl=TRUE))
# sum(grepl("FLOOD", stormdata$evtype, perl=TRUE))

sum(grepl("FROST", data$evtype, perl=TRUE)) #1401
sum(grepl("FREEZE", data$evtype, perl=TRUE)) #1438
sum(grepl("FROST", data$evtype, perl=TRUE) | grepl("FREEZE", data$evtype, perl=TRUE)) #1496
sum(grepl("FROST/FREEZE", stormdata$evtype, perl=TRUE)) #1342

sum(grepl("FUNNEL", data$evtype, perl=TRUE)) #6986
sum(grepl("FUNNEL.CLOUD.", data$evtype, perl=TRUE)) #89
sum(grepl("FUNNEL CLOUD", stormdata$evtype, perl=TRUE)) #6839

# sum(grepl("FREEZING FOG", data$evtype, perl=TRUE)) #45
# sum(grepl("FREEZING FOG", stormdata$evtype, perl=TRUE)) #45

sum(grepl("HEAVY RAIN", data$evtype, perl=TRUE)) #11793
sum(grepl("HEAVY RAIN.", data$evtype, perl=TRUE)) #55
sum(grepl("HEAVY RAIN", stormdata$evtype, perl=TRUE)) #11723

sum(grepl("HEAVY SNOW", data$evtype, perl=TRUE)) #15801
sum(grepl("HEAVY SNOW.", data$evtype, perl=TRUE)) #83
sum(grepl("HEAVY SNOW", stormdata$evtype, perl=TRUE)) #15708

sum(grepl("HIGH SURF", data$evtype, perl=TRUE)) #959
sum(grepl("HIGH SURF", stormdata$evtype, perl=TRUE)) #725

sum(grepl("HIGH WIND", data$evtype, perl=TRUE)) #21951
sum(grepl("HIGH WIND.", data$evtype, perl=TRUE)) #1599
sum(grepl("HIGH WIND", stormdata$evtype, perl=TRUE)) #20347

sum(grepl("ICE STORM", data$evtype, perl=TRUE)) #2032
sum(grepl("ICE STORM.", data$evtype, perl=TRUE)) #2
sum(grepl("ICE STORM", stormdata$evtype, perl=TRUE)) #2006

sum(grepl("LAKE.EFFECT.SNOW", data$evtype, perl=TRUE)) #657
# sum(grepl("LAKE.EFFECT SNOW", toupper(evtype.off$evtype), perl=TRUE))
sum(grepl("LAKE.EFFECT.SNOW", stormdata$evtype, perl=TRUE)) #636

sum(grepl("LAKESHORE.FLOOD", data$evtype, perl=TRUE)) #23
sum(grepl("LAKESHORE FLOOD", stormdata$evtype, perl=TRUE)) #23

sum(grepl("LIGHTNING", data$evtype, perl=TRUE)) #15776
sum(grepl("LIGHTNING", stormdata$evtype, perl=TRUE)) #15754

# sum(grepl("MARINE.HAIL", data$evtype, perl=TRUE)) #442
# sum(grepl("MARINE HAIL", stormdata$evtype, perl=TRUE)) #442

# sum(grepl("MARINE.HIGH.WIND", data$evtype, perl=TRUE)) #135
# sum(grepl("MARINE.HIGH.WIND", stormdata$evtype, perl=TRUE)) #135

# sum(grepl("MARINE STRONG WIND", data$evtype, perl=TRUE)) #48
# sum(grepl("MARINE STRONG WIND", stormdata$evtype, perl=TRUE)) #48

sum(grepl("MARINE.THUNDERSTORM", data$evtype, perl=TRUE)) #5812
sum(grepl("MARINE THUNDERSTORM WIND", stormdata$evtype, perl=TRUE)) #5812

sum(grepl("RIP.CURRENT", data$evtype, perl=TRUE)) #777
sum(grepl("RIP.CURRENT.", data$evtype, perl=TRUE)) #307
sum(grepl("RIP CURRENT", stormdata$evtype, perl=TRUE)) #470

# sum(grepl("SEICHE", data$evtype, perl=TRUE)) #21
# sum(grepl("SEICHE", stormdata$evtype, perl=TRUE)) #21

sum(grepl("SLEET", data$evtype, perl=TRUE)) #121
sum(grepl("SLEET.", data$evtype, perl=TRUE)) #28
sum(grepl("SLEET", stormdata$evtype, perl=TRUE)) #59

sum(grepl("STORM.SURGE", data$evtype, perl=TRUE)) #409
sum(grepl("STORM SURGE/TIDE", stormdata$evtype, perl=TRUE)) #148

sum(grepl("STRONG.WIND", data$evtype, perl=TRUE)) #3814
sum(grepl("STRONG.WIND.", data$evtype, perl=TRUE)) #200
sum(grepl("STRONG WIND", stormdata$evtype, perl=TRUE)) #3614

sum(grepl("THUNDERSTORM.WIND", data$evtype, perl=TRUE)) #109445
sum(grepl("TSTM.WIND", data$evtype, perl=TRUE)) #227228
sum(grepl("THUNDERSTORM WIND", stormdata$evtype, perl=TRUE)) #88375

sum(grepl("HURRICANE", data$evtype, perl=TRUE))
sum(grepl("TYPHOON", data$evtype, perl=TRUE))
hurricane.party <- grepl("HURRICANE", data$evtype, perl=TRUE) | grepl("TYPHOON", data$evtype, perl=TRUE)
sum(hurricane.party)

data$evtype[hurricane.party] <- "HURRICANE"
sum(grepl("HURRICANE", data$evtype, perl=TRUE))

# replace unofficial event types
for (i in 1:length(evtype.off$evtype)) {
        logic.vect <- grepl(evtype.off$match1[i], data$evtype, perl=TRUE) | grepl(evtype.off$match2[i], data$evtype, perl=TRUE)
        data$evtype[logic.vect] <- evtype.off$evtype[i]
}

head(data$evtype)
head(evtype.off$match1)
evtype.off$evtype

# only include official NOAA storm data event types
stormdata <- data[toupper(data$evtype) %in% toupper(evtype.off$evtype), ]
stormdata$evtype <- as.factor(tolower(stormdata$evtype))
# rm(data)

# Date class
stormdata$bgn_date <- as.Date(stormdata$bgn_date, format = "%m/%d/%Y")

# damage columns calculated
# replace abbreviations for *exp columns
# replace.exp <- function(exp = c("propdmgexp", "cropdmgexp")) {
#         abbs <- c("H", "K", "M", "B")
#         digs <- c(2, 3, 6, 9)
#         
#         exp.var <- data.frame()
#         for(i in 1:4) {
#                 exp.var <- gsub(abbs[i], digs[i], stormdata[exp], ignore.case = TRUE)
#         }
#         exp.var <- as.integer(exp.var)
#         exp.var[is.na(exp.var)] <- 0
#         exp.var
# }
# stormdata$propdmgexp <- replace.exp("propdmgexp")
# stormdata$cropdmgexp <- replace.exp("cropdmgexp")
# 
# stormdata$cropdmgexp <- as.integer(stormdata$cropdmgexp)
# summary(data$cropdmgexp)
# data$cropdmgexp <- as.integer(data$cropdmgexp)
# 
# 
# # new variables for calculated damage values, including total damage
# stormdata$propdmg <- stormdata$propdmg * 10^stormdata$propdmgexp
# stormdata$cropdmg <- stormdata$cropdmg * 10^stormdata$cropdmgexp
# stormdata$totaldmg <- stormdata$propdmg + stormdata$cropdmg
# 
# # remove *exp columns
# stormdata <- stormdata[, -c(7,9)]


# damage columns calculated
abbs <- c("H", "K", "M", "B")
digs <- c(2, 3, 6, 9)

# replace abbreviations for *exp columns
for(i in 1:4) {
        stormdata$propdmgexp <- gsub(abbs[i], digs[i], stormdata$propdmgexp, ignore.case = TRUE)
        stormdata$cropdmgexp <- gsub(abbs[i], digs[i], stormdata$cropdmgexp, ignore.case = TRUE)
}

stormdata$propdmgexp <- as.integer(stormdata$propdmgexp)
stormdata$propdmgexp[is.na(stormdata$propdmgexp)] <- 0

stormdata$cropdmgexp <- as.integer(stormdata$cropdmgexp)
stormdata$cropdmgexp[is.na(stormdata$cropdmgexp)] <- 0

# new variables for calculated damage values, including total damage
stormdata$propdmg <- stormdata$propdmg * 10^stormdata$propdmgexp
stormdata$cropdmg <- stormdata$cropdmg * 10^stormdata$cropdmgexp
stormdata$totaldmg <- stormdata$propdmg + stormdata$cropdmg
# remove *exp columns
stormdata <- stormdata[, -c(7,9)]

## RESULTS

# tables of total damage by event type
byevent <- split(stormdata, stormdata$evtype)
cost <- function(cat) {
        df <- data.frame()
        for(i in 1:length(names(byevent))) {
                total <- sum(byevent[[i]][, cat])
                df <- rbind(df, total)
        }    
        names(df) <- c(cat)
        df
}
total.cost <- function() {
        category = c("injuries", "fatalities", "propdmg", "cropdmg", "totaldmg")
        df2 <- data.frame(names(byevent))
        names(df2) <- "evtype"
        for (i in 1:length(category)) {
                ind <- cost(category[i])
                df2 <- cbind(df2, ind)
        }
        df2
}
damage <- total.cost()

inj <- damage[order(damage$injuries, decreasing = TRUE), c(1:2)]
inj[1:6, ]

# maps of total harm or damage by state
map.dmg <- function(dmg = c("injuries", "fatalities", "propdmg", "cropdmg", "totaldmg")) {
        library(maps)
        library(scales)
        library(mapproj)
        
        data(state)
        data.dmg = data.frame()
        for (i in 1:50) {
                state.dmg <- stormdata[stormdata$state == state.abb[i], ]
                sum.dmg <- sum(state.dmg[, dmg])
                data.dmg <- rbind(data.dmg, sum.dmg)
        }
        data.dmg <- cbind(tolower(state.name), data.dmg)
        names(data.dmg) <- c("state", "damage")
        
        #map data
        state_df <- map_data("state")
        map.data <- merge(state_df, data.dmg, by.x = "region", by.y = "state")
        map.data <- map.data[order(map.data$order), ] ##re-order
        
        #cut count var for scale
        map.data$cut = cut(map.data$damage, breaks = c(seq(0, max(map.data$damage),
                                                           by = max(map.data$damage) / 9)))
        
        #plot
        ggplot(map.data, aes(long, lat, group = group)) +
                geom_polygon(data = state_df, colour = "#333333", fill = "#D0D0D0", size = 0.3) +
                geom_polygon(aes(fill = cut)) +
                scale_fill_brewer(palette = "PuRd", guide = guide_legend(title = capitalize(dmg))) +
                geom_path(data = state_df, colour = "#666666", size = 0.1) +
                coord_map("lagrange") +
                labs(title = paste(capitalize(dmg), "by State, 1950-2012", sep = " "), x="", y="") +
                theme_bw()
}
map.dmg("fatalities")

# time-series function for event.type frequency
ts <- function(type = evtype.off$evtype) {
        years <- unique(year(stormdata$bgn_date))
        counts <- data.frame()
        
        for(i in 1:length(years)) {
                count <- nrow(stormdata[year(stormdata$bgn_date) == years[i] & stormdata$evtype == type, ])
                counts <- rbind(counts, c(years[i], count))
        }
        names(counts) <- c("year", "count")
        counts
}
# plot function for ts
plot.ts <- function(evtype = evtype.off$evtype) {
        set <- ts(evtype)
        #tmod <- lm(count ~ year, data = set)
        
        #time-series plot        
        t <- ggplot(data = set, aes(y = count, x = year))
        t + geom_point(colour = "blue") + 
                #geom_line(data = tmod, aes(x = year, y = tmod$fitted)) +
                ggtitle(paste(capitalize(evtype), "Frequency", sep = " ")) +
                xlab("Year") +
                ylab(paste(capitalize(evtype), "Count", sep = " ")) +
                theme_bw()        
}
plot.ts("tornado")

# geographic distribution of event types
map.evtype <- function(evtype = evtype$evtype) {
        
        library(maps)
        library(scales)
        library(mapproj)
        
        data(state)
        # evtype data
        evtype.data <- data.frame()
        for (i in 1:length(state.abb)) {
                subset.evtype <- stormdata[stormdata$evtype == evtype, ]
                sum.states <- sum(subset.evtype$state == state.abb[i])
                evtype.data <- rbind(evtype.data, sum.states)
        }
        evtype.data <- cbind(tolower(state.name), evtype.data)
        names(evtype.data) <- c("state", "count")
        
        # map data
        state_df <- map_data("state")
        map.data <- merge(state_df, evtype.data, by.x = "region", by.y = "state")
        map.data <- map.data[order(map.data$order), ] # re-order
        
        # cut count var for scale
        map.data$cut = cut(map.data$count, breaks = seq(0, max(map.data$count),
                by = max(map.data$count) / 9))
        
        # plot
        ggplot(map.data, aes(long, lat, group = group)) +
                geom_polygon(data = state_df, colour = "#333333", fill = "#D0D0D0", size = 0.3) +
                geom_polygon(aes(fill = cut)) +
                scale_fill_brewer(palette = "PuRd", guide = guide_legend(title = "Count")) +
                geom_path(data = state_df, colour = "#666666", size = 0.1) +
                coord_map("lagrange") +
                labs(title = paste(capitalize(evtype), "Counts by State", sep = " "), x="", y="") +
                theme_bw()
}
map.evtype("tornado")