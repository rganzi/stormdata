## Reproducible Research - Peer Assessment 2: Storm Data

## DATA PROCESSING
library(lubridate)
library(stats)
library(Hmisc)

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
file2url <- "https://raw.githubusercontent.com/rganzi/stormdata/master/evtype_off.csv"
file2name <- "evtype_off.csv"
evtype.download <- function() {
        if (!file.exists(file2name)) {
                download.file(file2url, dest = file2name, method = "curl")
        }
        evtype.off <- read.csv(file2name, header = FALSE, col.names = "evtype", 
                               colClasses = "character")
}
evtype.off <- evtype.download()

# only include official NOAA storm data event types
stormdata <- data[data$evtype %in% toupper(evtype.off$evtype), ]
stormdata$evtype <- as.factor(tolower(stormdata$evtype))
rm(data)

# Date class
stormdata$bgn_date <- as.Date(stormdata$bgn_date, format = "%m/%d/%Y")

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
library(xtable)
library(ggplot2)

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
xtable(inj[1:6, ])

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
        map.data$damage <- as.character(map.data$damage)
        map.data$damage <- as.numeric(map.data$damage)
        
        map.data$cut = cut(map.data$damage, breaks = c(seq(0, max(map.data$damage) * (8/9),
                                                           by = max(map.data$damage) / 8), max(map.data$damage)))
        
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