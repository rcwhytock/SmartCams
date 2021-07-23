#### Note this would be a lot shorter if the dateTime format of the alerts for the image time included the seconds
# Load libs
library(exifr)
library(lubridate)
library(ggplot2)
library(caret)

# Load the meta data
alerts <- read.csv("../metaData/rawAlerts.csv")
head(alerts)

# Load the benchmark data from Netherlands
netherland <- read.csv("../metaData/messages-Nederland.csv")
head(netherland)
unique(netherland$imei)

# Load image names and paths
imageList <- list.files("../SmartCamData", full.names = T, recursive = TRUE)

# Create data frame of image paths, site names and image name
imageListSplit <- strsplit(imageList, split = "/")
imageListSplit <- as.data.frame(do.call("rbind", imageListSplit))

imageListDF <- data.frame(imageName = imageListSplit$V6,
                          site = imageListSplit$V3,
                          path = imageList)

imageTimes <- read_exif(imageListDF$path, tags = c("DateTimeOriginal"))
head(imageTimes)

exifSplit <- strsplit(imageTimes$SourceFile, split = "/")
exifSplit <- as.data.frame(do.call("rbind", exifSplit))

exifSplitDF <- data.frame(imageName = exifSplit$V6,
                          site = exifSplit$V3,
                          path = imageTimes$SourceFile,
                          image_datetime = ymd_hms(imageTimes$DateTimeOriginal, tz = "UTC"))

# Merge images with alerts based on datetime
alerts$image_datetime <- ymd_hms(alerts$image_datetime, tz = "UTC")


alerts[which(! alerts$image_datetime %in% exifSplitDF$image_datetime),] # seems to be some duplicates, remove

alertsNoDup <- alerts[which(!duplicated(alerts$image_datetime)),]
alertsNoDup <- alertsNoDup[- which(!alertsNoDup$image_datetime %in% exifSplitDF$image_datetime),] # images not yet collected or deleted from database (e.g. those with 2019 date)

# 588 messages

# Merge
finalDat <- merge(alertsNoDup, exifSplitDF, by = "image_datetime", all.y = TRUE)

# Now load in the Mbaza classifications
mbaza <- read.csv("../MbazaOutput/classification_result_corrected_20-07-2021.csv")
head(mbaza)

# Need to fix some of the Mbaza paths because I later standardized the database
mbaza$location <- gsub(pattern = "C:\\\\GitHub\\\\SmartCams\\\\", replacement = "", x = mbaza$location)
mbaza$location <- gsub(pattern = "\\\\", replacement = "/", x = mbaza$location)
mbaza$location  <- gsub(pattern = "SmartCamData/SEGC1/",  replacement = "SmartCamData/SEGC1/DCIM/100EK113/", mbaza$location)
mbaza$location  <- gsub(pattern = "SmartCamData/AirportWest1/",  replacement = "SmartCamData/AirportWest1/DCIM/100EK113/", mbaza$location)

# Remove section of string from the finalDat path
finalDat$path <- gsub("\\../", "", finalDat$path)

# Merge the alerts and the Mbaza classifications, keeping on the alerts
names(finalDat)[17] <- "location"
mergeAll <- merge(finalDat, mbaza, by = "location")

# Identify which images weren't received as alerts (mostly Cayet)
mergeAll[which(is.na(mergeAll$inference_class)),c("location", "image_datetime")] 
# no alerts from AirportEast between 18 June 16:58:09 and 30 June 10:35:29, no alerts from Cayet 16 June 12:35:28 and 30 June 09:30:37

# Fix the site names 
unique(mergeAll$site)
mergeAll[which(mergeAll$site %in% c("AirportWest1", "AirportWest2")),"site"] <- "Airport West"
mergeAll[which(mergeAll$site %in% c("SEGC1", "SEGC2")),"site"] <- "SEGC"
mergeAll[which(mergeAll$site %in% c("KazamabikaOriginal")),"site"] <- "Kazamabika"
mergeAll[which(mergeAll$site %in% c("AirportEast")),"site"] <- "Airport East"

# Table of alerts per day for each camera, for supplementary mat.
mergeAllReceived <- mergeAll[which(!is.na(mergeAll$inference_class)),] 

# Remove images with no timestamp
mergeAllReceived <- mergeAllReceived[which(year(mergeAllReceived$image_datetime) > year(dmy("01/01/2019"))),]

# Store alerts by day
alertsByDay <- as.data.frame.matrix((table(mergeAllReceived$site, day(mergeAllReceived$image_datetime))))
write.csv(alertsByDay, "../Results/alertsByDay.csv")

# Store images per day
imagesByDay <- as.data.frame.matrix((table(mergeAll$site, day(mergeAll$image_datetime))))
imagesByDay <- imagesByDay[,-1] # Remove first column which is images with no timestamp during testing at SEGC
write.csv(imagesByDay, "../Results/imagesByDay.csv")

# Plot mean bridge power per day
meanPower <- aggregate(bridge_voltage ~ site + day(mergeAllReceived$image_datetime), data = mergeAllReceived, FUN = min)
names(meanPower)[2] <- "day"

# Remove Cayet as no alerts received
meanPower <- subset(meanPower, site != "Cayet")


meanPower

pdf(file = "../Results/Figures/bridgeVoltage.pdf", width = 4, height = 4)
ggplot(data = meanPower, aes(day, bridge_voltage)) +
  geom_line(color = "steelblue", size = 0.5) +
  geom_point(color="steelblue", size = 1) + 
  labs(title = "",
       subtitle = "",
       y = "Bridge voltage", x = expression(paste("Deployment day (1 =", " 1"^"st", " June 2021)"))) + 
  ylim(c(2000,4000)) +
  facet_wrap(~ site) +
  theme_classic() +
  theme(panel.spacing = unit(1, "lines"))
dev.off()

# Calculate time between image taken and alert sending
head(mergeAllReceived)
mergeAllReceived$transmit_time <- ymd_hms(mergeAllReceived$transmit_time)

# Subtract an hour from the image taken time as Gabon is UTC +1
mergeAllReceived$image_datetime_UTC <- mergeAllReceived$image_datetime - hours(1)

# Only keep airport East, Airport West, SEGC2 and Kazamabika
unique(mergeAllReceived$site)

mergeAllReceivedTimes <- subset(mergeAllReceived, site %in% c("Airport East", "Airport West", "Kazamabika", "SEGC"))


# Time diff
mergeAllReceivedTimes$diffTime <- difftime(mergeAllReceivedTimes$transmit_time, mergeAllReceivedTimes$image_datetime_UTC,  units = "mins")

head(mergeAllReceivedTimes[,c("site", "transmit_time", "image_datetime")])
median(mergeAllReceivedTimes$diffTime) # 7.35 minutes
max(mergeAllReceivedTimes$diffTime) # 9473.9 minute
mergeAllReceived[which(mergeAllReceivedTimes$diffTime > 9473),]

min(mergeAllReceivedTimes$diffTime) # -1.55 minutes is odd, check
mergeAllReceivedTimes[mergeAllReceivedTimes$diffTime < 0,] # these took less than a minute to arrive in SEGC2 because very open sky, change to 0

length(which(mergeAllReceivedTimes$diffTime < 16)) # 296 messages received in less than 5 minutes 
medianTime <- aggregate(diffTime ~ site, data = mergeAllReceivedTimes, FUN = median)
minTime <- aggregate(diffTime ~ site, data = mergeAllReceivedTimes, FUN = min)
maxTime <- aggregate(diffTime ~ site, data = mergeAllReceivedTimes, FUN = max)

# I should join these but I'm lazy
write.csv(medianTime, "../Results/medianDiffTime.csv")
write.csv(minTime, "../Results/minDiffTime.csv")
write.csv(maxTime, "../Results/maxDiffTime.csv")

pdf(file = "../Results/Figures/alertTime.pdf", width = 4, height = 4)
ggplot(mergeAllReceivedTimes, aes(x=diffTime)) + 
  geom_histogram() +
  labs(title = "",
       subtitle = "",
       y = "Frequency", x = expression(paste("Time difference (min)"))) +
  theme_classic() +
  geom_vline(xintercept = median(mergeAllReceivedTimes$diffTime), linetype = "dashed", col = "skyblue") +
  theme(plot.margin = margin(10, 25, 10, 10))
dev.off()

pdf(file = "../Results/Figures/alertTimeSite.pdf", width = 4, height = 4)
ggplot(mergeAllReceivedTimes, aes(x=diffTime)) + 
  geom_histogram() +
  labs(title = "",
       subtitle = "",
       y = "Frequency", x = expression(paste("Time difference (min)"))) +
  theme_classic() +
  facet_wrap(~ site) +
  theme(plot.margin = margin(1, 15, 1, 1), panel.spacing = unit(1, "lines"))
dev.off()

# AI Model evaluation
mergeAllReceived[!mergeAllReceived$common_name %in% c("Loxodonta Species", "Human"),"common_name"] <- "Other"
mergeAllReceived[mergeAllReceived$common_name %in% c("Loxodonta Species"),"common_name"] <- "Elephant_African"
summary(factor(mergeAllReceived$common_name))
summary(factor(mergeAllReceived$inference_class))

mergeAllReceived$inference_class <- factor(mergeAllReceived$inference_class)
mergeAllReceived$common_name <- factor(mergeAllReceived$common_name)

confmat <- confusionMatrix(reference = mergeAllReceived$common_name, data = mergeAllReceived$inference_class)

str(confmat)
write.csv(confmat$byClass, "../results/ClassModelStats.csv")
write.csv(confmat$overall, "../results/ClassModelStats.csv")
write.csv(confmat$table, "../results/confusionMatrix.csv")

# Compare autoML with Mbazza
mergeAllReceived[!mergeAllReceived$label %in% c("Elephant_African", "Human"),"label"] <- "Other"
mergeAllReceived$label <- factor(mergeAllReceived$label)
mergeAllReceived$label

confmatMbazza <- confusionMatrix(reference = mergeAllReceived$common_name, data = mergeAllReceived$label)
confmatMbazza

write.csv(confmatMbazza$byClass, "../results/ClassModelStatsMbazza.csv")
write.csv(confmatMbazza$overall, "../results/ClassModelStatsMbazza.csv")
write.csv(confmatMbazza$table, "../results/confusionMatrixMbazza.csv")

# Offset time column to calculate events
mergeAllReceived <- mergeAllReceived[with(mergeAllReceived, order(site, image_datetime_UTC)),]
head(mergeAllReceived)

eventList <- vector("list", length = length(unique(mergeAllReceived$site)))
twindow <- 30*60 # (30 min window in s)

for(i in 1:length(unique(mergeAllReceived$site))){
  
  newDat <- subset(mergeAllReceived, site == unique(mergeAllReceived$site)[i])
  newDat$image_datetime_offset <- c(newDat$image_datetime_UTC[1], newDat$image_datetime_UTC[-nrow(newDat)])
  newDat$event_timeDiff <-  newDat$image_datetime_UTC - newDat$image_datetime_offset
  newDat$seq <- ifelse(newDat$event_timeDiff < twindow, 0, 1)
  seqPoints <- which(newDat$seq == 1)
  seqPoints <- c(1,seqPoints,length(newDat$seq))
  
  newDat$eventNumber <- NA
  
  for(j in 1:(length(seqPoints)-1)){
    
    newDat$eventNumber[c(seqPoints[j]:seqPoints[j+1])] <- rep(j, length = length(seqPoints[j]:seqPoints[j+1]))
    
  }
  
  eventList[[i]] <- newDat 
  
}

eventDF <- do.call("rbind", eventList)
head(eventDF)

eventDF[,c("site", "image_datetime", "eventNumber")]

# Create unique events
eventDF$uniqueEvent <- paste0(eventDF$site, "_", eventDF$eventNumber)

voteCountTruth <- as.data.frame.matrix(table(eventDF$uniqueEvent, eventDF$common_name))
voteCountTruth$topVoteTruth <- max.col(voteCountTruth[,c("Elephant_African", "Human", "Other")],)

voteCount <- as.data.frame.matrix(table(eventDF$uniqueEvent, eventDF$inference_class))
voteCount$topVote <- max.col(voteCount[,c("Elephant_African", "Human", "Other")],)

confMatEvent <- confusionMatrix(data = factor(voteCount$topVote), reference = factor(voteCountTruth$topVoteTruth))
# Ele = 1, Human = 2, Other = 3

write.csv(confMatEvent$byClass, "../results/ClassModelStatsEvent.csv")
write.csv(confMatEvent$overall, "../results/ClassModelStatsEvent.csv")
write.csv(confMatEvent$table, "../results/confusionMatrixEvent.csv")

# For each event, take the prediction with the maximum softmax
