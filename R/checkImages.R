# 17-07-2021 R. Whytock

#### Note this would be a lot shorter if the dateTime format of the alerts for the image time included the seconds
# Load libs
library(exifr)
library(lubridate)

# Load the meta data
alerts <- read.csv("../metaData/rawAlerts.csv")
head(alerts)

# Load 'cam' meta with the earliest and latest image datetime in the folders
camNames <- read.csv("../metaData/camMeta.csv")
head(camNames)

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
                          DateTimeOrignal = ymd_hms(imageTimes$DateTimeOriginal))

# Merge the imei numbers with the stations names, first take name, imei and date start and end
camNames$firstImageTime <- dmy_hm(camNames$firstImageTime)
camNames$lastImageTime <- dmy_hm(camNames$lastImageTime)

names(exifSplitDF)[4] <- "image_datetime"

exifSplitDF$imei <- NA

exifSplitDFList <- vector("list", length = nrow(camNames))
for (i in 1:nrow(camNames)) {
  
  newDat <- exifSplitDF[which(exifSplitDF$site == camNames$site[i]),]
  
  newDat <- newDat[which(round(newDat$image_datetime, units = "days") >= round(camNames$firstImageTime[i], units = "days")
                         & round(newDat$image_datetime, units = "days") <= round(camNames$lastImageTime[i], units = "days")),] 
  
  newDat$imei<- camNames$imei[i]
  exifSplitDFList[[i]] <- newDat
  
}

exifSPlitDf <- do.call("rbind", exifSplitDFList)

exifSplitDF$image_id <- paste(exifSplitDF$imei, exifSplitDF$image_datetime)
alerts$image_id <- paste(alerts$imei, alerts$image_datetime)

merge(exifSplitDF, alerts, by = "image_datetime")

alerts$image_datetime <- ymd_hms(alerts$image_datetime)

alerts$site <- NA

# Loop through each imei and start - end time in the alerts to match the sites
alertList <- vector("list", length = nrow(camNames))
for(i in 1:nrow(camNames)){
  
  newDat <- alerts[which(alerts$imei == camNames$imei[i]),]
  
  newDat <- newDat[which(newDat$image_datetime >= camNames$firstImageTime[i]
               & newDat$image_datetime <= camNames$lastImageTime[i]),] 
  
  newDat <- newDat[complete.cases(newDat),]
  newDat$site <- camNames$site[i]
  alertList[[i]] <- newDat

  
}

alertListDF <- do.call("rbind", alertList)
head(alertListDF)

summary(factor(alertListDF$site))


# Now merge model alerts with image filename
exifSplitDF$imageID <- paste(exifSplitDF$site, exifSplitDF$image_datetime)
alerts$imageID <- paste(alerts$site, alerts$image_datetime)

alertsMerge <- merge(alerts, exifSplitDF, by = "image_datetime", all.y = F)

# Remove empty rows
alertsComplete <- alerts[complete.cases(alertsMerge),]

# Plot battery power over time
for(i in 1:nrow(camNames)){
  
  newDat <- subset(alertsComplete, site == camNames$site[i])
  plot(bridge_voltage ~ image_datetime, newDat)
  
}