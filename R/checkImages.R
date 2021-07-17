# 17-07-2021 R. Whytock

# Load libs
library(exifr)
library(lubridate)

# Load the meta data
alerts <- read.csv("../metaData/data.csv")
head(alerts)

camNames <- read.csv("../metaData/camMeta.csv")
head(camNames)

# Remove rows with ID 4318 as not a deployment
alerts <- subset(alerts, id != 4318)

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
                          path = imageTimes$SourceFile)

# Merge the imei numbers with the stations names
alertsMerge <- merge(alerts, camNames)
