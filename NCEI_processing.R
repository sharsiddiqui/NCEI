setwd("~/FIRO AR/Risk Communication/Compounding Hazards")
files <- list.files(pattern='NCEI_Details')

file1 <- read.csv(files[1])
file2 <- read.csv(files[2])
file3 <- read.csv(files[3])
file4 <- read.csv(files[4])
file5 <- read.csv(files[5])

data <- rbind(file1,file2,file3,file4,file5)
rm(file1,file2,file3,file4,file5)

## convert damage to numeric

## adjust to 2022 inflation

## calculate sum of losses
