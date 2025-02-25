library(readr)
library(purrr)
library(tidyverse)
library(ggplot2)

reads_color_id <- function(x){
  if (grepl("green", x)){
    read.csv(x,
             skip = 1,
             col.names = c("Well",
                           "Sample",
                           "Channel",
                           "Cycled.volume",
                           "threshold_green",
                           "Partition",
                           "valid_green",
                           "positive_green",
                           "RFU_green"
                            )
             )
  } else if (grepl("yellow", x)){
    read.csv(x,
             skip = 1,
             col.names = c("Well",
                           "Sample",
                           "Channel",
                           "Cycled.volume",
                           "threshold_yellow",
                           "Partition",
                           "valid_yellow",
                           "positive_yellow",
                           "RFU_yellow"
             )
    )
  } else if (grepl("orange", x)){
    read.csv(x,
             skip = 1,
             col.names = c("Well",
                           "Sample",
                           "Channel",
                           "Cycled.volume",
                           "threshold_orange",
                           "Partition",
                           "valid_orange",
                           "positive_orange",
                           "RFU_orange"
             )
    )
  } else if (grepl("red", x)){
    read.csv(x,
             skip = 1,
             col.names = c("Well",
                           "Sample",
                           "Channel",
                           "Cycled.volume",
                           "threshold_red",
                           "Partition",
                           "valid_red",
                           "positive_red",
                           "RFU_red"
             )
    )
  } else{
    stop("no color detected")
  }
}

# get all zip files from raw_data
files <- list.files("raw_data", pattern = ".zip")
# create new directory for unzipped files
unzip_dir <- strtrim(paste("raw_data", files[1], sep = "/"), 50)
dir.create(unzip_dir)
# unzip files in new directory
unzip(paste("raw_data", files[1], sep = "/"), exdir = unzip_dir)
# get list of all .csv files in all subdirectories
zip_files <- list.files(patter = ".csv", recursive = TRUE)
# get list of files in corresponding subdirectory
zip_files <- zip_files %>%
  str_subset(pattern = unzip_dir)
# read all unzipped files and adding the information of the color channel 
df <- do.call(cbind, lapply(zip_files, function(x) reads_color_id(x)))
#drop duplicate columns such as well and sample, cycled.volume, partition
df <- df %>%
  subset(., select = which(!duplicated(names(.)))) %>%
  relocate(Partition, .after = Cycled.volume)

ggplot(df, aes(x = RFU_green, y = RFU_yellow))+
  geom_point()
