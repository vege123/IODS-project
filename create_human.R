initsetup <- function(k){
  setwd(k)
  library(Matrix)
  library(ggplot2)
  library(dplyr)
}
initsetup("/Users/veikko/Desktop/datakurssi/IODS-project/data")
getwd()
rm(list=ls())

hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)

gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

glimpse(hd)
dim(hd)
summary(hd)
colnames(hd)

# rename variables
colnames(hd) <- c("hdi_rank", "country", "hdi_index", "leab", "exp_edu", "mean_edu",
                   "gnipc", "gnipc-hdi_rank")
colnames(hd)

str(gii)
dim(gii)
summary(gii)
colnames(gii)

# rename variables
colnames(gii) <- c("gii_rank", "country", "gii_index", "mmr", "abr", "pr",
                   "edu2F", "edu2M", "labF", "labM")
colnames(gii)

# creating two new variables
gii <- gii %>%
  mutate(edu2_sexratio = edu2F/edu2M, lab_sexratio = labF / labM)

# combining the datasets
human <- gii %>%
  inner_join(hd, by="country")

# writing the data frame into a csv file to data folder
write.csv(human, file = "human.csv", row.names = F)

