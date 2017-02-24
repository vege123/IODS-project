
initsetup <- function(k){
  setwd(k)
  library(Matrix)
  library(ggplot2)
  library(dplyr)
  library(gdata)
}
initsetup("/Users/veikko/Desktop/datakurssi/IODS-project/data")





human <- read.table("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human1.txt", sep = ",", header = T, encoding = "utf-8")

to_keep <- c("Country", "Edu2.FM", "Labo.FM", "Edu.Exp", 
             "Life.Exp", "GNI", "Mat.Mor", "Ado.Birth", "Parli.F")
newdf <- human %>%
  dplyr::select(one_of(to_keep))



# filter out all rows with NA values
human_ <- filter(newdf, complete.cases(newdf))

# remove regions
human_ <- human_[1:155,]
rownames(human_) <- human_$Country

human_ <- human_ %>%
  select(-Country)

human <- human_ 


