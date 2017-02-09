#Veikko Isotalo
#4.2.2017
#Data wrangling: Week 3
#data from: https://archive.ics.uci.edu/ml/datasets/STUDENT+ALCOHOL+CONSUMPTION

myfun <- function(projectWD){
  setwd(projectWD)
  library(magrittr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
}
k <- "/Users/veikko/Desktop/datakurssi/IODS-project/data"
myfun(k)
getwd()

math <- read.csv("student-mat.csv", header = T, sep = ";")
por <- read.csv("student-por.csv", header = T, sep = ";")

dim(math)
dim(por)
colnames(math)
head(math)

join_by <- c("school","sex","age","address","famsize","Pstatus","Medu",
             "Fedu","Mjob","Fjob","reason","nursery","internet")

math_por <- math %>%
  left_join(por, by = join_by, suffix = c(".math", ".por")) %>%
  drop_na()

# alternatively: math_por <- inner_join(math, por, by = join_by, suffix = c(".math", ".por"))
glimpse(math_por)

alc <- math_por %>%
  select(one_of(join_by))

# the columns in the datasets which were not used for joining the data
notjoined_columns <- colnames(math)[!colnames(math) %in% join_by]

# for every column name not used for joining...
for(column_name in notjoined_columns) {
  
  two_columns <- select(math_por, starts_with(column_name))
  
  first_column <- select(two_columns, 1)[[1]]
  if(is.numeric(first_column)) {
    alc[column_name] <- round(rowMeans(two_columns))
  } else { 
    alc[column_name] <- first_column
  }
}

# glimpse at the new combined data
glimpse(alc)

alc <- alc %>%
  mutate(alc_use = (Walc + Dalc)/2)

alc <- alc %>%
  mutate(high_use = alc_use > 2)

glimpse(alc)

write.csv(alc, file = "alcohol_data.csv", row.names = F)

#DATA WRANGLING ENDS
#########################################################################