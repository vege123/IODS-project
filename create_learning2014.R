myfun <- function(k){
  setwd(k)
  library(Matrix)
  library(ggplot2)
  library(dplyr)
}
myfun("/Users/veikko/Desktop/datakurssi/IODS-project/data")
getwd()

lrn14 <- read.table("JYTOPKYS3-data.txt", header = T, sep = "\t")
str(lrn14)
dim(lrn14)

deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")

# select the columns related to deep learning and create column 'deep' by averaging
deep_columns <- select(lrn14, one_of(deep_questions))
lrn14$deep <- rowMeans(deep_columns)

# select the columns related to surface learning and create column 'surf' by averaging
surface_columns <- select(lrn14, one_of(surface_questions))
lrn14$surf <- rowMeans(surface_columns)

# select the columns related to strategic learning and create column 'stra' by averaging
strategic_columns <- select(lrn14, one_of(strategic_questions))
lrn14$stra <- rowMeans(strategic_columns)

colnames(lrn14)[57] <- "age"
colnames(lrn14)[58] <- "attitude"
colnames(lrn14)[59] <- "points"

varsToGet <- c("gender", "age", "attitude", "deep", "stra", "surf", "points")
new_lrn14 <- select(lrn14, one_of(varsToGet))

new_lrn14 <- filter(new_lrn14, points > 0)

setwd("/Users/veikko/Desktop/datakurssi/IODS-project")
getwd()

write.csv(new_lrn14, file = "learning2014.csv", sep = ",")

learning2014 <- read.csv("learning2014.csv", sep = ",", header = T)
learning2014 <- learning2014[-1]
str(learning2014)
head(learning2014)

learning2014$attitude <- learning2014$attitude / 10
head(learning2014)
str(learning2014)
summary(learning2014)

library(GGally)
p <- ggpairs(learning2014, mapping = aes(col = gender, alpha = 0.3), 
             lower = list(combo = wrap("facethist", bins = 20)))
# draw the plot
p

#trying out to split the data into 3 groups based on "points" variable
learning2014$pgroup <- as.numeric(cut_number(learning2014$points,3))

tbl <- table(learning2014$points) 
barplot(tbl, xlab = "Points", ylab = "Count")
f <-  factor(learning2014$points, levels = 7:33)
barplot(table(f), xlab = "Points", ylab = "Count")

learning2014$pgroup2 <- factor(learning2014$pgroup, labels = c("low", "middle", "high"))
table(learning2014$pgroup2)

test.learn <- subset(learning2014, select = c("age","attitude", "deep", "stra", "surf", "points", "pgroup2"))
ggpairs(test.learn, mapping = aes(col = pgroup2, alpha = 0.3), 
        lower = list(combo = wrap("facethist", bins = 20)))

# pairs(learning2014, col = learning2014$gender)

library(leaps)

#let's remove those two additional variables
drops <- c("pgroup", "pgroup2")
learning2014_org <- learning2014[ , !(names(learning2014) %in% drops)]

#model validation by best subset selection
regfit.full <- regsubsets(points~.,learning2014_org)
reg.summary <- summary(regfit.full)

#some validation metrics
which.max(reg.summary$adjr2)
which.min(reg.summary$cp )
which.min(reg.summary$bic )


par(mfrow = c(1,1))
plot(reg.summary$cp ,xlab="Number of Variables ",ylab="Cp", type="l")
points(3,reg.summary$cp[3],col="red",cex=2,pch=20)

plot(regfit.full,scale="bic")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="r2")
coef(regfit.full ,3)


lm.fit <- lm(points ~ attitude + stra + age, data=learning2014_org)
summary(lm.fit)

#ggplot(learning2014_org, aes(x = attitude, y = points, col = gender)) + geom_smooth(method = "lm") +
#  ggtitle("Student's attitude versus exam points") 

par(mfrow = c(2,2))
plot(lm.fit, which = c(1,2,5))





