#this is my project data
#i'd like to cluster the data, but maybe summary stats would
#help to show why

#loading data
goalies <- read.csv("~/cs406/r_stuff/goalies.csv",header=TRUE)

#making sure properly loaded
#might need to preprocess
str(goalies)
goalies$name

#turning Hofers into a scalar
rows <- nrow(goalies)
for (row in c(1:rows))
{
  goalies$Hofers[row] <- sum(goalies[row,19:43])
}
goalies$Hofers

#trying various lm expressions
g_subset <- goalies[,c(3:15, ncol(goalies))]
g_subset #this is the cleanest set yet
cor(g_subset)
g_subset$best_year <- NULL

stillchecking.lm <- lm (sv. ~ total_shots + to*from, data=g_subset)
summary(stillchecking.lm)

gcheck.lm <- lm(sv. ~ to*from + Hofers,data=goalies)
summary(gcheck.lm)

#this gives some weird shit
plot(stillchecking.lm)
plot(sv. ~ to*from + Hofers, data=goalies)


summary (goalies)
plot(goalies$from, goalies$sv.)
abline(lm (goalies$sv. ~ goalies$from), col="red")

plot(goalies$to, goalies$sv., xlab="Final Season", ylab="Career Sv%")
abline(lm (goalies$sv. ~ goalies$to), col="red")
summary (lm (goalies$sv. ~ goalies$to))


goalie.explore <- lm(goalies$sv. ~ goalies$to*goalies$from + goalies$gp*goalies$min +goalies$age_from)
summary(goalie.explore)

goalie.explore <- lm(goalies$sv. ~ goalies$to + goalies$from + goalies$min)
summary(goalie.explore)

goalie.explore <- lm(goalies$sv. ~ goalies$min)
summary(goalie.explore)

plot(goalie.explore)

plot(goalies$min, goalies$sv., xlab="Minutes played", ylab="Career Sv%")
abline(lm (goalies$sv. ~ goalies$min), col="red")
summary (lm (goalies$sv. ~ goalies$min))

plot(goalies$to, goalies$height_cm, xlab="Final Season", ylab="Height in cm")
abline(lm (goalies$height_cm ~ goalies$to), col="red")
summary (lm (goalies$height_cm ~ goalies$to))

numlist <- c(3:12, 14, 15, 19:ncol(goalies))
numlist
stuff <- goalies[,numlist]

goalies[4]
timelump <- goalies$to + goalies$from

summary (lm(goalies$sv. ~ timelump))
summary (lm(goalies$sv. ~ goalies$to + goalies$from))

model <- lm(sv. ~ . - (qs + bs + qual_r + id + name), data=goalies)
summary (model)

model <- lm(sv. ~ ., data=goalies)
summary (model)

unique (unlist (lapply (goalies, function (x) which (is.na (x)))))
for (index in ncol(goalies))
{
  goalies[index]
}
goalies[index]

head(g_subset)

#finally trying some clustering
set.seed(1)
grpGoalies <- kmeans(g_subset[,c("from", "to")], centers = 3, nstart = 10)
grpGoalies
head(grpGoalies)
o <- order(grpGoalies$cluster)
#i have no idea what any of this means
data.frame (goalies$name[o], grpGoalies$cluster[o])

#trying to graph
# plotting cluster assignments on Red and White meat scatter plot
par(mfrow=c(1,1))
plot(g_subset$from, g_subset$to, xlab = "Season started", ylab="Final season")
text(x=g_subset$from, y=g_subset$to, labels=goalies$name, col=grpGoalies$cluster+1)

#giving a goalie its group
g_subset$cluster <- grpGoalies$cluster
g_subset$cluster
goalies$cluster <- g_subset$cluster

#how many in each?
#cluster 1
sum (g_subset$cluster == 1)
#cluster 2
sum (g_subset$cluster == 2)
#cluster 3
sum (g_subset$cluster == 3)

#cluster 1 summary
clust1_subs <- g_subset[which(g_subset$cluster==1),]
summary(clust1_subs)

#cluster 2 summary
clust2_subs <- g_subset[which(g_subset$cluster==2),]
summary(clust2_subs)

#cluster 3 summary
clust3_subs <- g_subset[which(g_subset$cluster==3),]
summary(clust3_subs)

mean(clust1_subs$sv.)
sd(clust1_subs$sv.)
range(clust1_subs$height_cm)
mean(clust1_subs$height_cm)

mean(clust2_subs$sv.)
sd(clust2_subs$sv.)
range(clust2_subs$height_cm)
mean(clust2_subs$height_cm)

mean(clust3_subs$sv.)
sd(clust3_subs$sv.)
range(clust3_subs$height_cm)
mean(clust3_subs$height_cm)

#these are the clustered indices
ind1 <- goalies[which(g_subset$cluster==1),]$id
ind2 <- goalies[which(g_subset$cluster==2),]$id
ind3 <- goalies[which(g_subset$cluster==3),]$id

(mean1 <- mean(goalies[ind1,]$sv.))
(mean2 <- mean(goalies[ind2,]$sv.))
(mean3 <- mean(goalies[ind3,]$sv.))

(sd1 <- sd(goalies[ind1,]$sv.))
(sd2 <- sd(goalies[ind2,]$sv.))
(sd3 <- sd(goalies[ind3,]$sv.))

goalies$zscore <- 0

goalies[ind1,]$zscore <- ((goalies[ind1,]$sv. - mean1) / sd1)
goalies[ind2,]$zscore <- ((goalies[ind2,]$sv. - mean2) / sd2)
goalies[ind3,]$zscore <- ((goalies[ind3,]$sv. - mean3) / sd3)

longcareer <- goalies[which(goalies$gp > 499),]

mean(goalies$gp)

plot(goalies$to, goalies$zscore, xlab = "Season ended", ylab="Z Score")
text(x=goalies$to, y=goalies$zscore, labels=goalies$name, col=grpGoalies$cluster+1)
abline(lm (goalies$zscore ~ goalies$to))

#TWEAK HERE ASDFASDFASDFASDFASDFASDFASDFSDFLJLL
#ASLSFLSDLFLJFLJASDFLJASDFLJ
summary(lm (goalies$zscore ~ goalies$to + goalies$from + goalies$age_from + goalies$weight_kg + goalies$gp + goalies$w + goalies$l + goalies$total_shots + goalies$min))

plot(longcareer$to, longcareer$zscore, xlab = "Season ended", ylab="Z Score")
text(x=longcareer$to, y=longcareer$zscore, labels=longcareer$name, col=grpGoalies$cluster+1)

#i finally have my z scores
plot (zscore ~ total_shots, data=goalies)
abline(lm(zscore ~ total_shots, data=goalies), col="red")

plot (zscore ~ w, data=goalies)
summary(lm(zscore ~ w, data=goalies))

plot (zscore ~ w + total_shots, data=goalies)
summary(lm(zscore ~ w + total_shots, data=goalies))

plot (zscore ~ Hofers, data=goalies)
summary(lm(zscore ~ Hofers, data=goalies))

plot (zscore ~ total_shots, data=goalies)
abline(lm(zscore ~ total_shots, data=goalies), col="red")

range(goalies$weight_kg)
range(goalies$to)
range(goalies$qs)

z.explore <- lm(goalies$zscore ~ goalies$to*goalies$from + goalies$min)
summary(z.explore)

z.explore <- lm(goalies$zscore ~ goalies$total_shots + goalies$min)
summary(z.explore)

ncol(goalies)
goalies$zscore

#partitioning into training and test
#partitioning train and test sets
set.seed(56)
sampleSize <- floor (0.50 * nrow(goalies))
#sampleSize #check value
train <- sample(1:nrow(goalies), sampleSize, replace = FALSE)
train #check value


testing.set <- goalies[-train,]
training.set <- goalies[train,]


training.set$zscore
model <- lm(zscore ~ age_from + age_to + height_cm + weight_kg + gp + w + l + total_shots + min, data=training.set)
summary(model)

model <- lm(zscore ~ age_to + total_shots + min*gp, data=training.set)
summary(model)

predictions <- predict(model, newdata=testing.set)
predictions
goalies[224,]$zscore
-0.5854760573 - 0.2431339
length(predictions)
resid(model, data=test)
resid(model)

goalies[-train,]$zscore - predictions
length(predictions)
nrow(testing.set)
z.resids <- predictions - testing.set$zscore
blah

plot(testing.set$age_to + testing.set$total_shots + testing.set$min * testing.set$gp, z.resids)
plot(testing.set$age_to, z.resids, xlab="Age at Final Season", ylab="Residuals", main="Residual Plot: Age")
abline(0,0)

plot(testing.set$total_shots, z.resids, xlab="Total Shots Faced", ylab="Residuals", main="Residual Plot: Shots")

plot(testing.set$min, z.resids, xlab="Minutes", ylab="Residuals", main="Residual Plot: Minutes")
plot(testing.set$gp, z.resids, xlab="Games Played", ylab="Residuals", main="Residual Plot: GP")

rss <- sum((predictions - testing.set$zscore)^2)
rss

z.resids

testing.set$zscore
testing.set

predictions
goalies[221,]$zscore

#how many values is it predicting?
nrow(testing.set)
length(predictions)

plot(predictions, testing.set$zscore, xlab="Predicted Values", ylab="Actual value", main="Predicted vs. Actual Goalie Z Score")

