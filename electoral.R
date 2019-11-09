rm(list = ls()) 
# clears the global environment
setwd("/Users/andrew/Library/Mobile Documents/com~apple~CloudDocs/PoliSci 150A")
# sets the working directory
d = read.csv("campaign_finance.csv")
# loads the data set in and stores it in the variable d
stats = function(var){
print(c("Mean: ", mean(var)))
print(c("Median: ", median(var)))
print(c("Standard deviation: ", sd(var)))
}
# creates a function call stats that prints a brief summary of the indicated variable 
stats(d$dem_group_pct)
stats(d$dem_indiv_pct)
stats(d$dem_vote_pct)
# uses the stats function to print a brief summary of the democratic percentage of 
# group contributions, individual contributions, and votes
plot(d$dem_money_pct, d$dem_vote_pct)
abline(lm(d$dem_money_pct~d$dem_vote_pct), col = "red")
# plots the relationship between democratic percentage of total contributions and 
# democratic vote share, which shows a positive, linear correlation between the two
# variables
states = unique(d$state)
states.vote = rep(NA, length(states))
states.money = rep(NA, length(states))
for(i in 1:length(states)){
  states.vote[i] = mean(d$dem_vote_pct[which(d$state == states[i])])
  states.money[i] = mean(d$dem_money_pct[which(d$state == states[i])])
}
states.data = data.frame(states, states.money, states.vote)
plot(states.data$states.money, states.data$states.vote, col = "white")
text(states.data$states.money, states.data$states.vote, labels = states.data$states, 
     cex = 0.5)
abline(lm(d$dem_money_pct~d$dem_vote_pct), col = "red")
# plots the state-level average of democratic percentage of contributions and democratic 
# voteshare, labeled with the state name, to create a cleaner view of the linear relati-
# onship
d$grp.diff = d$dem_vote_pct - d$dem_group_pct
d$ind.diff = d$dem_vote_pct - d$dem_indiv_pct
hist(d$grp.diff)
abline(v = mean(d$grp.diff), col = "red")
hist(d$ind.diff)
abline(v = mean(d$ind.diff), col = "red")
# compares the use of share of group contributions and share of individual contributions
# as an unbiased estimator of democratic vote share.  The histogramos show that both types
# of contributions are fairly unbiased estimators with meanns close to zero, although share 
# of group contributions may be slightly better, as it has the lower variance
error = function(estimator){
  count = 0
  for(i in 1:length(d$dem_group_pct)){
    if((estimator[i] > 50 & d$dem_vote_pct[i] < 50) | (estimator[i] < 50 & 
                                                       d$dem_vote_pct[i] > 50)){
      count = count + 1
    }
  }
  count
}
error(d$dem_group_pct) / length(d$state)
error(d$dem_indiv_pct) / length(d$state)
# creates a function titled error that is used to calculate the percentage of times that share 
# of individual and group contributions was wrong in predicting the outcome of the election, 
# showing that share of group contributions is a more accurate predictor of election results
# par(mfrow=c(9,5), mar=c(1,1,1,1), oma=c(3,3,3,3))
for(j in 1:length(states)){
  d1 = d[which(d$state == states[j]),]
  plot(d1$dem_money_pct, d1$dem_vote_pct, main = states[j], yaxt = "n", xaxt = "n")
  abline(lm(d1$dem_money_pct~d1$dem_vote_pct))
}
# creates graphs plotting share of total contributions and voteshare for each state
sum = 0
for(x in seq(10, 100, by = 10)){
  d.1 = d[which(d$dem_group_pct < x), ]
  sum = sum + cor(d.1$dem_indiv_pct, d.1$dem_vote_pct)
}
sum / 10

sum1 = 0
for(y in seq(10, 100, by = 10)){
  d.1 = d[which(d$dem_indiv_pct < y), ]
  sum1 = sum1 + cor(d.1$dem_group_pct, d.1$dem_vote_pct)
}
sum1 / 10
# attempts to control for one of the variables by grouping observations into intervals of 
# 10 for one variable and calculating the correlation coefficient for the other variable 
# and voteshare and then averages the r values for each group.  From this we can see that
# share of group contributions is more strongly correlated with voteshare than share of 
# individual contributions 
