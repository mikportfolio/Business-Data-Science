

# Step 1: install packages and import library
###  tidyverse (data cleaning and readr), 
###  dplyr (for piping, summarize), 
###  ggplot2 (for box plots), 
###  lubridate (for formatting dates)


# Step 2: import dataset
wine <- read.csv("C:\\Users\\mudrik\\Downloads\\wine.csv")

# Step 3: CLean data by scaling the variables
xwine <- scale(wine[,1:11])
apply(xwine,2,sd) # sd=1
apply(xwine,2,mean) # mean=0

# Step 4: fit two clusters
two <- kmeans(xwine,2,nstart=10)
two$centers                               # big differences on all accounts
tapply(wine$color,two$cluster,table)      # to inspect color distribution in each cluster
# Note: the two clusters are red v. white!


# Step 5: randomize order in plot, just so its not all white on top of red
# note that my cluster 1 was red; this could be flipped for you.
i <- sample(1:nrow(xwine))  
plot(wine$fixed.acidity[i], wine$volatile.acidity[i],
     pch=21, cex=.75, bty="n",
     xlab="fixed acidity",
     ylab="volatile acidity",
     bg=c("maroon","gold")[two$cluster[i]],
     col=c("maroon","gold")[wine$color[i]])




# Step 6: create a big long vector of clusters
# takes a bit of time 
# you'll also get warnings of non-convergence;
# this isn't too bad; it just means things look 
# longer than the allotted 10 minimization iterations.
kfit <- lapply(1:200, function(k) kmeans(xwine,k))


# Step 7: get AICc and BIC for the output of kmeans
kIC <- function(fit, rule=c("A","B")){
	df <- length(fit$centers) # K*dim
	n <- sum(fit$size)
	D <- fit$tot.withinss
	rule=match.arg(rule)
	if(rule=="A")
		return(D + 2*df*n/max(n-df-1,1))
	else
		return(D + log(n)*df)
}

# Step 8: choose number of clusters?
# source("kIC.R") ## utility script
# you give it kmeans fit, 
# then "A" for AICc (default) or "B" for BIC
kaicc <- sapply(kfit,kIC)
kbic <- sapply(kfit,kIC,"B")

# Step 9: plot kaicc and kbic
plot(kaicc, xlab="K", ylab="IC", 
     ylim=range(c(kaicc,kbic)), # get them on same page
     bty="n", type="l", lwd=2)
abline(v=which.min(kaicc))
lines(kbic, col=4, lwd=2)
abline(v=which.min(kbic),col=4)
# both AICc and BIC choose very complicated models
# you get too big, and you lose interpretive power.
# no clear role of what to do here, it depends what you want.
# we'll just focus on k=30, where the BIC starts to level out

# Step 10: Assign k value
k=30
# IS R^2 of 70% (for explaining deviance of x)
1 - sum(kfit[[k]]$tot.withinss)/kfit[[k]]$totss
# clearly still splitting on color
tapply(wine$color,kfit[[k]]$cluster,table)
## look also at quality average by cluster
tapply(wine$quality,kfit[[k]]$cluster,mean)
# mostly all down around 5-6.5; no clear `star' clusters
# main sources of variation not driven by quality

# Step 11: cluster regression
# to confirm, we can see how well things work in cluster regression
# include color as well
library("gamlr")
xclust <- sparse.model.matrix(~factor(kfit[[k]]$cluster)+wine$color) # cluster membership matrix
wineregclust <- cv.gamlr(xclust,wine$quality,lambda.min.ratio=1e-5) # 
plot(wineregclust)
max(1-wineregclust$cvm/wineregclust$cvm[1]) # OOS R2 around 0.22


## we end up doing a better job just regressing onto raw x;
library(gamlr)  
# create matrix of al three way interactions
x <- model.matrix(quality~.^3, data=wine)[,-1]
winereg <- cv.gamlr(x,wine$quality,lambda.min.ratio=1e-5)
plot(winereg)
max(1-winereg$cvm/winereg$cvm[1]) # max OOS R2 on y of about 1/3
# this is due to two things:
#  * quality is not a dominant source of variation, so quality signal is lost in clustering
#  * the data is not that high dimensional to begin with, so we gain little in DR