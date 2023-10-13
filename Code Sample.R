#format dataset#
####    import dataset    ####
library(readr)
marketing_campaign <- read_delim("marketing_campaign.csv", 
                                 "\t", escape_double = FALSE, trim_ws = TRUE)
rownames <- marketing_campaign$ID
marketing_campaign <- marketing_campaign[,-1]
rownames(marketing_campaign) <- rownames
marketing_campaign <- na.omit(marketing_campaign)

####     added variables    ####
#demographic
marketing_campaign$KidsHousehold <- marketing_campaign$Kidhome + marketing_campaign$Teenhome
marketing_campaign$AgeRegistered <- as.numeric(substring(marketing_campaign$Dt_Customer, 7, 10)) - marketing_campaign$Year_Birth
marketing_campaign$Age <- 2016 - marketing_campaign$Year_Birth
marketing_campaign$CustomerLen <- marketing_campaign$Age - marketing_campaign$AgeRegistered
marketing_campaign$CustomerLen <- as.factor(marketing_campaign$CustomerLen)
marketing_campaign$KidsHousehold <- as.factor(marketing_campaign$KidsHousehold)
#purchase information
marketing_campaign$AvgPurchaseMnt <- (marketing_campaign$MntFishProducts + marketing_campaign$MntFruits + marketing_campaign$MntGoldProds + marketing_campaign$MntMeatProducts + marketing_campaign$MntSweetProducts + marketing_campaign$MntWines)/(marketing_campaign$NumWebPurchases + marketing_campaign$NumCatalogPurchases + marketing_campaign$NumStorePurchases + marketing_campaign$NumDealsPurchases)
marketing_campaign$TotalNumPurchases <- marketing_campaign$NumWebPurchases + marketing_campaign$NumCatalogPurchases + marketing_campaign$NumStorePurchases + marketing_campaign$NumDealsPurchases
marketing_campaign$PctDealPurchases <- marketing_campaign$NumDealsPurchases / (marketing_campaign$NumWebPurchases + marketing_campaign$NumCatalogPurchases + marketing_campaign$NumStorePurchases + marketing_campaign$NumDealsPurchases)

####    restructure categorical variables    ####
marketing_campaign$Education <- as.factor(marketing_campaign$Education)
plot(marketing_campaign$Education, marketing_campaign$Income) 
library(dplyr)
marketing_campaign = marketing_campaign %>% 
  mutate(Education = case_when(
    Education == "2n Cycle" ~ "Master",
    Education == "Basic" ~ "Basic",
    Education == "Graduation" ~ "Bachelor",
    Education == "Master" ~ "Master",
    Education == "PhD" ~ "PhD"
  ))
marketing_campaign$Education = as.factor(marketing_campaign$Education)
marketing_campaign$Education <- ordered(marketing_campaign$Education, levels = c("Basic", "Bachelor", "Master", "PhD"))

levels(as.factor(marketing_campaign$Marital_Status))
marketing_campaign = marketing_campaign %>% 
  mutate(Marital_Status = case_when(
    Marital_Status == "Absurd" ~ "Single",
    Marital_Status == "Alone" ~ "Single",
    Marital_Status == "Divorced" ~ "Single",
    Marital_Status == "Married" ~ "Partner",
    Marital_Status == "Single" ~ "Single",
    Marital_Status == "Together" ~ "Partner",
    Marital_Status == "Widow" ~ "Single",
    Marital_Status == "YOLO" ~ "Single"
  ))
marketing_campaign$Marital_Status = as.factor(marketing_campaign$Marital_Status)

####    remove outliers    ####
#income
outliers <- boxplot(marketing_campaign$Income, plot = FALSE)$out
marketing_campaign <- marketing_campaign[-which(marketing_campaign$Income %in% outliers),]

#age
outliers <- boxplot(marketing_campaign$Age, plot = FALSE)$out
marketing_campaign <- marketing_campaign[-which(marketing_campaign$Age %in% outliers),]

####    subsets    ####
ds <- subset(marketing_campaign, select = c(Age,
                                            AgeRegistered,
                                            CustomerLen,
                                            Education,
                                            Income,
                                            Marital_Status,
                                            KidsHousehold,
                                            MntWines,
                                            MntFruits,
                                            MntMeatProducts,
                                            MntFishProducts,
                                            MntSweetProducts,
                                            MntGoldProds,
                                            AvgPurchaseMnt,
                                            NumDealsPurchases,
                                            PctDealPurchases,
                                            NumWebPurchases,
                                            NumCatalogPurchases,
                                            NumStorePurchases,
                                            NumWebVisitsMonth))

product <- subset(marketing_campaign, select = c(Age,
                                                 AgeRegistered,
                                                 CustomerLen,
                                                 Education,
                                                 Income,
                                                 Marital_Status,
                                                 KidsHousehold,
                                                 MntWines,
                                                 MntFruits,
                                                 MntMeatProducts,
                                                 MntFishProducts,
                                                 MntSweetProducts,
                                                 MntGoldProds))

purchase <- subset(marketing_campaign, select = c(Age,
                                                  AgeRegistered,
                                                  CustomerLen,
                                                  Education,
                                                  Income,
                                                  Marital_Status,
                                                  KidsHousehold,
                                                  AvgPurchaseMnt,
                                                  NumDealsPurchases,
                                                  PctDealPurchases,
                                                  TotalNumPurchases,
                                                  NumWebPurchases,
                                                  NumCatalogPurchases,
                                                  NumStorePurchases,
                                                  NumWebVisitsMonth))

####    initial analysis    ####
library(psych)
purchase <- na.omit(purchase)
pairs.panels(purchase[,c(1,5,8:14)])

length(boxplot.stats(purchase$Age)$out) # 3 outliers
length(boxplot.stats(purchase$Income)$out) # 6 outliers
length(boxplot.stats(purchase$AvgPurchaseMnt)$out) # 98 outliers
length(boxplot.stats(purchase$NumDealsPurchases)$out) # 84 outliers
length(boxplot.stats(purchase$PctDealPurchases)$out) # 3 outliers
length(boxplot.stats(purchase$TotalNumPurchases)$out) # 2 outliers
length(boxplot.stats(purchase$NumWebPurchases)$out) # 3 outliers
length(boxplot.stats(purchase$NumCatalogPurchases)$out) # 23 outliers
length(boxplot.stats(purchase$NumStorePurchases)$out) # none
length(boxplot.stats(purchase$NumWebVisitsMonth)$out) # 6 outliers

hist(log(purchase$Income))
length(boxplot.stats(log(purchase$Income))$out) # 37 outliers even with log transformed data

hist(log(purchase$AvgPurchaseMnt))
length(boxplot.stats(log(purchase$AvgPurchaseMnt))$out) # 2 outliers with log tranformation

hist(scale(purchase$NumDealsPurchases))

hist(scale(purchase$NumDealsPurchases))

####    initial analysis plots    ####
#demographic exploratory variables: Age, AgeRegistered, Income
boxplot(Income ~ CustomerLen * KidsHousehold, data = purchase, main = "Customer Income", xlab = "Customer Registration Length and Dependents", ylab = "Customer Age")

#summary exploratory variables: AvgPurchaseMnt, PctDealPurchases, NumWebVisitsMonth
#     average bill insights
boxplot(AvgPurchaseMnt ~  CustomerLen, data = purchase, main = "Customer Length and Purchase Information", xlab = "Customer Registation Length", ylab = "Average Purchase Amount") #customers that have been registered longer seem to have larger checks on average
boxplot(AvgPurchaseMnt ~  Education * CustomerLen, data = purchase, main = "Customer Characteristics and Purchase Information", xlab = "Education and Customer Registation Length", ylab = "Average Purchase Amount") #clear divide between education despite how long the customer has been registered with the store
boxplot(AvgPurchaseMnt ~  KidsHousehold, data = purchase, main = "Household Spend", xlab = "Kids in Home", ylab = "Average Purchase Amount") #good plot, can see a clear pattern of how number of kid impacts the average check - people with less children have a larger bill at checkout on average despite how long they have been registered with the store

#     customer taking advantages of deals offerred
boxplot(PctDealPurchases ~  KidsHousehold, data = purchase, main = "Customer Spend", xlab = "Kids in Home", ylab = "% of Purchases Made Using Deal") #people with children tend to use or advantage of deals offerred when making purchases

#     customer engagement
boxplot(NumWebVisitsMonth ~  CustomerLen, data = purchase, main = "Customer Engagement", xlab = "Customer Registration Length", ylab = "Number of Website Visits in Last Month") #customers who have been registered longer with the store engage more with the store's website
boxplot(NumWebVisitsMonth ~  CustomerLen * KidsHousehold, data = purchase, main = "Customer Engagement", xlab = "Customer Registration Length and Kids in Home", ylab = "Number of Website Visits in Last Month") #can see how much narrower the boxplots get when length of registration and number of children increases

####   reset categorical variables    ####
purchase$KidsHousehold <- as.numeric(purchase$KidsHousehold) - 1
purchase$CustomerLen <- as.numeric(purchase$CustomerLen) + 1
purchase$Education <- as.numeric(purchase$Education)
purchase$Marital_Status <- as.numeric(purchase$Marital_Status)

####    pie chart with percentages    ####
# education
s1 = 0
s2 = 0
s3 = 0
s4 = 0
for (i in 1:length(purchase$Education)) {
  if(purchase$Education[i] == "Basic"){
    s1 = s1 + 1
  } 
  else if(purchase$Education[i] == "Bachelor"){
    s2 = s2 + 1
  }
  else if(purchase$Education[i] == "Master"){
    s3 = s3 + 1
  }
  else {s4 = s4+ 1}
}
slices <- c(s1, s2, s3, s4)
lbls <- c("Basic", "Bachelor", "Master", "PhD")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices, labels = lbls, radius = 2.5, main="Education") #bachelors largest at 50%; masters and PhD roughly the same at 25% and 22% respectively

# customer length
s1 = 0
s2 = 0
s3 = 0
for (i in 1:length(purchase$CustomerLen)) {
  if(purchase$CustomerLen[i] == 2){
    s1 = s1 + 1
  } 
  else if(purchase$CustomerLen[i] == 3){
    s2 = s2 + 1
  }
  else{s3 = s3 + 1}
}
slices <- c(s1, s2, s3)
lbls <- c("2 year", "3 years", "4 years")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices, labels = lbls, radius = 2.5, main="Customer Registration Length") #3 year longest duration - 53%

# dependents
s1 = 0
s2 = 0
s3 = 0
s4 = 0
for (i in 1:length(purchase$KidsHousehold)) {
  if(purchase$KidsHousehold[i] == 0){
    s1 = s1 + 1
  } 
  else if(purchase$KidsHousehold[i] == 1){
    s2 = s2 + 1
  }
  else if(purchase$KidsHousehold[i] == 2){
    s3 = s3 + 1
  }
  else{s4 = s4 + 1}
}
slices <- c(s1, s2, s3, s4)
lbls <- c("None","1 Dependent", "2 Dependents", "3 Dependents")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices, labels = lbls, radius = 2.5, main="Dependents in Household") #50%  of customers have 1 dependent followed by people with no dependents at 28%

####    mds    ####
library(StatMatch)
df <- as.matrix(scale(purchase))
d = gower.dist(df, var.weights = NULL)
fit = cmdscale(d, eig = TRUE, k = 2)
fit$GOF # [1] 0.2725368 0.3825113; these are small values which is exactly what we want

x = fit$points[,1]
y = fit$points[,2]
plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", main = "Metric MDS")

transpose = t(as.matrix(purchase))
d.test = gower.dist(transpose, var.weights = NULL)
fit2 = MASS :: isoMDS(d.test)
fit2$stress # [1] 0.001526409 small stress value is good; means we can continue with cluster analysis and that there are likely clusters here

####    k-means    ####
df <- na.omit(purchase)
#df <- df[,c(1,2,3,5,7,8,10,11)]
df.clus = kmeans(df, 3)
plot(df$PctDealPurchases, df$AvgPurchaseMnt, col = df.clus$cluster + 1)


####    k-medoids    ####
#find optimal number of clusters
library(cluster)
library(factoextra)
pam(df, 3, metric = "euclidean", stand = FALSE)

fviz_nbclust(df, pam, method = "wss") # number of clusters vs. total withinn sum of squares

gap_stat <- clusGap(df, FUN = pam, K.max = 10, B = 50) # number of clusters vs. gap statistic
fviz_gap_stat(gap_stat) #based on this, 3 clusters seems to be a good test

#perform k-medoids clustering
set.seed(1)
kmed <- pam(df, k = 3)
kmed
fviz_cluster(kmed, data = df)

plot(x, y, col = kmed$clustering, xlab = "Coordinate 1", ylab = "Coordinate 2", main = "Metric MDS")

####    hierarchical clustering    ####
d <- as.dist(d)
h.cluster = hclust(d)
plot(h.cluster, cex = 0.6, hang = -1) # cluster dendogram

clusters = cutree(h.cluster, k = 4)
plot(x, y, col = clusters, xlab = "Coordinate 1", ylab = "Coordinate 2", main = "Metric MDS")

####    density clustering    ####
library(dbscan)
library(fps)

dens = dbscan(df, eps = 1, minPts = 10) #check to see how many clusters produced; note df is scaled
dens #20 clusters
plot(x, y, col = dens$cluster + 1, xlab = "Coordinate 1", ylab = "Coordinate 2", main = "Metric MDS with Density Clustering")

###    examine performance    ###
df.clus$cluster %% 3 + 1
table(df.clus$cluster %% 3 + 1, as.numeric(ds$CustomerLen))

####    add clusters to dataset    ####
library(dplyr)
ds = mutate(purchase, cluster = kmed$clustering)

# clusters
s1 = 0
s2 = 0
s3 = 0
for (i in 1:length(ds$cluster)) {
  if(ds$cluster[i] == 1){
    s1 = s1 + 1
  } 
  else if(ds$cluster[i] == 2){
    s2 = s2 + 1
  }
  else{s3 = s3 + 1}
}
slices <- c(s1, s2, s3)
lbls <- c("Cluster 1 -","Cluster 2 -", "Cluster 3 -")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices, col = c(2, 3, 4), labels = lbls, radius = 1, main="Cluster Breakout")

####    analysis on new clusters    ####
# demographic exploratory variables: Age, AgeRegistered, Income
boxplot.stats(ds$Income)$out # extreme outliers: 157243 162397 160803 157733 157146 666666
boxplot(Income ~ cluster, col = c(2,3,4), data = ds, outline = FALSE, xlab = "Cluster", ylab = "Income") 

boxplot(Age ~ cluster, col = c(2,3,4), data = ds, outline = FALSE, xlab = "Cluster", ylab = "Age") #not necessarily useful
boxplot(AgeRegistered ~ cluster, col = c(2,3,4), data = ds, xlab = "Cluster", ylab = "Age Registered") #not necessarily useful

# barplot exploratory variables: CustomerLen, Education, Marital_Status, KidsHousehold
# length of customer registration
table <- table(ds$cluster, ds$CustomerLen)
rowSum <- rowSums(table)
prop.tb = data.frame()
for(i in 1:dim(table)[1]){
  for (j in 1:length(rowSum)) {
    prop.tb[i,j] = table[i,j]/rowSum[i]
  }
}
prop.tb = as.matrix(prop.tb)
colnames(prop.tb) = colnames(table)
rownames(prop.tb) = rownames(table)
barplot(prop.tb, main = "Length of Customer Registration", xlab = "Years", col = c(2,3,4), beside = TRUE)
legend("topright", c("Cluster 1", "Cluster 2", "Cluster 3"), fill = c(2,3,4))

# marital status
table <- table(ds$cluster, ds$Marital_Status)
rowSum <- rowSums(table)
prop.tb = data.frame()
for(i in 1:dim(table)[1]){
  for (j in 1:dim(table)[2]) {
    prop.tb[i,j] = table[i,j]/rowSum[i]
  }
}
prop.tb = as.matrix(prop.tb)
rownames(prop.tb) = rownames(table)
colnames(prop.tb) = c("Partner","No Partner")
barplot(prop.tb, main = "Partnership Status", col = c(2,3,4), beside = TRUE)
legend("topright", c("Cluster 1", "Cluster 2", "Cluster 3"), fill = c(2,3,4))

# dependents
table <- table(ds$cluster, ds$KidsHousehold)
rowSum <- rowSums(table)
prop.tb = data.frame()
for(i in 1:dim(table)[1]){
  for (j in 1:dim(table)[2]) {
    prop.tb[i,j] = table[i,j]/rowSum[i]
  }
}
prop.tb = as.matrix(prop.tb)
rownames(prop.tb) = rownames(table)
colnames(prop.tb) = colnames(table)
barplot(prop.tb, main = "Dependents in Househols", xlab = "Number of Dependents", col = c(2,3,4), beside = TRUE)
legend("topright", c("Cluster 1", "Cluster 2", "Cluster 3"), fill = c(2,3,4))

# education
table <- table(ds$Education, ds$cluster)
colSum <- colSums(table)
prop.tb = data.frame()
for(i in 1:dim(table)[2]){
  for (j in 1:dim(table)[1]) {
    prop.tb[j,i] = table[j,i]/colSum[i]
  }
}
prop.tb = as.matrix(prop.tb)
rownames(prop.tb) = c("Basic","Bachelor","Masters","PhD")
colnames(prop.tb) = colnames(table)
barplot(prop.tb, main = "Education Level", xlab = "Cluster", col = c(4,5,6,7), beside = TRUE)
legend("topright", c("Basic", "Bachelor", "Masters", "PhD"), fill = c(4,5,6,7))


####    cluster analysis: exploratory variables    ####
###    purchasing behavior    ###
#     average bill insights
boxplot(AvgPurchaseMnt ~  cluster, data = ds, col = c(2,3,4), main = "Average Purchase Amount", xlab = "cluster") #customers that have been registered longer seem to have larger checks on average

boxplot(TotalNumPurchases ~  cluster, data = ds, col = c(2,3,4), main = "Number of Purchases in Past 2 Years", xlab = "cluster")

#     customer taking advantages of deals offerred
boxplot(PctDealPurchases ~  cluster, data = ds, col = c(2,3,4), main = "% of Purchases Made with Discount", xlab = "cluster")

boxplot(NumDealsPurchases ~  cluster, data = ds, col = c(2,3,4), main = "Number of Purchases Made with Discount", xlab = "cluster")


###    interaction with company   ###
#     customer engagement
boxplot(NumWebPurchases ~  cluster, data = ds, col = c(2,3,4), main = "Place of Purchase: Website", xlab = "cluster")
boxplot(NumCatalogPurchases ~  cluster, data = ds, col = c(2,3,4), main = "Place of Purchase: Catalog", xlab = "cluster")
boxplot(NumStorePurchases ~  cluster, data = ds, col = c(2,3,4), main = "Place of Purchase: Store", xlab = "cluster")

boxplot(NumWebVisitsMonth ~  cluster, data = ds, col = c(2,3,4), main = "Number of Website Visits in Past Month", xlab = "cluster")
