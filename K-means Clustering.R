#K-means clustering
mydata = read.csv("project_data1_final.csv",header = TRUE)
attach(mydata)
mydata$X.orgId <- NULL #removing all unnecessary data
mydata$orgName <- NULL
mydata$userId <- NULL
mydata$domain <- 1:nrow(mydata) #replacing domain column with row number
mydata$domain <- as.character(as.numeric(mydata$domain)) #treat domain column as character
mydata$mis_services_classification<- NULL #removing all factor data
mydata$domains_classification<- NULL
mydata$ood_services_classification<- NULL
mydata$certificates_classification.<- NULL 
mydata$domains_numerator<- NULL # removing all fraction data
mydata$domains_denominator<- NULL
mydata$certificates_numerator<- NULL
mydata$certificates_denominator<- NULL
mydata$mis_services_numerator<- NULL
mydata$mis_services_denominator<- NULL
mydata$ood_services_numerator<- NULL
mydata$ood_services_denominator<- NULL
summary(mydata) # all comumns with constant values to be removed for scaling
mydata$credentials_red<- NULL
mydata$credentials_amber<- NULL
mydata$credentials_green<- NULL
mydata$phishing_red<- NULL
mydata$phishing_amber<- NULL
mydata$ransomware_red<- NULL
mydata$ransomware_amber<- NULL
mydata$ransomware_green<- NULL
library(dplyr)
df <- mydata %>%
  mutate_at(c(2:21), funs(c(scale(.)))) #scaling the data
library(factoextra)
library(NbClust)
#Elbow Method
fviz_nbclust(df[2:21], kmeans, method = "wss") +
  geom_vline(xintercept = 9, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(df[2:21], kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
set.seed(123)
fviz_nbclust(df[2:21], kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

# Compute k-means with k = 9 for elbow method
set.seed(123)
km.res <- kmeans(df[2:21], 9, nstart = 25)

# Print the results
print(km.res)

#paste results of clusters to the main data
dd <- cbind(mydata, cluster = km.res$cluster)
head(dd)

#plot clusters on first two Principal Components
fviz_cluster(km.res, data = df[2:21])
#zoom in for a better view of overlapping clusters
fviz_cluster(km.res, data = df[2:21]) + xlim(-1.5,1.5) + ylim(-1.5,1.5)

# Compute k-means with k = 2 for other methods
set.seed(123)
km.res <- kmeans(df[2:21], 2, nstart = 25)

# Print the results
print(km.res)

#paste results of clusters to the main data
dd <- cbind(mydata, cluster = km.res$cluster)
head(dd)

#plot clusters on first two Principal Components
fviz_cluster(km.res, data = df[2:21])