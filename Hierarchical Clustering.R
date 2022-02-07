#Hierarchial Clustering
mydata = read.csv("project_data1_final.csv",header = TRUE)
attach(mydata)
mydata$mis_services_classification<- NULL
mydata$domains_classification<- NULL
mydata$ood_services_classification<- NULL
mydata$certificates_classification.<- NULL #removing all factor data
mydata$domains_numerator<- NULL # removing all fraction data
mydata$domains_denominator<- NULL
mydata$certificates_numerator<- NULL
mydata$certificates_denominator<- NULL
mydata$mis_services_numerator<- NULL
mydata$mis_services_denominator<- NULL
mydata$ood_services_numerator<- NULL
mydata$ood_services_denominator<- NULL
summary(mydata) # all comumns with constant values to be removed for scaling by pca
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
  mutate_at(c(2:21), funs(c(scale(.))))
dist_mat <- dist(df[2:21], method = 'euclidean') #dissimilarity matrix formed with squared euclidean distance
hclust_comp <- hclust(dist_mat, method = 'complete') #complete linkage used
plot(hclust_comp) #dendogram plotted
cutree(hclust_comp, k = 9) #tree cut to have 9 clusters and data point pasted with their cluster
dist_mat <- dist(df[2:21], method = 'euclidean')
hclust_comp <- hclust(dist_mat, method = 'single') #single linkage used
plot(hclust_comp)
cutree(hclust_comp, k = 9)

#this is extra to plot heat map of clusters
suppressPackageStartupMessages(library(dendextend))
avg_dend_obj <- as.dendrogram(hclust_comp)
avg_col_dend <- color_branches(avg_dend_obj, h = 2)
plot(avg_col_dend)

