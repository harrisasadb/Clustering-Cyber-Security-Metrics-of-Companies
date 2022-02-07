#PCA
mydata = read.csv("project_data.csv",header = TRUE) #importing data
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
summary(mydata) # all columns with constant values to be removed for scaling
mydata$credentials_red<- NULL
mydata$credentials_amber<- NULL
mydata$credentials_green<- NULL
mydata$phishing_red<- NULL
mydata$phishing_amber<- NULL
mydata$ransomware_red<- NULL
mydata$ransomware_amber<- NULL
mydata$ransomware_green<- NULL

library(corrplot)
corrplot(cor(mydata[2:21]), method="ellipse") #correlation plot to check if correlation matrix needed
#domain number would be ignored in PCA
mydata_pca_cor <- prcomp(apply(df[2:21],2, scale))
mydata_pca_cor #specifications of PCA
screeplot(mydata_pca_cor) #plots the scree plot
summary(mydata_pca_cor) #summarize the result of PCA and give variation percentage
library(ggbiplot) #now biplots constructed of first 4 principal components
ggbiplot(mydata_pca_cor)
biplot(mydata_pca_cor, choices=c(1,2), cex=0.7,col=c("black","red"), scale = 0, xlabs =
         row.names(mydata))
biplot(mydata_pca_cor, choices=c(3,4), cex=0.7,col=c("black","red"), scale = 0, xlabs =
         row.names(mydata))
biplot(mydata_pca_cor, choices=c(1,3), cex=0.7,col=c("black","red"), scale = 0, xlabs =
         row.names(mydata))
biplot(mydata_pca_cor, choices=c(1,4), cex=0.7,col=c("black","red"), scale = 0, xlabs =
         row.names(mydata))
biplot(mydata_pca_cor, choices=c(2,3), cex=0.7,col=c("black","red"), scale = 0, xlabs =
         row.names(mydata))
biplot(mydata_pca_cor, choices=c(2,4), cex=0.7,col=c("black","red"), scale = 0, xlabs =
         row.names(mydata))
