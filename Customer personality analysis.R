############################# Customer Analysis ###############################
# Aim: Summarizing customer segments by performing cluster analysis           #
#                                                                             #
#                                                                             #
#                                                                             #
########################Loading packages ######################################
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)


#Loading Data 
customer <- read_excel("C:/Users/sasee/Downloads\\marketing_campaign.xlsx")


######################## Data Exploration ######################################

#structure 
str(customer)

#dimensions
dim(customer) #2240 rows and 29 columns

# checking for missing values 
colSums(is.na(customer)) #24 missing values in Income column.

#####################handling missing values ##################################

library(mice)
                                                     ###################################
customer_subset <- subset(customer, select =c(1,5) ) ## subsetting ID and Income    ####
                                                     ## because the data is too large###
                                                     ###################################

imputed_data <- mice(customer_subset, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_data)
imputed_data$imp$Income  #######imputing data using method pmm

## Looking at the imputed column all together
comp <- complete(imputed_data, 2)

##merging imputed data with the original 
customer_data <- merge(customer, comp,by="Income",all.x=FALSE, all.y = TRUE)

colSums(is.na(customer_data))##### no missing values now 



########## Data Manipulation 
# combining kids and teen columns 
customer_data$Child <- customer_data$Kidhome + customer_data$Teenhome

#removing unimportant variables
cus_data <- subset(customer_data, select = -c(27,28,  6,7, 2, 30))
names(customer_data)
dim(customer)###### new dimension of the final data




########################### correlation matrix ###############################
library(GGally)

customercorr <- cus_data %>%
  select_if(is.numeric)

ggcorr(data = customercorr, label = T) +
  labs(title = "Correlation Matrix")


########## As shown in the legend red boxes indicate strong correlation ########
##########whereas blue ones show negative correlation ########################





##########################key visualizations ##################################

##Histogram in joining date

ggplot(cus_data,aes(Dt_Customer))+
  geom_histogram(aes(y=..count..*10),
                 bins=30, 
                 fill="#800040")+
  labs(title = "Distribution of customers joining date", y= "Frequency", X= "Date")


#####summing all  types of purchases by year
website <- aggregate(cus_data[,17], by = list(cus_data[,2]),FUN=sum)

store <- aggregate(cus_data[,16], by = list(cus_data[,2]),FUN=sum)

catlog <- aggregate(cus_data[,15], by = list(cus_data[,2]),FUN=sum)

web <- aggregate(cus_data[,14], by = list(cus_data[,2]),FUN=sum)

deals <- aggregate(cus_data[,13], by = list(cus_data[,2]),FUN=sum)

type <- data.frame(website, store, catlog, web, deals)#####creating data frame of the summed values
names(type) <- c("Year_", 
                 "website_", 
                 "year2", 
                 "store_",  
                 "year3", 
                 "catlog_",  
                 "year4", 
                 "web_", 
                 "year5", 
                 "deals_")##################renaming columns

### ploting ###

 ggplot(type, aes(x=Year_)) + 
  geom_line(aes(y = website_), color = "#cc2900") +
 geom_line(aes(y = store_), color="steelblue", linetype="solid",size= 1,show.legend = TRUE)+
  geom_line(aes(y = catlog_), color="#bf00ff", linetype="solid", size= 1,show.legend = TRUE)+
  geom_line(aes(y = web_), color="#e60073", linetype="solid",size= 1,show.legend = TRUE)+
  geom_line(aes(y = deals_), color="#29a329", linetype= "solid", size= 1,show.legend = TRUE)+
   labs(title = "Modes of purchases by Birth year", y= "Frequency", x=" dob")

### Pie of customer complaints
 table(cus_data$Complain)
 
 table_compl<- data.frame(Complaints= c("Yes", "No" ),
                         Freq= c( 23  , 2843 ))
 
 library(webr)
 
 PieDonut(table_compl, 
          aes(Complaints,  count= Freq), 
          explode = 2, 
          r0 = 0.45, r1 = 0.9)  
 
 
#################################clustering ###################################
library(factoextra)

data_scale <- scale(customercorr)
f_data1 <- dist(customercorr)  

########### Visualizing elbow plot 
fviz_nbclust(data_scale, kmeans, method="wss")+
  labs(subtitle= "elbow method")

#########kmeans cluster with 4 groups 
km.out <- kmeans(data_scale, centers=4, nstart = 100)
print(km.out)

###############visualize the cluster 
km.clusters <- km.out$cluster

fviz_cluster(list(data= data_scale, cluster= km.clusters))  

#######################Summarizing clusters####################################
cluster_results <- customercorr %>%
  mutate(cluster = km.out$cluster) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

cluster_results$the_summary





