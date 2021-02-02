# This file is used to create reproducable random sampling of the orders and order_product datasets. Should be used
# if working on those whole datasets is too computationally strenuous.

library(tidyverse)

# Reading in the original data
orders <- read_csv("orders.csv") %>%
  mutate(order_dow = as.character(recode(order_dow, `0`="Sunday", `1`="Monday", `2`="Tuesday",
                                         `3`="Wednesday", `4`="Thurday", `5`="Friday", `6`="Saturday")))

order_products_prior <- read_csv("order_products__prior.csv")
order_products_train <- read_csv("order_products__train.csv")
order_products <- rbind(order_products_prior, order_products_train)

# Randomly sampling the data
set.seed(42)  # Allows for reproducable sample results
random_users <- sample(unique(orders$user_id), 40000)  #40000 users are randomly chosen (~20% sample)
orders_sample <-
  subset(orders, user_id %in% random_users)

order_products_sample <-
  subset(order_products, order_id %in% unique(orders_sample$order_id))

# Before sampling:
#   orders - 3.4 million rows  
#   order_products - 32.4 million rows

#After sampling:
#   orders - 660 thousand rows
#   order_products - 6.23 million rows

write_csv(orders_sample, "orders_sample.csv")
write_csv(order_products_sample, "order_products_sample.csv")



library(tidyverse)
library(ape)                #to make hierarchical clusters
library(mclust)             #for k-means clustering
library(BBmisc)             #for data normalization


# Reading In The Data

orders <- read_csv("orders_sample.csv") %>%
  mutate(order_hour_of_day = as.numeric(recode(order_hour_of_day, "00"=0, "01"=1, "02"=2, "03"=3, "04"=4, "05"=5, "06"=6, "07"=7, "08"=8, "09"=9, "10"=10, "11"=11, "12"=12, "13"=13, "14"=14, "15"=15, "16"=16, "17"=17, "18"=18, "19"=19, "20"=20, "21"=21, "22"=22, "23"=23)))
order_products <- read_csv("order_products_sample.csv")
products <- read_csv("products.csv")  # Used to provide product names (hasn't been incorporated yet)


# Preparing the Data


orders$eval_set <- as.factor(orders$eval_set)
orders$order_dow <- as.factor(orders$order_dow)
products$product_name <- as.factor(products$product_name)
products$aisle_id <- as.factor(products$aisle_id)


#We want to establish our clustering only on the `prior` dataset.

orders <- orders %>% filter(eval_set == "prior")
order_products <-
  order_products %>%
  inner_join(orders, by="order_id")
order_products[is.na(order_products)] <- 0



# Research Question

#Can we cluster users into different groups based on the products they ordered, the time of their orders and if they reordered or not?
  
  # Clustering
  
#**Clustering Users Using K-means**
  
# First I tried K-means clustering to cluster the users. I chose K-means clustering because it provides a clear and straightforward visualtion of the groups. Also, I can adjust how many groups I want. The variables used to cluster users include what products they ordered, during what time of the day they placed the order, the order of products they added the cart, if the user has reordered and how many days the user reordered. 


# select clustering variables
user_vars <- order_products %>%
  select(product_id,order_hour_of_day,add_to_cart_order,reordered,days_since_prior_order)


#I first tried to cluster the users into 5 groups. 


set.seed(1)
num_clusters <- 5
user_clusts <- user_vars %>%
  kmeans(centers=num_clusters) %>%
  fitted("classes") %>%  
  as.character()
users <- order_products %>%
  mutate(cluster=user_clusts)


#I plotted product_id on the x-axis and order_hour_of_day on the y-axis. 


ggplot(users,aes(x=product_id,y=order_hour_of_day))+geom_point(aes(color=cluster))


#It seems that product_id is the dominant determinant of the group clustering at this time. Also, because the dataset is too large, it takes a lot of time to plot the graph. Therefore, I only included the first 1000 rows in the sample data below. If this method works, I can apply the clustering to the entire population. 


sample <- order_products[1:1000,]



# select clustering variables
user_vars <- sample %>%
  select(product_id,order_hour_of_day,add_to_cart_order,reordered,days_since_prior_order)



set.seed(1)
num_clusters <- 5
user_clusts <- user_vars %>%
  kmeans(centers=num_clusters) %>%
  fitted("classes") %>%  
  as.character()
users <- sample %>%
  mutate(cluster=user_clusts)



ggplot(users,aes(x=product_id,y=order_hour_of_day))+geom_point(aes(color=cluster))


#This time, the graph looks better. Product_id is still the most important determinant.


ggplot(users,aes(x=add_to_cart_order,y=days_since_prior_order))+geom_point(aes(color=cluster))

#Then I plotted add_to_cart_order and days_since_prior_order. It seems that neither of the two variables matter to the clustering. The clusters are randomly distributed in this case. Now, I'm reflecting: where did I do wrong?

#I realize that I did not normalize the variables before clustering. Because the product_id usually contains very large numbers, they are weighted more. Therefore, I'm going to normalize the variables below. 

# Normalize variables


# normalize values before K-means clustering
user_norm<-normalize(user_vars)



set.seed(1)
num_clusters <- 5
user_clusts <- user_norm %>%
  kmeans(centers=num_clusters) %>%
  fitted("classes") %>%  
  as.character()
users <- sample %>%
  mutate(cluster=user_clusts)


ggplot(users,aes(x=product_id,y=order_hour_of_day))+geom_point(aes(color=cluster))


#I replotted the graph about product_id. Now, product has no dominant effect. 

ggplot(users,aes(x=add_to_cart_order,y=days_since_prior_order))+geom_point(aes(color=cluster))

#Then I also checked the rest two variables. The clusters are less randomed than before.


#**Clutering Users Using Hierarchical Clustering**
  
#Next, I tried hierarchical clustering (bottom_down) for the users. I chose this method because it provides a good visualization of the different levels and how the clusters are made.


reorderedUsers <- sample %>%
  filter(reordered==1)
# The variables we are going to use to cluster reordered users
user_vars <- reorderedUsers %>%
  select(product_id,order_hour_of_day,add_to_cart_order,reordered,days_since_prior_order)


#First, I clustered based on the unnormalized variables.

user_diffs<-dist(user_vars)
user_diffs%>%
  hclust() %>%
  as.phylo() %>%
  plot(cex=0.6,label.offset=1)


#Next, I clustered based on the normalized variables.

# normalize values before clustering
users_norm <- normalize(user_vars)
user_diffs<-dist(user_norm)
user_diffs%>%
  hclust() %>%
  as.phylo() %>%
  plot(cex=0.6,label.offset=1)


#Even though the hierarchical clusters are very dense, I can still compare the differences between the unnormalized and normalized one. For the unnormalized graph, it is clear that there are a few dominant variables that divide the groups. On the other hand, for the nomrlized graph, it's a combination of many little things. 

#Overall, the hierarchical cluster are hard to read because one user has many characteristics and it is hard to plot all of them in one clear tree. The hierarchical cluster might be more useful for products other than users. The K-means gives a good and clear clustering of the users. 

