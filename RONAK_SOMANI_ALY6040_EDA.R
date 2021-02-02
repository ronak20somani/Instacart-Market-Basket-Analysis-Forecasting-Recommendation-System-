library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(stringr)
library(DT)
options(scipen=5)
orders <- fread('orders.csv')
products <- fread('products.csv')
order_products_train<- fread('order_products__train.csv')
order_products_prior <- fread('order_products__prior.csv')
aisles <- fread('aisles.csv')
departments <- fread('departments.csv')

kable(head(orders,12))
glimpse(orders)

kable(head(products,10))
glimpse(products)

kable(head(order_products_train,10))
glimpse(order_products_train)

kable(head(order_products_prior,10))
glimpse(order_products_prior)

kable(head(aisles,10))
glimpse(aisles)

kable(head(departments,10))
glimpse(departments)

### Recode variables
### We should do some recoding and convert character variables to factors. 

orders <- orders %>% mutate(order_hour_of_day = as.numeric(order_hour_of_day), eval_set = as.factor(eval_set))
products <- products %>% mutate(product_name = as.factor(product_name))
aisles <- aisles %>% mutate(aisle = as.factor(aisle))
departments <- departments %>% mutate(department = as.factor(department))

### When do people order?
### Let's have a look when people buy groceries online. 


#### Hour of Day
## There is a clear effect of hour of day on order volume. Most orders are between 8.00-18.00

orders %>% 
  ggplot(mapping = aes(x=order_hour_of_day)) + 
  geom_histogram(stat="count",fill="lightblue")+geom_line(stat="count")+geom_point(stat="count")+theme_bw()+
  labs(x = "ORDER_HOUR_OF_DAY", y = "ORDER VOLUME")+
  ggtitle("MAX. UNIT SOLD AT WHAT TIME OF DAY") + theme(plot.title = element_text(hjust = 0))

#### Day of Week
## There is a clear effect of day of the week. Most orders are on days 0 and 1. Unfortunately there is no info regarding which values represent which day, but one would assume that this is the weekend.

area.color1 <- c("goldenrod4","goldenrod4","goldenrod","goldenrod","goldenrod","goldenrod","goldenrod")
orders %>% 
  ggplot(aes(x=order_dow)) + 
  geom_histogram(stat="count",fill=area.color1)+theme_bw()+
  labs(x = "ORDER_DAY_OF_WEEK", y = "TOTAL ORDER")+
  ggtitle("TOTAL NUMBER OF ORDERS AS PER DAYS OF WEEK") + theme(plot.title = element_text(hjust = 0))
  

### When do they order again?
## People seem to order more often after exactly 1 week. 

orders %>% 
  ggplot(aes(x=days_since_prior_order, fill = ..count..)) + 
  geom_histogram(stat="count")+theme_bw()+
  labs(x = "NUMBER OF DAYS", y = "COUNT OF ORDERS")+
  ggtitle("DAYS SINCE PRIOR ORDER ANALYSIS") + theme(plot.title = element_text(hjust = 0))


### Bestsellers
## Let's have a look which products are sold most often (top10). And the clear winner is:
## **Bananas**


tmp <- order_products_train %>% 
group_by(product_id) %>% 
summarize(count = n()) %>% 
top_n(10, wt = count) %>%
left_join(select(products,product_id,product_name),by="product_id") %>%
arrange(desc(count)) 
kable(tmp)

area.color <- c("CORAL", "CORAL", "CORAL1", "CORAL1","CORAL2", "CORAL2", "CORAL3","CORAL3","CORAL4","CORAL4")
tmp %>% 
ggplot(aes(x=reorder(product_name,count), y=count))+
geom_bar(stat="identity", fill = area.color)+theme_bw()+coord_flip()+
theme(axis.text.x=element_text(angle=0, hjust=1),axis.title.y = element_blank())+
  labs(y = "NUMBER OF ORDERS")+
  ggtitle("BESTSELLER PRODUCT") + theme(plot.title = element_text(hjust = 0))

## Merging The Products_Aisles_Departments Data sets
Products_Aisles<-merge(products,aisles,by="aisle_id")
Products_Aisles_Departments<-merge(Products_Aisles,departments,"department_id")
kable(head(Products_Aisles_Departments,6))

glimpse(Products_Aisles_Departments)

Number_of_Product_each_Aisle<-Products_Aisles_Departments%>%group_by(aisle)%>%summarise(Number_of_Products=n())%>%arrange(desc(Number_of_Products))

#Top 20 Aisle by number of product offerings
Top_20<-head(Number_of_Product_each_Aisle,n=20)

#Plotting Number of Products in each aisle in decreasing order(Top 20)
library(gridExtra)
area.color3 <- c("lightsalmon4", "lightsalmon4", "lightsalmon4", "lightsalmon4","lightsalmon", "lightsalmon", "lightsalmon","lightsalmon","lightsalmon","lightsalmon","lightsalmon", "lightsalmon", "lightsalmon", "lightsalmon","lightsalmon", "lightsalmon", "lightsalmon","lightsalmon","lightsalmon","lightsalmon")
p1 = ggplot(Top_20, aes(x = reorder(aisle,Number_of_Products), y = Number_of_Products,label=paste0(round(Number_of_Products,0)), fill = area.color3))+
  geom_bar(stat = "identity", show.legend = FALSE)+coord_flip()+theme_classic()+
  labs(title="Top 20 Aisle by Variety of Product Offering",y="Number of Products",x="Aisle")+
  geom_text(nudge_y = 35)


#Number of Products in each department

Number_of_Product_each_department<-Products_Aisles_Departments%>%group_by(department)%>%summarise(Number_of_Products=n())%>%arrange(desc(Number_of_Products))


#Vis--Bar chart for number of products in each department
area.color4 <- c("lightsalmon4", "lightsalmon4", "lightsalmon4", "lightsalmon4","lightsalmon", "lightsalmon", "lightsalmon","lightsalmon","lightsalmon","lightsalmon","lightsalmon", "lightsalmon", "lightsalmon", "lightsalmon","lightsalmon", "lightsalmon", "lightsalmon","lightsalmon","lightsalmon","lightsalmon","lightsalmon")
p2 = ggplot(Number_of_Product_each_department, aes(x = reorder(department,Number_of_Products), y = Number_of_Products,label=paste0(round(Number_of_Products,0)), fill = area.color4)) +
  geom_bar(stat = "identity", show.legend = FALSE)+coord_flip()+labs(title="Department by Variety of Product offering",y="Number of Products",x="Department")+ theme_classic()+
  geom_text(nudge_y = 250)

grid.arrange(p1,p2, ncol = 2)
