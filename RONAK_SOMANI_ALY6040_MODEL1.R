library(dplyr)
library(stringr)
library(DT)
library(knitr)      
library(tidyverse)  
library(data.table) 
library(caret)      
library(ggplot2)
library(gplots)
library(ROCR)      
library(kableExtra) 
library(gridExtra) 
library(lattice)

#Loading Files
aisles <- read.csv("aisles.csv")
dim(aisles)
head(aisles)

depts <- read.csv("departments.csv")
dim(depts)
head(depts)

opp <- read.csv("order_products__prior.csv")
dim(opp)
head(opp)

opt <- read.csv("order_products__train.csv")
dim(opt)
head(opt)

orders <- read.csv("orders.csv")
dim(orders)
head(orders)

length(
  unique(
    orders$user_id
  )
)

products <- read.csv("products.csv")
dim(products)
head(products)

#Benchmark dataset Output
sampsub <- read.csv("sample_submission.csv", stringsAsFactors = F)
dim(sampsub)
head(sampsub)

all(sampsub$products==sampsub$products[1])

sampbasket <- as.numeric(strsplit(sampsub$products[1],"\\ ")[[1]])

products %>% filter(product_id %in% sampbasket)

#Partitioning off a validation set
trainorders <- orders %>% filter(eval_set=="train") %>% select(order_id)
set.seed(9383)
intrain <- as.logical(
  rbinom(
    n = nrow(trainorders), 
    size = 1, 
    prob = 0.8
  )
)
trainingIDs <- trainorders %>% filter(intrain)
validationIDs <- trainorders %>% filter(!intrain)
nrow(trainingIDs)
nrow(validationIDs)

#Now we can set up a function to evaluate how accurate our attempts are. Let’s assign the “going bananas benchmark” to our validation set.
valsample <- data.frame(order_id = validationIDs, products = sampsub[1,2])
valsample[,2] <- as.character(valsample[,2])
head(valsample)

#To evaluate this, we need to find the “ground truth” for our validation set. For each order number, we need to get a list of products which appear in that order and which are repeat orders. We can do this with the following pipeline
##
## Subset to training orders
##
groundtruth <- filter(orders, eval_set == "train") %>% 
  ##
  ## Join with the training data frame "opt"
  ## to get the product IDs
  ##
  left_join(opt) %>% 
  ##
  ## Only consider reordered items;
  ## only use order_id and product_id columns
  ## 
  filter(reordered == 1) %>% select(order_id, product_id) %>%
  ##
  ## Create a new column with a string listing
  ## the products in each order
  ##
  group_by(order_id) %>% mutate(truth = paste0(product_id, collapse = " ")) %>% 
  ##
  ## Use only order_id and "truth" columns and delete
  ## duplicate rows.
  ##
  select(order_id, truth) %>% unique() %>% data.frame()
## Let's see what the data frame looks like
tail(groundtruth)

#F1 score
#Let’s create some functions to compute the F1 score:
string.ratio <- function(s1, s2){
  mean( strsplit(s1, split = " ")[[1]] %in% strsplit(s2, split = " ")[[1]] )
}

f1strings <- function(s1, s2){
  p = string.ratio(s1, s2)
  r = string.ratio(s2, s1)
  f1 = ifelse(p != 0 & r != 0,
              2 * p * r / (p + r),
              0)
  f1
}

#Now, with our ground truth data frame and our functions to compute F1, we can write a function that takes a data frame of order_ids and predictions and outputs a data frame with a new column for the F1 score of that prediction:
pred.to.f1 <- function(df){
  groundtruth %>% inner_join(df) %>% mutate(f1 = f1strings(products, truth)) %>% select(order_id, f1) 
}

#Let’s use this to compute the F1 score for the validation sample:
mean(pred.to.f1(valsample)$f1)

#A better benchmark
#First let’s look at a single example so we can work out how to do this. Let’s look at user 178520.
ord1 <- orders %>% filter(user_id == 178520) %>% select(order_id) %>% inner_join(opp) 

n1 <- ord1 %>% distinct(order_id) %>% nrow()
fracs1 <- ord1 %>% count(product_id) %>% mutate(frac = n/n1)
#We see that very few of the products are chosen in over 50% of orders, so this seems to be quite conservative:
qplot(fracs1$frac)

#Actually there are six items orders over half the time:
guess1 <- fracs1 %>% filter(frac >= 0.5)
guess1 %>% inner_join(products) %>% select(frac, product_name)

#So this person mostly buys breakfast supplies and energy drinks.
#Let’s put this as our guess for this one user:

ans1 <- guess1$product_id %>% paste(collapse = " ")
val1 <- orders %>% filter(eval_set == "train", user_id == 178520) %>% select(order_id) %>% mutate(products = ans1)
val1
#We can find the F1 score of this one example by running
f1strings((groundtruth %>% filter(order_id == 2331095) %>% select(truth))[[1,1]], ans1)

#Now we need to apply the same manipulations automatically to the whole prior dataset.
practice <- data.frame(user_id = c(1,1,1,1,2,2,2,2,2), order_id = c(1,1,2,2,3,3,3,4,4), product_id = c(7,9,7,10,8,11,7,6,11))

ordercount <- practice %>% group_by(user_id) %>% summarize(norders = n())

practice %>% group_by(user_id, product_id) %>% summarize(count = n()) %>% inner_join(ordercount) %>% mutate(frac = count/norders) %>% filter(frac >= 0.5)

## Making a data frame listing the fraction of times a product appeared
## in a user's order

## The computation takes a bit of time so let's save the output

if(!file.exists("orderfracs.rds")){
  ##  
  ## Link the prior orders to the order data to get user IDs
  ##
  benchdat <- opp %>% inner_join(orders) %>% 
    ##
    ## Keep only the user_id, order_id and product_id columns
    ##
    select(user_id, order_id, product_id)
  ##
  ## Make a new data frame norders listing the number
  ## of distinct orders for each user
  ##
  norders <- benchdat %>% group_by(user_id) %>% summarize(number_orders = n_distinct(order_id))
  ##
  ## Make a new data frame listing which fraction of a user's
  ## orders include a product.
  ## First count the occurrences of each product
  ##
  orderfracs <- benchdat %>% group_by(user_id, product_id) %>% summarize(count = n()) %>% 
    ##
    ## Then join to the data frame with the number of orders
    ## per user and compute the fraction
    ##
    inner_join(norders) %>% mutate(frac = count/number_orders) %>% 
    ##
    ## Return only the fields we need
    ##
    select(user_id, product_id, frac) 
  
  saveRDS(orderfracs, file = "orderfracs.rds")
} else {
  orderfracs <- readRDS("orderfracs.rds")
}

bench2 <- orderfracs %>% filter(frac >= 0.5)

filtered.to.form <- function(df){
  df %>% group_by(user_id) %>% mutate(products = paste0(product_id, collapse = " ")) %>% select(user_id, products) %>% unique()
}

bench2form <- filtered.to.form(bench2)

form.to.train <- function(df){
  filter(orders, eval_set == "train") %>% left_join(df) %>% select(order_id, products) %>% mutate(products = ifelse(is.na(products),"",products))
}

guesses1 <- form.to.train(bench2form)

f1scores <- pred.to.f1(guesses1)

mean(f1scores$f1)

# Let's make the data frame with the same pipeline as above

guess1test <- filter(orders, eval_set == "test") %>% left_join(bench2form) %>% select(order_id, products) %>% mutate(products = ifelse(is.na(products),"None",products))

write.csv(guess1test, file = "guess1.csv", quote = F, row.names = F)

#Uploading the test set with this cutoff
# Let's make the data frame with the same pipeline as above

prods.26 <- orderfracs %>% filter(frac >= 0.26) %>% filtered.to.form()

test.26 <- filter(orders, eval_set == "test") %>% left_join(prods.26) %>% select(order_id, products) %>% mutate(products = ifelse(is.na(products),"None",products))

write.csv(test.26, file = "cutoff26.csv", quote = F, row.names = F)

#logistic regression 
word.in.sentence <- function(word, sentence){
  word %in% strsplit(sentence, split = " ")[[1]]
}

if(!file.exists("prodsinorderdays.RDS")){
  trainordersdays <- orders %>% filter(eval_set == "train") %>% select(order_id, user_id, days_since_prior_order)
  
  trainordersprods <- opt %>% filter(reordered == 1) %>% group_by(order_id) %>% mutate(prods = paste0(product_id, collapse = " "))  %>% select(order_id, prods) %>% unique()
  
  prodsinorderdays <- trainordersdays %>% inner_join(orderfracs) %>% inner_join(trainordersprods) %>% group_by(order_id) %>% mutate(inorder = word.in.sentence(product_id, prods)) %>% data.frame()
  
  saveRDS(prodsinorderdays, "prodsinorderdays.RDS")
} else {
  prodsinorderdays <- readRDS("prodsinorderdays.rds")
}

head(prodsinorderdays)

#logistic regression model with days_since_prior_order and frac as the regressors. 

if(!file.exists("dayfraccoefs.rds")){
  if(!file.exists("dayfracfit.rds")){
    dayfracfit <- glm(inorder ~ frac + days_since_prior_order, data = prodsinorderdays, family = binomial)
    saveRDS(dayfracfit, "dayfracfit.rds")
  } else {
    dayfracfit <- readRDS("dayfracfit.rds")
  }
  dayfraccoefs <- summary(dayfracfit)$coef[,1]
  saveRDS(dayfraccoefs, "dayfraccoefs.rds")
} else {
  dayfraccoefs <- readRDS("dayfraccoefs.rds")
}
dayfraccoefs

#function to give the response for a given value of frac and days_since_prior_order.
day.frac.response <- function(frac, days_since_prior_order){
  linear = dayfraccoefs[1] + dayfraccoefs[2] * frac + dayfraccoefs[3] * days_since_prior_order
  1 / (1 + exp(- linear))
}

#plot of the response as a function of frac, showing how it varies with the number of days—the impact of days_since_prior_order is small but not negligible.

df.days <- data.frame(days_since_prior_order = c(rep(1,101),rep(15,101),rep(30,101)), frac = 0.01 * 0:100) %>% mutate(response = day.frac.response(frac, days_since_prior_order))
##
df.days$days_since_prior_order <- as.factor(df.days$days_since_prior_order) 
##
ggplot(df.days, aes(x = frac, y = response, colour = days_since_prior_order)) + geom_line(lwd = 2, alpha = 0.8)

#let us make predictions on the test dataset.
predtestfracdays <- orders %>% filter(eval_set == "test") %>% select(order_id, user_id, days_since_prior_order) %>% inner_join(orderfracs) %>% mutate(response = day.frac.response(frac, days_since_prior_order))

if(!file.exists("predday26.rds")){
  predday26 <- predict(dayfracfit, newdata = data.frame(days_since_prior_order = c(1,15,30), frac = 0.26), type = "response")
  saveRDS(predday26, "predday26.rds")
} else {
  predday26 <- readRDS("predday26.rds")
}
predday26

#check some values in the region of 0.1 to 0.15. We’ll use the training dataset again. We need to get a data frame with the order number, days since prior order, and frac variable.
odpf <- orders %>% filter(eval_set == "train") %>% select(order_id, user_id, days_since_prior_order) %>% inner_join(orderfracs) %>% mutate(response = day.frac.response(frac, days_since_prior_order))

#let’s try different values of the cutoff, i.e. let’s vary p, make the prediction that an item is in the cart if the response from the logistic regression is ≥p, and see which value of p gives the best value of F1.
if(!file.exists("dayscores.rds")){
  day.scores <- NULL
  for(i in 0:20){
    p <- 0.1 + 0.0025 * i
    print(i)
    prod.pred <- odpf %>% filter(response >= p) %>% group_by(order_id) %>% mutate(products = paste0(product_id, collapse = " ")) %>% select(order_id, products) %>% unique()
    ##
    ##
    ##
    f1.scores <- pred.to.f1(prod.pred)
    day.scores <- c(day.scores, mean(f1.scores$f1))
  }
  saveRDS(day.scores,"dayscores.RDS")
} else {
  day.scores <- readRDS("dayscores.rds")
}
if(!file.exists("dayscoresfine.rds")){
  day.scores.fine <- NULL
  for(i in 0:19){
    p <- 0.125 + 0.00025 * i
    print(i)
    prod.pred <- odpf %>% filter(response >= p) %>% group_by(order_id) %>% mutate(products = paste0(product_id, collapse = " ")) %>% select(order_id, products) %>% unique()
    ##
    ##
    ##
    f1.scores <- pred.to.f1(prod.pred)
    day.scores.fine <- c(day.scores.fine, mean(f1.scores$f1))
  }
  saveRDS(day.scores.fine,"dayscoresfine.rds")
} else {
  day.scores.fine <- readRDS("dayscoresfine.rds")
}

#We can plot our results:
day.scores.df <- data.frame(cutoff = c(0.1 + 0.0025 * (0:20), 0.125 + 0.00025 * (0:19)), F1 = c(day.scores, day.scores.fine))
#
best.cutoff = 0.128125
#
ggplot(day.scores.df, aes(x = cutoff, y = F1)) + geom_line(lwd = 1) + geom_vline(xintercept = best.cutoff)


##
## Make a data frame with test order_id's, frac and days_since_prior_order
##
test.odpf <- orders %>% filter(eval_set == "test") %>% select(order_id, user_id, days_since_prior_order) %>% inner_join(orderfracs) %>% 
  ##
  ## Predict the response
  ##
  mutate(response = day.frac.response(frac, days_since_prior_order))

##
## Subset to the products with scores above the cutoff
##
frac.day.pred <- test.odpf %>% filter(response >= best.cutoff) %>% 
  ##
  ## Put output in desired form
  ##
  group_by(order_id) %>% mutate(products = paste0(product_id, collapse = " ")) %>% select(order_id, products) %>% unique()

##
## Make sure we have all order_id's from test set, setting "None"
## if there are no products predictes
##
frac.day.pred.all <- filter(orders, eval_set == "test") %>% left_join(frac.day.pred) %>% select(order_id, products) %>% mutate(products = ifelse(is.na(products),"None",products))


write.csv(frac.day.pred.all, file = "fracdaypred.csv", quote = F, row.names = F)

