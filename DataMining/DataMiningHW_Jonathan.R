install.packages('arules')
library(arules)
library(dplyr)

#### get list of order items per category ####
data <- read.csv("DataMining/restaurantData.csv")

list(data %>% select (type) %>%
       distinct())

Meat <- list(data %>% filter(type == 'Meat') %>% 
               select (order) %>%
               distinct())

Side <- list(data %>% filter(type == 'Side') %>% 
               select (order) %>%
               distinct())

Wine <- list(data %>% filter(type == 'Wine') %>% 
               select (order) %>%
               distinct())

#### remove the sides ####

orderdata <- read.csv("DataMining/restaurantData.csv")
orderdata <- orderdata %>%
  filter(type!='Side') %>%
  select(3,1)

# rewrite back to file
write.csv(orderdata,"DataMining/restaurantData_noside.csv")

#### load transaction data ####

# format = c("basket", "single")
data<-read.transactions("DataMining/restaurantData_noside.csv",
                        sep=',',
                        format='single',
                        col=c('orderNumber','order'))

#### assosiation analysis ####
apriori(data)
summary(data)
# inspect(data[1:5])
itemFrequency(data[, 1:3])
itemFrequencyPlot(data, support = 0.1)

itemFrequencyPlot(data, topN = 20)
image(data[1:100])

rules <- apriori(data, parameter = list(support = 0.001, confidence = 0.01,
                                        minlen = 2, maxlen = 2),
                 appearance = list(lhs=c('Filet Mignon','Pork Tenderloin','Roast Chicken','Duck'),
                                   rhs=c('Blackstone Merlot','Cantina Pinot Bianco','Meiomi Pinot Noir','Duckhorn Chardonnay')))

summary(rules)

rules

inspect(head(rules, by = "lift", n  = 3))


inspect(rules)
inspect(sort(rules, by = "lift"))
inspect(sort(rules, by = "support"))
#rules@quality$lift

#sweetrules <- subset(rules, items %in% c("chocolate","ice cream"))
#inspect(sweetrules)


#### Association for all three ####
meal<-read.transactions("DataMining/restaurantData.csv",
                        sep=',',
                        format='single',
                        col=c('orderNumber','order'))

mrules <- apriori(meal,parameter = list(support = 0.001,
                                        confidence = 0.001,
                                        minlen=3,maxlen=3),
                  appearance = list(rhs=c('Blackstone Merlot','Cantina Pinot Bianco','Meiomi Pinot Noir','Duckhorn Chardonnay')))
inspect(sort(mrules, by = "lift"))
inspect(sort(mrules, by = "support"))

#### Try not remove the sides but specify item name ####

meatrules <- apriori(meal,parameter = list(support = 0.001,
                                           confidence = 0.001,
                                           minlen=2,maxlen=2),
                     appearance = list(lhs=c('Filet Mignon','Pork Tenderloin','Roast Chicken','Duck'),
                                       rhs=c('Blackstone Merlot','Cantina Pinot Bianco','Meiomi Pinot Noir','Duckhorn Chardonnay')))

inspect(sort(meatrules, by = "lift"))
inspect(sort(meatrules, by = "count"))
write.csv(inspect(sort(meatrules, by = "lift")),"DataMining/result.csv")


all.equal(df3,df4)