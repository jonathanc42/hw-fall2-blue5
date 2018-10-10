library(arules)
library(dplyr)

# remove the sides
orderdata<-read.csv("DataMining/restaurantData.csv")
orderdata <- orderdata %>%
  filter(type!='Side') %>%
  select(3,1)

#### rewrite back to file ####
write.csv(orderdata,"DataMining/restaurantData_noside.csv")

data = read.transactions("DataMining/restaurantData_noside.csv", format=c("single"), sep = ",", cols=c(
  "orderNumber", "order"))
#?read.transactions
summary(data)
inspect(data[1:5])
itemFrequency(data[, 1:3])
itemFrequencyPlot(data, support = 0.1)
itemFrequencyPlot(data, topN = 20)
image(data[1:100])
apriori(data)
rules <- apriori(data, parameter = list(support = 0.001, confidence = 0.01,
                                        minlen = 2, maxlen = 2),
                 appearance = list(lhs=c('Filet Mignon','Pork Tenderloin','Roast Chicken','Duck'),
                                   rhs=c('Blackstone Merlot','Cantina Pinot Bianco','Meiomi Pinot Noir','Duckhorn Chardonnay')))
#rules <- apriori(data, parameter = list(support = 0.006, confidence = 0.25, minlen = 2))
rules
summary(rules)
inspect(rules[1:10])
inspect(sort(rules, by = "lift")[1:10])
