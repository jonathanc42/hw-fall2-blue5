library(arules)
library(dplyr)


# remove the sides
orderdata<-read.csv("DataMining/restaurantData.csv")
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

apriori(data)
summary(data)
# inspect(data[1:5])
itemFrequency(data[, 1:3])
itemFrequencyPlot(data, support = 0.1)

itemFrequencyPlot(data, topN = 20)
image(data[1:100])
rules <- apriori(data, parameter = list(support = 0.006, confidence = 0.25, minlen = 2))
rules
summary(rules)

inspect(rules[1:3])
inspect(sort(rules, by = "lift")[1:5])
#rules[order(-rules@quality$lift)]
rules@quality$lift

#sweetrules <- subset(rules, items %in% c("chocolate","ice cream"))
#inspect(sweetrules)