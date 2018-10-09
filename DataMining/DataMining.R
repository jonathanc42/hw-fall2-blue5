install.packages('arules')
library(arules)

groceries = read.transactions("groceries.csv", sep = ",")

summary(groceries)

inspect(groceries[1:5])

itemFrequency(groceries[, 1:3])

itemFrequencyPlot(groceries, support = 0.1)

itemFrequencyPlot(groceries, topN = 20)

image(groceries[1:100])

> apriori(groceries)

> rules <- apriori(groceries, parameter = list(support = 0.006, confidence = 0.25, minlen = 2))

> rules

set of 463 rules

> summary(rules)

> inspect(rules[1:3])

> inspect(sort(rules, by = "lift")[1:5])


> sweetrules <- subset(rules, items %in% c("chocolate","ice cream"))

> inspect(sweetrules)