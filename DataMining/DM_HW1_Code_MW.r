library(dplyr)
library(tidyr)
library(arules)

# Import Data
df <-read.csv('/Users/mwitebsky/Documents/NCSU/AA502/Data Mining/Data/restaurantData.csv')

# subset to different type pairings
meat_wines <- df %>% filter(type %in% c('Wine', 'Meat'))
write.csv(meat_wines, '/Users/mwitebsky/Documents/NCSU/AA502/Data Mining/Data/meat_wine.csv',
          row.names=FALSE)

meat_sides <- df %>% filter(type %in% c('Meat', 'Side'))
write.csv(meat_sides, '/Users/mwitebsky/Documents/NCSU/AA502/Data Mining/Data/meat_side.csv',
          row.names=FALSE)

# create lists of unique orders for each type
wines <- as.vector(unique(df %>% filter(type=='Wine') %>% select(order))[['order']])
meats <- as.vector(unique(df %>% filter(type=='Meat') %>% select(order))[['order']])
sides <- as.vector(unique(df %>% filter(type=='Side') %>% select(order))[['order']])
meat_wine_types <- list(meats=meats, wines=wines)
meat_side_types <- list(meats=meats, sides=sides)

# read meat_wine into transactions object
meat_wine_orders <- read.transactions('/Users/mwitebsky/Documents/NCSU/AA502/Data Mining/Data/meat_wine.csv',
                                      sep=',', format='single', cols=c('orderNumber', 'order'))
summary(meat_wine_orders)

# print top lift pair rules for each meat and wine
for (type in meat_wine_types) {
  for (item in type) {
    print(item)
    rules <- apriori(meat_wine_orders, parameter=list(supp=.001, conf=.001, minlen=2, maxlen=2), appearance=list(lhs=item), 
                     control=list(verbose=FALSE))
    inspect(sort(rules, by='lift'))
    cat('\n\n-------------------------------------------------------------------\n\n')
  }
}

# read meat side into transaction object
meat_side_orders <- read.transactions('/Users/mwitebsky/Documents/NCSU/AA502/Data Mining/Data/meat_side.csv',
                                      sep=',', format='single', cols=c('orderNumber', 'order'))

# print top lift pair rules for each meat and side
for (type in meat_side_types) {
  for (item in type) {
    print(item)
    rules <- apriori(meat_side_orders, parameter=list(supp=.001, conf=.001, minlen=2, maxlen=2), appearance=list(lhs=item), 
                     control=list(verbose=FALSE))
    inspect(sort(rules, by='lift'))
    cat('\n\n-------------------------------------------------------------------\n\n')
  }
}


# Provide counts of top meat wine pairings
for (meat in meats) {
  rules <- apriori(meat_wine_orders, parameter=list(supp=.001, conf=.001, minlen=2, maxlen=2),
                   appearance=list(rhs=meat), 
                   control=list(verbose=FALSE))
  inspect(sort(rules, by='support')[1:5])
}


# Provide counts of top meat side pairings
for (meat in meats) {
  rules <- apriori(meat_side_orders, parameter=list(supp=.001, conf=.001, minlen=2, maxlen=2),
                   appearance=list(rhs=meat), 
                   control=list(verbose=FALSE))
  inspect(sort(rules, by='support')[1:5])
}

# Provide count of top full orders
full_orders <- read.transactions('/Users/mwitebsky/Documents/NCSU/AA502/Data Mining/Data/restaurantData.csv',
                                  sep=',', format='single', cols=c('orderNumber', 'order'))

for (meat in meats) {
  rules <- apriori(full_orders, parameter=list(supp=.001, conf=.001, minlen=3, maxlen=3),
                   appearance=list(rhs=meat), 
                   control=list(verbose=FALSE))
  inspect(sort(rules, by='support')[1:10])
}


# list item frequency
FrequentItems <- eclat(full_orders, parameter=list(support=.001, maxlen=1))

