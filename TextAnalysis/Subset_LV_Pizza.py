import pandas as pd

reviews = pd.read_csv('../Assets/yelp_academic_dataset_review.csv')
biz = pd.read_csv('../Assets/yelp_academic_dataset_business.csv')

lv_pizza_biz = biz.loc[(biz['city']=="Las Vegas") & 
                       (biz['state']=='NV') &
                       (biz['categories'].str.lower().str.contains('pizza')), :]

lv_pizza_reviews = (lv_pizza_biz.loc[:, ['business_id', 'name', 'stars', 'state', 
                                         'city', 'neighborhood','address', 'postal_code']]
                                .rename(columns={'stars': 'biz_stars'})
                                .merge(reviews, how='inner', on='business_id'))

lv_pizza_reviews.to_csv('../Assets/lv_pizza_reviews.csv', index=False)