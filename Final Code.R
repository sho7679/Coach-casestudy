---
  title: "COach Case Study on CLV"
output: html_document
---
  
  ```{r}
## Setup 


###############################

library(lubridate)
library(tidyverse)
library("dplyr")
library(gmodels)
library(scales)
```


```{r}
#load data
data <- read_delim("brand_acq_tnsactions.csv.gz", delim = "\t")
```



```{r}
#add more specific dates
data <- data %>%
  mutate(
    date=ymd(dtKey),
    year=year(date),
    month=month(date),
    week=week(date),
    ymo=ymd(sprintf("%04d-%02d-01", year, month))
  )

```


```{r}
## Pseduo-churn modeling
# Create an indicator for when each customer entered dataset, and the order of the purchase


# adding the gross 
resamp <- data %>%
  group_by(
    custKey, week
  ) %>%
  summarize(
    gross = sum(gross),
    num_sku = n()
  ) %>%
  arrange(custKey, week)
```

```{r}
# if you want to get rid of the refund variable
resamp %>%
  mutate(
    refund = as.integer(gross <= 0)
  ) %>% 
  filter(refund == 0)

```

```{r}

#resamp <- resamp %>% 
#  group_by(custKey) %>%
#  arrange(week) %>% 
#  mutate(
#    purchase_seq = row_number(), #for each customer which number purchase this is (frequen)f
#    customer_entry = min(week), #when did the customer enter the dataset
#    lifetime_seq = (week - customer_entry) + 1
#  )


resamp <- resamp %>%
  group_by(custKey) %>%
  arrange(week) %>%
  mutate(
    purchase_seq = row_number(),
    customer_entry = min(week),
    lifetime_seq = (week - customer_entry) + 1
  )
```



```{r}
today <- max(data$week)

lifetime <- resamp %>%
  distinct(custKey, week, gross) %>%
  group_by(custKey) %>%
  arrange(week) %>%
  mutate(
    lweek=lag(week),
    diff=week-lweek
  ) %>%
  summarise(
    # Summary lifetime info.
    firstdate=min(week),
    lastdate=max(week),
    lifetime=as.integer(lastdate-firstdate)+1,
    currentperiod=as.integer(today-lastdate),
    npurchases=n(),
    
    # Interpurchase info.
    muperiod=mean(diff, na.rm=T),
    muperiod=as.integer(ifelse(is.nan(muperiod), 0, muperiod)),
    sdperiod=sd(diff, na.rm=T),
    sdperiod=ifelse(is.na(sdperiod), 116, sdperiod),
    
    # Monetary info.
    mupurchase = mean(gross, na.rm=T),
    histvalue = sum(gross, na.rm=T)
  )


# Interpurchase heterogeneity
expperiod <- lifetime %>%
  filter(npurchases>1) %>%
  group_by(npurchases) %>%
  summarize(
    mu=mean(muperiod),
    sd=sd(muperiod),
    lwr=mu-1.96*sd,
    lwr=ifelse(lwr<0, 0, lwr),
    upr=mu+1.96*sd
  ) %>%
  arrange(npurchases)

# Code the churns using actual data
lifetime <- lifetime %>%
  left_join(expperiod) %>%
  mutate(
    churned = as.integer(currentperiod > (muperiod+1.96*sdperiod))
  )

CrossTable(lifetime$churned)

```

```{r}
############################################################
# Examine some sample characteristics
############################################################

# Plot to see interpurchase heterogeneity
expperiod %>%
  ggplot(aes(x=npurchases, y=mu/4, ymin=lwr/4, ymax=upr/4)) +
  geom_errorbar(width=0) +
  geom_line() +
  scale_y_continuous(breaks=pretty_breaks()) +
  labs(
    x="Number of Months with Purchase",
    y="Mean Interpurchase Period (Months)"
  )
```
```{r}
# See number of customers by interpurchase period
lifetime %>%
  filter(npurchases>1) %>%
  ggplot(aes(x=muperiod/4)) +
  geom_histogram() +
  scale_y_continuous(labels = comma, breaks=pretty_breaks(10)) +
  labs(
    x="Interpurchase Period (Months)",
    y="Number of Customers"
  )
```
```{r}
# Lifetime distributions
lifetime %>%
  filter(npurchases>1) %>%
  ggplot(aes(x=lifetime/4)) +
  geom_density(fill="black") +
  scale_y_continuous(labels = percent) +
  labs(
    x="Observed Lifetime (Months)",
    y="Percent of Customers"
  )

```

```{r}
############################################################
# Link the churns back to the week-level dataset
############################################################

resamp <- resamp %>%
  left_join(
    lifetime %>%
      select(custKey, week=lastdate, churned)
  ) %>%
  mutate(
    churned = ifelse(is.na(churned), 0, churned)
  )

resamp
```

```{r}

############################################################
# Run churn prediction
############################################################

# still need to make sure we have RFM feats
# have F = purchase_seq
# have M = gross
# Need R
resamp <- resamp %>%
  group_by(custKey) %>%
  arrange(week) %>%
  mutate(
    
    # recency
    timelag = week - lag(week),
    timelag = ifelse(is.na(timelag), 0, timelag)
    
  )

```

```{r}
# Set up our train test split
train.rat <- 0.70
N <- nrow(resamp)
train <- sample(1:N, size = ceiling(train.rat*N))
test <- (1:N)[-train]

df.train <- resamp[train,]
df.test <- resamp[test,]

```

```{r}
# very basic model
glm.churn <- glm(churned ~ timelag + lifetime_seq + gross + week + num_sku, 
                 data = df.train, family = binomial(link="logit"))
summary(glm.churn)
```


```{r}
# Make some churn predictions on the test set
df.test <- df.test %>%
  ungroup() %>%
  mutate(
    yhat = predict(glm.churn, df.test, type = "response")
  )
hist(df.test$yhat)
```

```{r}
# Threshold
df.test <- df.test %>%
  mutate(
    churn_pred = as.integer(yhat > 0.05)
  )
```

```{r}
# Check the confusion matrix
with(df.test, CrossTable(churned, churn_pred))
cm <- with(df.test, table(churned, churn_pred))
acc <- (cm[1,1] + cm[2,2]) / sum(cm)
pre <- (cm[2,2]) / (cm[2,1] + cm[2,2])
rec <- (cm[1,1]) / (cm[1,1] + cm[1,2])
f1 <- 2 * (pre * rec) / (pre + rec)

# How should I interpret these?
acc
f1
```

```{r}
############################################################
# Estimate customer lifetime values
############################################################

# Make some churn predictions
resamp <- resamp %>%
  ungroup() %>%
  mutate(
    yhat = predict(glm.churn, resamp, type = "response")
  )
hist(resamp$yhat)

```

```{r}
# Threshold
# Probability of alive = projection factor
resamp <- resamp %>%
  mutate(
    churn_pred = as.integer(yhat > 0.05),
    alive_pr = 1 - yhat,
    alive_pr_rescale = rescale(alive_pr, c(0, 1))
  )
hist(resamp$alive_pr)
```

```{r}
# I only need the final probs
final_probs <- resamp %>%
  group_by(custKey) %>%
  arrange(week) %>%
  filter(row_number() == n())
hist(final_probs$alive_pr)
hist(final_probs$alive_pr_rescale)

final_probs
```

```{r}
# map and then aggregate
lifetime <- lifetime %>%
  left_join(
    final_probs %>%
      select(-gross)
  )
```

```{r}
# Project over estimated remaining lifetime per customer
clv <- lifetime %>%
  select(mupurchase, histvalue, lifetime, alive_pr_rescale, custKey) %>%
  ungroup() %>%
  mutate(
    est_lifetime = ceiling(mean(lifetime, na.rm=T)),
    remain_life = ifelse(
      lifetime <= est_lifetime, 
      est_lifetime - lifetime,
      lifetime + 12
    )
  )

# Now, do the geometric summations!
clv
```

```{r}

pr_discount <- 0.05

#this is not a vectorized formula
calc_future_clv <- function(money, remaining_life, pr_churn, pr_discount){
  
  # No money accumulated yet...
  base_clv <- 0 
  
  #for each future time period, calculate the marginal addition to CLV
  for (t in 1: remaining_life){
    discount_factor <- (( 1+ pr_discount)^t * (1 + pr_churn)^t)
    
    period_value <- money / discount_factor
    
    base_clv <- base_clv + period_value
  }
  base_clv
}

calc_future_clv(100,12,1-.888, pr_discount)


```

```{r}

# iterate through the customer base

clv_estimates <- data.frame()

for(i in 1: nrow(clv)){
  
  cust_i <- clv[i,]
  
  m <- cust_i$mupurchase
  rl <- cust_i$remain_life
  prc <- 1 - cust_i$alive_pr_rescale
  
  clv_hat_i <- calc_future_clv(m,rl,prc,pr_discount)
  
  clv_estimates <- rbind(
    clv_estimates, 
    data.frame(
      i=i, 
      clv_hat = clv_hat_i
    )
  )
}
```


```{r}
clv_estimates <- clv_estimates %>% as_tibble()
clv_estimates
```



```{r}
clv_estimates <- clv_estimates %>% 
  as_tibble()
```



```{r}
summary(clv_estimates)
```

```{r}
clv <- clv %>%
  mutate(clv_hat = unlist(clv_estimates$clv_hat))
```

```{r}
clv
```

```{r}
clv %>%
  filter()
```


```{r}
clv %>%
  ggplot(aes(x=clv_hat))+
  geom_histogram(color='darkblue', fill='lightblue')
```


```{r}
clv %>%
  ggplot(aes(x=clv_hat))+
  geom_histogram(color='darkblue', fill='lightblue')+
  xlim(c(0,2500))
```

