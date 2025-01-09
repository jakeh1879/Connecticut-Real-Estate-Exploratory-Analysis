library('tidyverse')


tidyverse_logo()

# Load dataset
real_estate <- read.csv('Real_Estate_Adjusted.csv')

glimpse(real_estate)
View(real_estate)


# Remove use codes and remarks because the majority of records have NA for these fields
real_estate <- real_estate %>% 
  select(c(-Non.Use.Code,-Assessor.Remarks,-OPM.remarks))

View(real_estate)



# Rename column values
names(real_estate)

real_estate <- real_estate %>% 
  rename(
    'serial_num' = Serial.Number,
    'list_year' = List.Year,
    'date_recorded' = Date.Recorded,
    'city' = Town,
    'address' = Address,
    'assessed_value' = Assessed.Value,
    'sold_for' = Sale.Amount,
    'sale_ratio' = Sales.Ratio,
    'property_type' = Property.Type,
    'residential_type' = Residential.Type,
    'location' = Location
  )

View(real_estate)

na_counts <- numeric()

# Check for NAs or blanks

for (i in seq_along(real_estate)) {
  na_counts[i] <- sum(is.na(real_estate[i]) | real_estate[i] == "")

  
}

na_counts

# Put some placeholder value in residential_type column blank values

real_estate <- real_estate %>% 
  mutate(residential_type = if_else(residential_type == "",'Other',residential_type))

View(real_estate)

# Update location column to put values in separate latitude and longitude variables

real_estate <- real_estate %>% 
  mutate(location =
           str_remove(location, "POINT \\(") %>% 
           str_remove("\\)")) %>% 
  separate(location, into = c('lattitude','longitude'), sep = " ")

view(real_estate)

# Received warning: Expected 2 pieces. Missing pieces filled with `NA` in 2 rows [8268, 29182].
# Remove these two records with NAs

real_estate[8268,]
real_estate[29182,]

real_estate <- real_estate %>% 
  slice(c(-8268,-29182))

real_estate[8268,]
real_estate[29182,]

View(real_estate)

# This dataset had some extreme outliers which made the numeric exploratory analysis very difficult.
# I decided to filter for home sold for under $1.5 million so that I could make visual observations

filtered_data <- real_estate %>%
  filter(sold_for < 1500000 & assessed_value < 2000000)

count(filtered_data)

real_estate <- filtered_data

write.csv(real_estate, "final_real_estate.csv", row.names = TRUE)

#### EDA 

# Overall characteristics
str(real_estate)
glimpse(real_estate)

summary(real_estate)

## Univariate Analysis of each variable

# list_year
unique(real_estate$list_year)
table(real_estate$list_year)

prop.table((table(real_estate$list_year)))
round(100*prop.table((table(real_estate$list_year))), digits=0)

ggplot(data=real_estate, aes(x=list_year)) +
  geom_bar()

# date_recorded
unique(real_estate$date_recorded)
table(real_estate$date_recorded)

real_estate %>% 
  group_by(date_recorded) %>% 
  summarize(counts=n()/nrow(real_estate))


# city
unique(real_estate$city)
table(real_estate$city)

# Top 10 cities with most houses sold
top_cities <- count(real_estate, city) %>% top_n(n, n=10)
top_cities

top_cities <- top_cities %>%
  slice_max(n, n = 5)

ggplot(top_cities, aes(x = reorder(city, n), y = n, fill = "blue")) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs(title = "Top Cities",
       x = "City",
       y = "Count") +
  scale_fill_manual(values = "blue") +
  theme_minimal()

# address

unique(real_estate$city)
table(real_estate$city)

# property_type
unique(real_estate$property_type)
table(real_estate$property_type)

real_estate %>% 
  group_by(property_type) %>% 
  summarize(counts=n()/nrow(real_estate))

round(100*prop.table((table(real_estate$property_type))), digits=1)

ggplot(data=real_estate, aes(x=factor(property_type))) +
  geom_bar()

# residential_type

unique(real_estate$residential_type)
table(real_estate$residential_type)

real_estate %>% 
  group_by(residential_type) %>% 
  summarize(counts=n()/nrow(real_estate))

round(100*prop.table((table(real_estate$residential_type))), digits=1)

ggplot(data=real_estate, aes(x=factor(residential_type), fill=residential_type)) +
  geom_bar()

## Numeric Variables

# assessed_value

summary(real_estate$assessed_value)
sd(real_estate$assessed_value)
IQR(real_estate$assessed_value)
max(real_estate$assessed_value)
min(real_estate$assessed_value)

plot(real_estate$assessed_value)
hist(real_estate$assessed_value)

qplot(x=assessed_value, data=real_estate)

d <- density(real_estate$assessed_value)
plot(d)

boxplot(real_estate$assessed_value)

qqnorm(real_estate$assessed_value)
qqline(real_estate$assessed_value, col="red", lwd=2)

# sold_for

summary(real_estate$sold_for)
sd(real_estate$sold_for)
IQR(real_estate$sold_for)
max(real_estate$sold_for)
min(real_estate$sold_for)

plot(real_estate$sold_for)
hist(real_estate$sold_for)

qplot(x=sold_for, data=real_estate)

d <- density(real_estate$sold_for)
plot(d)

boxplot(real_estate$sold_for)


qqnorm(real_estate$sold_for)
qqline(real_estate$sold_for, col="red", lwd=2)

# sale_ratio

summary(real_estate$sale_ratio)
sd(real_estate$sale_ratio)
IQR(real_estate$sale_ratio)
max(real_estate$sale_ratio)
min(real_estate$sale_ratio)


boxplot(real_estate$sale_ratio)

qqnorm(real_estate$sale_ratio)
qqline(real_estate$sale_ratio, col="red", lwd=2)

## Bivariate Analysis

# Numeric x Numeric

cor(real_estate$assessed_value, real_estate$sold_for)

ggplot(real_estate, aes(x=assessed_value, y=sold_for)) + geom_point()

# Numeric x Categorical

# property_type

# Box plots
ggplot(real_estate, aes(x=property_type, y=sold_for)) + geom_boxplot()
ggplot(real_estate, aes(x=property_type, y=assessed_value)) + geom_boxplot()

# Density plots
ggplot(real_estate, aes(x=sold_for, color=property_type)) + geom_density()
ggplot(real_estate, aes(x=assessed_value, color=property_type)) + geom_density()

# Histograms 
ggplot(real_estate, aes(x=sold_for)) + geom_histogram() + facet_wrap(~property_type)
ggplot(real_estate, aes(x=assessed_value)) + geom_histogram() + facet_wrap(~property_type)

# Stats
real_estate %>% 
  group_by(property_type) %>% 
  summarize(mean=mean(sold_for),
            sd=sd(sold_for),
            Q1=quantile(sold_for,0.25),
            Q3=quantile(sold_for,0.75))

# residential_type

# Box plots
ggplot(real_estate, aes(x=residential_type, y=sold_for)) + geom_boxplot()
ggplot(real_estate, aes(x=residential_type, y=assessed_value)) + geom_boxplot()

# Density plots
ggplot(real_estate, aes(x=sold_for, color=residential_type)) + geom_density()
ggplot(real_estate, aes(x=assessed_value, color=residential_type)) + geom_density()

# Histograms 
ggplot(real_estate, aes(x=sold_for)) + geom_histogram() + facet_wrap(~residential_type)
ggplot(real_estate, aes(x=assessed_value)) + geom_histogram() + facet_wrap(~residential_type)

# Stats
real_estate %>% 
  group_by(residential_type) %>% 
  summarize(mean=mean(sold_for),
            sd=sd(sold_for),
            Q1=quantile(sold_for,0.25),
            Q3=quantile(sold_for,0.75))


# city

# Stats
real_estate %>% 
  group_by(city) %>% 
  summarize(mean=mean(sold_for),
            sd=sd(sold_for),
            Q1=quantile(sold_for,0.25),
            Q3=quantile(sold_for,0.75))

# Stats
real_estate %>% 
  group_by(city) %>% 
  summarize(mean=mean(assessed_value),
            sd=sd(assessed_value),
            Q1=quantile(assessed_value,0.25),
            Q3=quantile(assessed_value,0.75))




