options(warn = 2)
rm(list = ls())

library(forcats)
library(dplyr)
library(reshape2)
library(stringr)
library(ggplot2)
library(data.table)
library(lubridate)
library(zoo)
library(rvest)
library(stats)
library(tidyverse)
library(GGally)

# parameters ##################################################################

prop_config <- c(1, 1)    # target 1 bed 1 bath apartments
props_to_pull <- 25       # pages of apartments to loop through


# scrape ######################################################################

# fill in suburb-specific url from https://www.auhouseprices.com/rent
urls <- paste0('https://www.auhouseprices.com/rent/list/NSW/2088/Mosman/',
               1:props_to_pull, '/?sort=date&type=apartment&bmin=',
               prop_config[1], '&bmax=', prop_config[1])

# loop through URLs
for (i in 1:length(urls)) {

  if(i == 1 & exists('rent_all')) rm(rent_all)

  curr_url <- urls[[i]]
  print(paste0('getting ', i))
  temp <- read_html(curr_url)

  # sleep between requests for 2 seconds so as not to bombard the server
  print('sleeping')
  Sys.sleep(2)

  address <- temp %>%
    html_nodes('h4') %>%
    html_text() %>%
    .[which(. != ' Search Filter and Sorting ')]

  price_month <- temp %>%
    html_nodes('li') %>%
    html_text() %>%
    str_extract('^Rent.+/week.*\\d{4}$') %>%
    .[which(!is.na(.))]

  config <- temp %>%
    html_nodes('li') %>%
    html_text() %>%
    str_extract(' \\d \\d \\d*[ ]*$') %>%
    .[which(!is.na(.))]

  combined <- data.table(address, price_month, config)

  # append results of this iteration to our master data set
  if(!exists('rent_all')) {
    rent_all <- combined
  } else {
    rent_all <- rbind(rent_all, combined)
  }
}

# extract month
rent_all$month <- str_extract(rent_all$price_month, '[A-Z][a-z]{2} \\d{4}$')
rent_all$month <- dmy(paste0('01 ', rent_all$month))

# extract price
rent_all$price <- str_extract(rent_all$price_month, '(?<=Rent \\$).*(?=/week)')
rent_all$price <- as.numeric(rent_all$price)

# remove any dups
rent_all <- rent_all[!duplicated(rent_all)]

# subset to view only those matching property configuration specified above
pattern <- paste0(prop_config[[1]], '\\s', prop_config[[2]])

# create our analytical dataset
ads <- rent_all[grepl(pattern, rent_all$config), ]

# save it to csv file to share with agent
write.table(ads, file = 'rental_units_rushcuttersBay_2020_2022.csv', quote = FALSE, sep=',')

# analyse ####################################################################

# pre-smoothing plot the distribution
ads %>%
  ggplot(aes(x = reorder(factor(format(month, '%b %Y')), as.numeric(interaction(month(month), year(month)))), y = price)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2) +
  coord_flip() +
  theme_bw()+
  theme(panel.grid = element_blank())+
  labs(x = 'Month rented', y = 'Weekly rent',
       title = 'Distribution of weekly rent in Mosman',
       subtitle = 'August 2020 - November 2022')

# what is the suburb median?
median(ads$price)

# smooth data using rolling quarterly median
monthly_medians <- ads %>%
  group_by(month) %>%
  summarise(median_price = median(price))

rol_median <- rollmedian(monthly_medians$median_price, 3, na.pad = TRUE,
                         align = 'right')

names(rol_median) <- monthly_medians$month

rol_median <- data.table(month = as.Date(names(rol_median)),
                         rol_median = rol_median)
rol_median <- rol_median[!is.na(rol_median), ]

rol_median %>%
  ggplot(aes(x = month, y = rol_median)) +
  geom_bar(stat = 'identity', fill="#8A1A6C") +
  coord_cartesian(ylim = c(400, 600)) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid = element_blank()) +
  scale_x_date(date_labels = "%b %Y", date_breaks = '1 month') +
  labs(x = 'Month rented', y = 'Smoothed weekly rent',
       title = 'Weekly rental prices in Rushcutters Bay',
       subtitle = 'Smoothed by rolling quarterly median')

# take a closer look at an address of interest. I used it to look at my apartment building
building <- ads %>%
  filter(grepl("", address, ignore.case = TRUE) # fill in the grepl

# extract the year from the date column
building$date <- as.Date(building$month, format = "%Y-%m-%d")
building$year <- year(building$date)

# Extract the unit and unit number from the address column
building$unit <- str_extract(building$address, "\\d+/\\d+")
building$unit_number <- str_extract(building$unit, "\\d+")

# what is the median unit price pw at my address
addressMedian <- median(building$price)

# Group the data by unit and select the top row (i.e., the row with the maximum price) for each group
max_price_by_unit <- building %>%
  group_by(unit) %>%
  top_n(1, price)

ggplot(data = max_price_by_unit, aes(x = unit_number, y = price)) +
  geom_col(fill="#8A1A6C") +
  labs(x = "Unit", y = "Price") +
  geom_hline(yintercept = median, color = "black", linetype = "dashed") +
  theme_bw()+
  theme(panel.grid = element_blank()) +
  labs(x = 'Unit', y = 'Price ($ per week)',
       title = 'Weekly rental prices at') # fill in address


