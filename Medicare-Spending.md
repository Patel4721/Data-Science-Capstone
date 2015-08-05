U.S. Medicare Spending
========================================================
author: Shailesh Patel
date:   April 26, 2015

CMS Medicare Spending
========================================================

This application provides data from the Centers for Medicare & Medicaid Services from 2012. 

- The main purpose is to enable the user to view Medicaire spending by state and by provider type
- Data can be viewed on a map
- Data can be viewed in tabular format
- Tabular data may be downloaded

About the Data
========================================================

- The primary data source for these data is CMS’s CY2012 National Claims History (NCH) Standard Analytic Files (SAFs) which include claims as of 6/30/2013. The NCH SAFs contain 100 percent of Medicare final action claims for beneficiaries who are enrolled in the FFS program 
- Study Population: Providers that had a valid NPI and submitted Medicare Part B noninstitutional claims (excluding DME) during the 2012 calendar year.
- Years: Calendar Year 2012
- Special Note: This aggregate report was created using the Medicare Physician and Other Supplier Public Use File (PUF).  In the Medicare Physician and Other Supplier PUF any aggregated records which are derived from 10 or fewer beneficiaries are excluded to protect the privacy of Medicare beneficiaries.  As a result, the data in this aggregate report also reflect these redactions.  

Data Definition
========================================================

- npi - National Provider Identifier (NPI) for the performing provider on the claim.
nppes_provider_state - The state where the provider is located, as reported in NPPES. The fifty U.S. states and the District of Columbia are reported by the state postal abbreviation.  
- provider_type - Derived from the provider specialty code reported on the claim.  For providers that reported more than one specialty code on their claims, this is the specialty code associated with the largest number of services.
- medicare_patients - Total Medicare beneficiaries receiving the provider services.
- provider_services - Total number of services performed.
- total_medicare_payment_amt - Amount that Medicare paid after deductible and coinsurance amounts have been deducted for all the provider's line item service.

Medicare Spending by State Map
========================================================

```r
library(shiny)

# Plotting 
library(ggplot2)
library(rCharts)
library(ggvis)

# Data processing libraries
library(data.table)
library(reshape2)
library(dplyr)

# Load data
states_map <- map_data("state")
dt <- fread('cms-2012-data.csv') %>% mutate(provider_type = tolower(provider_type))

aggregate_by_state <- function(dt) {
  replace_na <- function(x) ifelse(is.na(x), 0, x)
  round_2 <- function(x) round(x, 2)
  
  states <- data.table(nppes_provider_state=sort(unique(dt$nppes_provider_state)))
  
  aggregated <- dt %>% 
    group_by(nppes_provider_state) %>%
    summarise_each(funs(sum), total_medicare_payment_amt)
  
  # We want all states to be present even if nothing happened
    left_join(states,  aggregated, by = "nppes_provider_state") %>%
    mutate_each(funs(replace_na), total_medicare_payment_amt) %>%
    mutate_each(funs(round_2), total_medicare_payment_amt)    
}

dt <- aggregate_by_state(dt)
dt <- dt %>% mutate(Spending = total_medicare_payment_amt)

p <- ggplot(dt, aes(map_id = nppes_provider_state))
p <- p + geom_map(aes_string(fill="Spending"), map = states_map, colour='black')
p <- p + expand_limits(x = states_map$long, y = states_map$lat)
p <- p + coord_map() + theme_bw()
p <- p + labs(x = "Long", y = "Lat", title = "Medicare Spending by State")
p + scale_fill_gradient(low = "#fff5eb", high = "#d94801")
```

![plot of chunk unnamed-chunk-1](Medicare-Spending-figure/unnamed-chunk-1-1.png) 


