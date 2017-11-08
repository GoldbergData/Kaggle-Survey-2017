Kaggle Survey 2017
================
Josh Goldberg
11/7/2017

### Load Libraries

``` r
library(tidyverse)
library(gridExtra)
library(scales)
library(forcats)
```

### Read Data

``` r
MC_raw_data <- read_csv('multipleChoiceResponses.csv')
FF_raw_data <- read_csv('freeformResponses.csv')
schema <- read_csv('schema.csv')
conversion_rates <- read_csv('conversionRates.csv')
```

We'll start with a quick glimpse of the data.

``` r
dim(MC_raw_data)
```

    ## [1] 16716   228

This dataset has 16,716 rows (survey entries) and 228 columns.

Create clean data frame to manipulate.

``` r
clean_MC_data <- MC_raw_data
clean_FF_data <- FF_raw_data
```

``` r
ggplot(filter(clean_MC_data, !is.na(Country))) +
  geom_bar(aes(x = fct_infreq(Country), y = ..prop.., group = 1)) +
  labs(x = 'Country', y = 'Responses',
       title = 'Survey Responses By Country') +
  scale_y_continuous(labels = percent) +
  theme(plot.title = element_text(size = 14, face = "bold",
                                       color = "black", vjust = -1)) +
  theme(plot.subtitle = element_text(size = 8, face = "italic",
                                          color = "black")) +
  theme(axis.text.x = element_text(size = 10, angle = 50, hjust = 1)) +
  theme(axis.title.x = element_text(size = 10, vjust = -0.2)) +
  theme(axis.title.y = element_text(size = 10)) +
  theme(panel.background = element_rect(fill = "white"))
```

![](Figs/Country-1.png)

Is the field of data science diverse? Let's look at each country's proportion by gender.

``` r
ggplot(filter(clean_MC_data, !is.na(Country) & !is.na(GenderSelect))) +
  geom_bar(aes(x = fct_infreq(Country), fill = GenderSelect), position = 'fill') +
  labs(x = 'Country', y = 'Proportion',
       title = 'Survey Responses by Country and Gender') +
  scale_y_continuous(labels = percent) +
  scale_fill_discrete(guide = guide_legend(title = NULL, keywidth = .75, keyheight = .75), name = 'Gender', 
                      breaks = c('A different identity', 'Female', 'Male',
                      'Non-binary, genderqueer, or gender non-conforming'),
                      labels = c('Different Identity',
                                 'Female', 'Male', 'Non-conforming')) +
  theme(plot.title = element_text(size = 14, face = "bold",
                                       color = "black", vjust = -1)) +
  theme(plot.subtitle = element_text(size = 8, face = "italic",
                                          color = "black")) +
  theme(axis.text.x = element_text(size = 10, angle = 50, hjust = 1)) +
  theme(axis.title.x = element_text(size = 10, vjust = -0.2)) +
  theme(axis.title.y = element_text(size = 10)) +
  theme(legend.title = element_text(size = 10, face = "bold")) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = "top") +
    theme(panel.background = element_rect(fill = "white"))
```

![](Figs/Country%20and%20Gender-1.png)
