Kaggle Survey 2017
================
Josh Goldberg
11/7/2017

Load Libraries
--------------

Read Data
---------

We'll start with a quick glimpse of the data.

    ## [1] 16716   228

This dataset has 16,716 rows (survey entries) and 228 columns.

Create clean data frame to manipulate.

Response by Country
-------------------

*Excluded NA* ![](Figs/Country-1.png)

Response by Gender
------------------

*Excluded NA* ![](Figs/Gender-1.png)

Response by Country and Gender
------------------------------

*Excluded NA* ![](Figs/Country%20and%20Gender-1.png)

Age Distribution
----------------

*Ages between 5 and 90; Excluded NA* ![](Figs/Age-1.png)

Age Distribution by Gender
--------------------------

*Ages between 5 and 90; Excluded NA* ![](Figs/Age%20Facet-1.png)

    ## # A tibble: 4 x 3
    ##           GenderSelect count    percent
    ##                 <fctr> <int>      <dbl>
    ## 1                 Male 13610 81.8843632
    ## 2               Female  2778 16.7137958
    ## 3 A different identity   159  0.9566211
    ## 4       Non-conforming    74  0.4452199

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    0.00   25.00   30.00   32.37   37.00  100.00     331
