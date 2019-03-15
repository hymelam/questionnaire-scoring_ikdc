IKDC Questionnaire Scoring
================

This document provides an example of the IKDC questionnaire scoring process. The IKDC is a patient-reported measure of knee-specific symptoms, function, and sports activity. Scores range from 0 to 100, with higher scores indicating higher levels of function.

``` r
data <- read.csv("ikdc_example_sml.csv", stringsAsFactors = FALSE)
```

``` r
str(data)
```

    ## 'data.frame':    199 obs. of  19 variables:
    ##  $ ID    : int  100 101 102 103 104 105 106 107 108 109 ...
    ##  $ PERF  : chr  "moderate activities like moderate physical work, running or jogging" "strenuous activities like heavy physical work, skiing or tennis" "very strenuous activities like jumping or pivoting as in basketball or soccer" "very strenuous activities like jumping or pivoting as in basketball or soccer" ...
    ##  $ PAIN  : chr  "three" "two" "zero" "one" ...
    ##  $ SEV   : chr  "five" "three" "zero" "one" ...
    ##  $ STIFF : chr  "mildly" "mildly" "not at all" "mildly" ...
    ##  $ SWELL : chr  "strenuous activities like heavy physical work, skiing or tennis" "strenuous activities like heavy physical work, skiing or tennis" "very strenuous activities like jumping or pivoting as in basketball or soccer" "moderate activities like moderate physical work, running or jogging" ...
    ##  $ LOCK  : chr  "no" "no" "no" "yes" ...
    ##  $ GIVE  : chr  "strenuous activities like heavy physical work, skiing or tennis" "strenuous activities like heavy physical work, skiing or tennis" "very strenuous activities like jumping or pivoting as in basketball or soccer" "very strenuous activities like jumping or pivoting as in basketball or soccer" ...
    ##  $ PARTIC: chr  "moderate activities like moderate physical work, running or jogging" "strenuous activities like heavy physical work, skiing or tennis" "very strenuous activities like jumping or pivoting as in basketball or soccer" "strenuous activities like heavy physical work, skiing or tennis" ...
    ##  $ UPSTRS: chr  "not difficult at all" "not difficult at all" "not difficult at all" "not difficult at all" ...
    ##  $ DNSTRS: chr  "not difficult at all" "minimally difficult" "not difficult at all" "minimally difficult" ...
    ##  $ KNEEL : chr  "minimally difficult" "moderately difficult" "moderately difficult" "not difficult at all" ...
    ##  $ SQUAT : chr  "minimally difficult" "not difficult at all" "not difficult at all" "minimally difficult" ...
    ##  $ SIT   : chr  "minimally difficult" "not difficult at all" "not difficult at all" "minimally difficult" ...
    ##  $ RISE  : chr  "minimally difficult" "not difficult at all" "not difficult at all" "not difficult at all" ...
    ##  $ RUN   : chr  "minimally difficult" "not difficult at all" "not difficult at all" "not difficult at all" ...
    ##  $ JUMP  : chr  "minimally difficult" "minimally difficult" "not difficult at all" "minimally difficult" ...
    ##  $ STOP  : chr  "minimally difficult" "minimally difficult" "not difficult at all" "moderately difficult" ...
    ##  $ CRFUNC: chr  "six" "eight" "ten" "ten" ...

In this example, the responses are stored as text strings. The responses need to be converted from text to item-level scores. Some items have equivalent scoring - we'll create functions to score those items.

``` r
# Recode item response: "unable to do" = 0 / "not difficult at all" = 4 
ik_unable_0 <- function(var) {
  res <- rep(999, length(var))
  res[is.na(var)] <- NA
  res[var == "unable to do"] <- 0
  res[var == "extremely difficult"] <- 1
  res[var == "moderately difficult"] <- 2
  res[var == "minimally difficult"] <- 3
  res[var == "not difficult at all"] <- 4
  return(res)
}

# Recode item response: "zero" = 10 / "ten" = 0
ik_numeric_rev <- function(var) {
  res <- rep(999, length(var))
  res[is.na(var)] <- NA
  res[var == "ten"] <- 0
  res[var == "nine"] <- 1
  res[var == "eight"] <- 2
  res[var == "seven"] <- 3
  res[var == "six"] <- 4
  res[var == "five"] <- 5
  res[var == "four"] <- 6
  res[var == "three"] <- 7
  res[var == "two"] <- 8
  res[var == "one"] <- 9
  res[var == "zero"] <- 10
  return(res)
} 
```

The following scoring functions are only used once (and thus aren't necessary to create - but it's nice to have all the recoding information in one place).

``` r
ik_crfunc <- function(var) {
  res <- rep(999, length(var))
  res[is.na(var)] <- NA
  res[var == "ten"] <- 10
  res[var == "nine"] <- 9
  res[var == "eight"] <- 8
  res[var == "seven"] <- 7
  res[var == "six"] <- 6
  res[var == "five"] <- 5
  res[var == "four"] <- 4
  res[var == "three"] <- 3
  res[var == "two"] <- 2
  res[var == "one"] <- 1
  res[var == "zero"] <- 0
  return(res)
} 

ik_perf <- function(var){
  res <- rep(999, length(var))
  res[is.na(var)] <- NA
  res[var == "very strenuous activities like jumping or pivoting as in basketball or soccer"] <- 4
  res[var == "strenuous activities like heavy physical work, skiing or tennis"] <- 3
  res[var == "moderate activities like moderate physical work, running or jogging"] <- 2
  res[var == "light activities like walking, housework, or yard work"] <- 1
  res[var == "unable to perform any of the above activities due to knee pain"] <- 0
  return(res)
}

ik_stiff <- function(var){
  res <- rep(999, length(var))
  res[is.na(var)] <- NA
  res[var == "not at all"] <- 4
  res[var == "mildly"] <- 3
  res[var == "moderately"] <- 2
  res[var == "very"] <- 1
  res[var == "extremely"] <- 0
  return(res)
}

ik_swell <- function(var){
  res <- rep(999, length(var))
  res[is.na(var)] <- NA
  res[var == "very strenuous activities like jumping or pivoting as in basketball or soccer"] <- 4
  res[var == "strenuous activities like heavy physical work, skiing or tennis"] <- 3
  res[var == "moderate activities like moderate physical work, running or jogging"] <- 2
  res[var == "light activities like walking, housework, or yard work"] <- 1
  res[var == "unable to perform any of the above activities due to knee swelling"] <- 0
  return(res)
}

ik_lock <- function(var){
  res <- rep(999, length(var))
  res[is.na(var)] <- NA
  res[var == "no"] <- 1
  res[var == "yes"] <- 0
  return(res)
}

ik_give <- function(var){
  res <- rep(999, length(var))
  res[is.na(var)] <- NA
  res[var == "very strenuous activities like jumping or pivoting as in basketball or soccer"] <- 4
  res[var == "strenuous activities like heavy physical work, skiing or tennis"] <- 3
  res[var == "moderate activities like moderate physical work, running or jogging"] <- 2
  res[var == "light activities like walking, housework, or yard work"] <- 1
  res[var == "unable to perform any of the above activities due to giving way of the knee"] <- 0
  return(res)
}

ik_partic <- function(var){
  res <- rep(999, length(var))
  res[is.na(var)] <- NA
  res[var == "very strenuous activities like jumping or pivoting as in basketball or soccer"] <- 4
  res[var == "strenuous activities like heavy physical work, skiing or tennis"] <- 3
  res[var == "moderate activities like moderate physical work, running or jogging"] <- 2
  res[var == "light activities like walking, housework, or yard work"] <- 1
  res[var == "unable to perform any of the above activities due to knee"] <- 0
  return(res)
}
```

We can now recode the text responses into item-level scores.

``` r
scores <- within(data, {
  # Variables that can be recoded with the ik_numeric_rev function
  PAIN <- ik_numeric_rev(PAIN)
  SEV <- ik_numeric_rev(SEV)
  # Variables that can be recoded with the ik_unable_0 function
  UPSTRS <- ik_unable_0(UPSTRS)
  DNSTRS<- ik_unable_0(DNSTRS)
  KNEEL<- ik_unable_0(KNEEL)
  SQUAT<- ik_unable_0(SQUAT)
  SIT<- ik_unable_0(SIT)
  RISE<- ik_unable_0(RISE)
  RUN<- ik_unable_0(RUN)
  JUMP<- ik_unable_0(JUMP)
  STOP<- ik_unable_0(STOP)
  # Variables with their own unique coding schemes
  CRFUNC <- ik_crfunc(CRFUNC)
  PERF<-ik_perf(PERF)
  STIFF<-ik_stiff(STIFF)
  SWELL<-ik_swell(SWELL)
  LOCK<-ik_lock(LOCK)
  GIVE<-ik_give(GIVE)
  PARTIC<-ik_partic(PARTIC)
})
```

It can be useful to store information about a dataset in an external list. This meta-data can then be used programmatically.

`dput(names(data_frame))` is a useful command for getting lists of variable names when you don't want to use column positions (e.g., `data_frame[,1]`). This will print all variable names into the console - including quotation marks and commas. This text can then easily be copied and pasted into the R script.

``` r
dput(names(data))
```

    ## c("ID", "PERF", "PAIN", "SEV", "STIFF", "SWELL", "LOCK", "GIVE", 
    ## "PARTIC", "UPSTRS", "DNSTRS", "KNEEL", "SQUAT", "SIT", "RISE", 
    ## "RUN", "JUMP", "STOP", "CRFUNC")

To score the IKDC, we will eventually need to perform different calculations based on the maximum value of each item. The variable names for items with a maximum value of 1 are stored in `var$max_1`, items with a maximum value of 4 are stored in `var$max_4`, and items with a maximum value of 10 are stored in `var$max_10`. `var$varlist` is a vector of all questionnaire item variable names. We can add infromation to this list as needed.

``` r
var <- list()
var$max_1 <- c("LOCK")
var$max_4 <- c("PERF","STIFF","SWELL","GIVE","PARTIC","UPSTRS",
                 "DNSTRS","KNEEL","SQUAT","SIT","RISE","RUN","JUMP","STOP")
var$max_10 <- c("PAIN","SEV", "CRFUNC")
var$varlist <- c(var$max_1, var$max_4, var$max_10)
```

The IKDC can only be scored if participants repond to 16 items or more. We'll record a count of the number of non-missing responses to be used later. Only columns that appear in the variable list will be included (i.e., the `ID` field is ignored).

``` r
scores$response_count <- rowSums( !is.na( scores[ colnames(scores) %in% var$varlist ]))
```

The IKDC score is calculated by: \[sum of the item level scores\] / \[sum of the maximum scores possible on the items to which the participants responded\] \* 100.

IKDC items have maximum scores of either 1, 4, or 10. In preparation of calculating the denominator value, we'll create a version of each IKDC item that contains either: NA (if the participant did not respond) or the maximum possible value (if the participant did respond). These variables will have the same names as the item-level score variables, but with a "\_denom" suffix.

``` r
# Add a denominator suffix ("_denom") to all questionnaire item names
var$denom_1 <- paste0(var$max_1, "_denom")
var$denom_4 <- paste0(var$max_4, "_denom")
var$denom_10 <- paste0(var$max_10, "_denom")
var$denom <- paste0(var$varlist, "_denom")

# Recode based on maximum value
scores[var$denom_10] <- 10
scores[var$denom_4] <- 4
scores[var$denom_1] <- 1
scores[var$denom][is.na(scores[var$varlist])] <- NA

# This works because we know the order of variables in var$denom and var$varlist are the same.
# i.e., first LOCK / LOCK_denom, then PERF / PERF_denom, etc.
```

Now we can calculate the IKDC score. The numerator is created by summing the item-level scores. The denominator is created by summing the maximum scores for non-missing items.

``` r
scores$numerator   <- rowSums(scores[,var$varlist], na.rm=TRUE)
scores$denominator <- rowSums(scores[,var$denom], na.rm=TRUE)
scores$ikdc_score  <- ifelse(scores$response_count > 15, (scores$numerator/scores$denominator)*100, NA)
```

The scores can be merged back into the original dataset.

``` r
data <- merge(data, scores[c("ID", "ikdc_score")], by = "ID", all=TRUE)
```

IKDC scores range from 0 to 100. Let's look at a summary:

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.4.4

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
data %>% summarize(
  min = min(ikdc_score, na.rm=TRUE),
  mean = mean(ikdc_score, na.rm=TRUE),
  max = max(ikdc_score, na.rm=TRUE),
  missing = sum(is.na(ikdc_score))
  )
```

    ##        min     mean max missing
    ## 1 22.98851 79.11275 100       1

We can also easily see the score's distribution.

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 3.4.4

``` r
theme_set(theme_light())

data %>% 
  filter(!is.na(ikdc_score)) %>% 
  ggplot(aes(ikdc_score))+
  geom_histogram(bins=40) +
  expand_limits(x = 0, y = 0) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  geom_vline(aes(xintercept=mean(ikdc_score)), 
             color="red", linetype="dashed", size=1)
```

![](base-r-version_files/figure-markdown_github/histogram-1.png)
