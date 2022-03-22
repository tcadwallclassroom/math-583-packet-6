# library(openintro)
# # glimpse(gss2010)
# gss2010_grass <- gss2010 %>% drop_na(grass)
# grass_table <- table(gss2010_grass$grass)
# 
# chisq.test(grass_table, correct = F)


# install.packages("infer")
# library(infer)
# 
# data(gss)
# table(gss$college)

#[Overview of the `gssr` package](https://kjhealy.github.io/gssr/articles/overview.html)

library(tidyverse)
#install.packages("drat")
library(drat)

drat::addRepo("kjhealy")
#install.packages("gssr")
library(gssr)

data(gss_doc)
data(gss_all)
gss_which_years(gss_all, grass)

num_vars <- c("hrsrelax", "mntlhlth", "hrs1")
cat_vars <- c("degree", "grass")
my_vars <- c(num_vars, cat_vars)

gss18 <- gss_get_yr(2018)
gss18 <- gss18 %>% 
  select(my_vars) %>% 
  mutate(
    # Convert all missing to NA
    across(everything(), haven::zap_missing),
    # Make all categorical variables factors and relabel nicely
    across(all_of(cat_vars), forcats::as_factor))
  

gss10 <- gss_get_yr(2010)
gss10 <- gss10 %>% 
  select(my_vars) %>% 
  mutate(
    # Convert all missing to NA
    across(everything(), haven::zap_missing),
    # Make all categorical variables factors and relabel nicely
    across(all_of(cat_vars), forcats::as_factor))

table(gss18$grass) %>% prop.table()
table(gss10$grass)
