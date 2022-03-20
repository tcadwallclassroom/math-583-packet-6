# library(openintro)
# # glimpse(gss2010)
# gss2010_grass <- gss2010 %>% drop_na(grass)
# grass_table <- table(gss2010_grass$grass)
# 
# chisq.test(grass_table, correct = F)


# install.packages("infer")
library(infer)

data(gss)
table(gss$college)
