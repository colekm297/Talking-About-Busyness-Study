library(tidyverse)
library(haven)
library(psych)
library(nFactors)
library(GPArotation)
library(dplyr)
library(Hmisc)
library(xtable)
library(corrplot)
library(htmlTable)
library(magrittr)
library(reshape)

### ---
### Create Composite of coworker and supervisor ratings
### ---

EC <- EC %>%
  mutate(TAB_CS = ((TAB_C + TAB_S) / 2), na.rm = TRUE) %>%
  mutate(TAB_CS_1 = ((TAB_C_1 + TAB_S_1) / 2), na.rm = TRUE) %>%
  mutate(TAB_CS_2 = ((TAB_C_2 + TAB_S_2) / 2), na.rm = TRUE) %>%
  mutate(TAB_CS_3 = ((TAB_C_3 + TAB_S_3) / 2), na.rm = TRUE) %>%
  mutate(TAB_CS_4 = ((TAB_C_4 + TAB_S_4) / 2), na.rm = TRUE) %>%
  mutate(TAB_CS_5 = ((TAB_C_5 + TAB_S_5) / 2), na.rm = TRUE) %>%
  mutate(TAB_CS_6 = ((TAB_C_6 + TAB_S_6) / 2), na.rm = TRUE) %>%
  mutate(TAB_CS_7 = ((TAB_C_7 + TAB_S_7) / 2), na.rm = TRUE) %>%
  mutate(TAB_CS_8 = ((TAB_C_8 + TAB_S_8) / 2), na.rm = TRUE) %>%
  mutate(TAB_CS_9 = ((TAB_C_9 + TAB_S_9) / 2), na.rm = TRUE) 

### Alpha reliabilities for new Achievement and Social subscales ###

### Create subsets for subscale composites ###

TAB_Achievement_Composite <- EC %>%
  select(TAB_CS_1, TAB_CS_2, TAB_CS_3)

TAB_Social_Composite <- EC %>%
  select(TAB_CS_7, TAB_CS_8, TAB_CS_9)

### Create subsets for subscales for coworker/supervisor separately ###

TAB_Ach_Supervisor <- EC %>%
  select(TAB_S_1, TAB_S_2, TAB_S_3)

TAB_Soc_Supervisor <- EC %>%
  select(TAB_S_7, TAB_S_8, TAB_S_9)

TAB_Ach_Coworker <- EC %>%
  select(TAB_C_1, TAB_C_2, TAB_C_3)

TAB_Soc_Coworker <- EC %>%
  select(TAB_C_7, TAB_C_8, TAB_C_9)

### Create a subset of both supervisor and coworker TAB Questions ###
### So that we can melt and make boxplots/barplots all at once ###

TAB_AchSocMWLJ_Both <- EC %>%
  select(TAB_S_1, TAB_S_2, TAB_S_3, TAB_S_4, TAB_S_5, TAB_S_7, TAB_S_8, TAB_S_9,
         TAB_C_1, TAB_C_2, TAB_C_3, TAB_C_4, TAB_C_5, TAB_C_7, TAB_C_8, TAB_C_9)

TAB_AchSocMWLJ_Coworker <- EC %>%
  select(TAB_C_1, TAB_C_2, TAB_C_3, TAB_C_4, TAB_C_5)

### Run the alpha reliability tests for the different subscales ###

a1 <- psych::alpha(TAB_Achievement_Composite)
a1

a2 <- psych::alpha(TAB_Social_Composite)
a2

a3 <- psych::alpha(TAB_LoseJob_Composite)
a3

a4 <- psych::alpha(TAB_Ach_Coworker)
a4

a5 <- psych::alpha(TAB_Soc_Coworker)
a5

a6 <- psych::alpha(TAB_Ach_Supervisor)
a6

a7 <- psych::alpha(TAB_Soc_Supervisor)
a7

a8 <- psych::alpha(TAB_MoreWork_Composite)
a8


### Graphs of variables ###

### Melt data frame into the proper form so histograms/boxplots ###
### can be made for every variable at once. ###

m1 <- melt(as.data.frame(ITabBoth))
m2 <- melt(as.data.frame(CombinedTAB))
m3 <- melt(as.data.frame(TAB_AchSocMWLJ_Both))

### Histograms ###

ggplot(m2, aes(y = value)) + 
  facet_wrap(~variable) + 
  geom_histogram(bins = 5) +
  ylim(0, 5)

ggplot(m2, aes(y = value)) + 
  facet_wrap(~variable) + 
  geom_bar(width = 1) +
  ylim(0, 5)

1?binwidth

### Barplots ###
ggplot(m1,aes(y = value)) + facet_wrap(~variable) + geom_histogram()

ggplot(m2,aes(y = value)) + facet_wrap(~variable) + geom_histogram()

### For newest subscales and dropped number six item below ###

ggplot(m3,aes(y = value)) + facet_wrap(~variable) + geom_histogram()


### Boxplot for newest subscales and dropped number six item ###

ggplot(m3,aes(y = value)) + facet_wrap(~variable) + geom_boxplot()
