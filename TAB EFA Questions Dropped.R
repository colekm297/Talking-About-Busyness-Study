library(reshape)
library(tidyverse)
library(haven)
library(psych)
library(nFactors)
library(GPArotation)

### Send console output to text file ###

sink("EFA Questions Dropped.log", type=c("output", "message"))

### Read in the file ###
EC <- (read_sav("E&C_Thesis_4-30_cleaned_renamed_scalescreated.sav"))


### NEW EFA With Questions Dropped ###

TabBoth_QsDropped <- EC %>%
  select(TAB_S_1,
         TAB_S_2,
         TAB_S_3,
         TAB_S_7,
         TAB_S_8,
         TAB_S_9,
         TAB_C_1,
         TAB_C_2,
         TAB_C_3,
         TAB_C_7,
         TAB_C_8,
         TAB_C_9
  )

### Run EFA ###

TabEFA_QsDropped <- fa(TabBoth_QsDropped, 
                      nfactors = 2, fm = "pa", rotate = "oblimin", 
                      n.iter = 100)
TabEFA_QsDropped

### Alpha reliabilities for new Achievement and Social subscales ###

closeAllConnections()

TAB_Achievement_Composite <- EC %>%
  select(TAB_CS_1, TAB_CS_2, TAB_CS_3)

TAB_Social_Composite <- EC %>%
  select(TAB_CS_7, TAB_CS_8, TAB_CS_9)

TAB_Achievement_Coworker <- EC %>%
  select(TAB_C_1, TAB_C_2, TAB_C_3)

TAB_Achievement_Supervisor <- EC %>%
  select(TAB_C_1, TAB_C_2, TAB_C_3)

TAB_Social_Coworker <- EC %>%
  select(TAB_C_7, TAB_C_8, TAB_C_9)

TAB_Social_Supervisor <- EC %>%
  select(TAB_C_7, TAB_C_8, TAB_C_0)

a1 <- psych::alpha(TAB_Achievement)
a1

a2 <- psych::alpha(TAB_Social)
a2
