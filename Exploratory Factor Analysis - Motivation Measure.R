library(tidyverse)
library(haven)
library(psych)
library(nFactors)
library(GPArotation)

### --- 
### Read in data
### --- 

EC <- (read_sav("E&C_Thesis_4-30_cleaned_renamed_scalescreated.sav"))

### ---
### Check Data
### ---

glimpse(EC)

### ---
### Select motivation measure (BAQ) items from dataset
### for both coworker ratings and supervisor ratings
### combined
### ---

TabBoth <- EC %>%
  select(BAQ_S_1,
         BAQ_S_2,
         BAQ_S_3,
         BAQ_S_4,
         BAQ_S_5,
         BAQ_S_6,
         BAQ_S_7,
         BAQ_S_8,
         BAQ_S_9,
         BAQ_S_10,
         BAQ_S_11,
         BAQ_S_12,
         BAQ_C_1,
         BAQ_C_2,
         BAQ_C_3,
         BAQ_C_4,
         BAQ_C_5,
         BAQ_C_6,
         BAQ_C_7,
         BAQ_C_8,
         BAQ_C_9,
         BAQ_C_10,
         BAQ_C_11,
         BAQ_C_12
  )

### ---
### Select motivation measure (BAQ) items from dataset
### for only supervisor ratings
### ---

TabSup <- EC %>%
  select(BAQ_S_1,
         BAQ_S_2,
         BAQ_S_3,
         BAQ_S_4,
         BAQ_S_5,
         BAQ_S_6,
         BAQ_S_7,
         BAQ_S_8,
         BAQ_S_9,
         BAQ_S_10,
         BAQ_S_11,
         BAQ_S_12)

### ---
### Select motivation measure (BAQ) items from dataset
### for only coworker ratings
### ---

TabCowork <- EC %>%
  select(BAQ_C_1,
         BAQ_C_2,
         BAQ_C_3,
         BAQ_C_4,
         BAQ_C_5,
         BAQ_C_6,
         BAQ_C_7,
         BAQ_C_8,
         BAQ_C_9,
         BAQ_C_10,
         BAQ_C_11,
         BAQ_C_12)

### ---
### Create Scree Plot
### ---

ev <- eigen(cor(TabCowork, use = "complete.obs")) # get eigenvalues
ap <- parallel(subject=nrow(TabCowork),var=ncol(TabCowork),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

### ---
### Check for outliers
### ---

outlier_values <- boxplot.stats(TabCowork)$out  # outlier values.
boxplot(TabCowork, main="Coworker Ratings", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

### ---
### Test 2, 3, and 4 factor models 
### for combined motivation ratings
### ---

Fit_Both_2Fact <- fa(TabBoth, 
                     nfactors = 2, fm = "pa", rotate = "oblimin", 
                     n.iter = 100)


Fit_Both_3Fact <- fa(TabBoth, 
                     nfactors = 3, fm = "pa", rotate = "oblimin", 
                     n.iter = 100)

Fit_Both_4Fact <- fa(TabBoth, 
                     nfactors = 4, fm = "pa", rotate = "oblimin", 
                     n.iter = 100)

### ---
### Test 2, 3, and 4 factor models
### for only coworker ratings
### ---

Fit_Cowork_2Fact <- fa(TabCowork, 
                       nfactors = 2, fm = "pa", rotate = "oblimin", 
                       n.iter = 100)


Fit_Cowork_3Fact <- fa(TabCowork, 
                       nfactors = 3, fm = "pa", rotate = "oblimin", 
                       n.iter = 100)

Fit_Cowork_4Fact <- fa(TabCowork, 
                       nfactors = 4, fm = "pa", rotate = "oblimin", 
                       n.iter = 100)

### ---
### Test 2, 3, and 4 factor models 
### for only supervisor ratings
### ---

Fit_Sup_2Fact <- fa(TabSup, 
                       nfactors = 2, fm = "pa", rotate = "oblimin", 
                       n.iter = 100)

Fit_Sup_3Fact <- fa(TabSup, 
                       nfactors = 3, fm = "pa", rotate = "oblimin", 
                       n.iter = 100)

Fit_Sup_4Fact <- fa(TabSup, 
                    nfactors = 4, fm = "pa", rotate = "oblimin", 
                    n.iter = 100)

### ---
### Examine results to find best fit
### ---

Fit_Both_2Fact
Fit_Both_3Fact
Fit_Both_4Fact 
Fit_Cowork_2Fact
Fit_Cowork_3Fact
Fit_Cowork_4Fact
Fit_Sup_2Fact
Fit_Sup_3Fact
Fit_Sup_4Fact
