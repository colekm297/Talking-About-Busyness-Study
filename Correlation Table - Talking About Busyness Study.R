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

### ---
### Read in Data File
### ---

EC <- (read_sav("E&C_Thesis_8-03_outliers_removed.sav"))

### ---
### Make Composite variables combining coworker and supervisor ratings
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
  mutate(TAB_CS_9 = ((TAB_C_9 + TAB_S_9) / 2), na.rm = TRUE) %>%
  mutate(BAQ_CS = ((BAQ_C + BAQ_S) / 2), na.rm = TRUE) %>%
  mutate(BAQ_CS_1 = ((BAQ_C_1 + BAQ_S_1) / 2), na.rm = TRUE) %>%
  mutate(BAQ_CS_2 = ((BAQ_C_2 + BAQ_S_2) / 2), na.rm = TRUE) %>%
  mutate(BAQ_CS_3 = ((BAQ_C_3 + BAQ_S_3) / 2), na.rm = TRUE) %>%
  mutate(BAQ_CS_4 = ((BAQ_C_4 + BAQ_S_4) / 2), na.rm = TRUE) %>%
  mutate(BAQ_CS_5 = ((BAQ_C_5 + BAQ_S_5) / 2), na.rm = TRUE) %>%
  mutate(BAQ_CS_6 = ((BAQ_C_7 + BAQ_S_6) / 2), na.rm = TRUE) %>%
  mutate(BAQ_CS_7 = ((BAQ_C_8 + BAQ_S_7) / 2), na.rm = TRUE) %>%
  mutate(BAQ_CS_8 = ((BAQ_C_12 + BAQ_S_8) / 2), na.rm = TRUE) %>%
  mutate(BAQ_CS_9 = ((BAQ_C_6 + BAQ_S_9) / 2), na.rm = TRUE) %>%
  mutate(BAQ_CS_10 = ((BAQ_C_9 + BAQ_S_10) / 2), na.rm = TRUE) %>%
  mutate(BAQ_CS_11 = ((BAQ_C_10 + BAQ_S_11) / 2), na.rm = TRUE) %>%
  mutate(BAQ_CS_12 = ((BAQ_C_11 + BAQ_S_12) / 2), na.rm = TRUE)

### ---
### Create TAB and BAQ Subscales with combined supervisor/coworker ratings 
### ---

EC <- EC %>%
  mutate(TAB_CS_Ach = ((TAB_CS_1 + TAB_CS_2 + TAB_CS_3) / 3), na.rm = TRUE) %>%
  mutate(TAB_CS_JobLoss = TAB_CS_4, na.rm = TRUE) %>%
  mutate(TAB_CS_MoreWork = TAB_CS_5, na.rm = TRUE) %>%
  mutate(TAB_CS_Soc = ((TAB_CS_7 + TAB_CS_8 + TAB_CS_9) / 3), na.rm = TRUE) %>%
  mutate(BAQ_CS_Ach = ((BAQ_CS_1 + BAQ_CS_2 + BAQ_CS_3 + BAQ_CS_4) / 4), na.rm = TRUE) %>%
  mutate(BAQ_CS = ((BAQ_C + BAQ_S) / 2), na.rm = TRUE) %>%
  mutate(BAQ_CS_Soc = ((BAQ_CS_9 + BAQ_CS_10 + BAQ_CS_11 + BAQ_CS_12) / 4), na.rm = TRUE) %>%
  mutate(TAB_C_Ach = ((TAB_C_1 + TAB_C_2 + TAB_C_3) / 3), na.rm = TRUE) %>%
  mutate(TAB_S_Ach = ((TAB_S_1 + TAB_S_2 + TAB_S_3) / 3), na.rm = TRUE) %>%
  mutate(TAB_C_JobLoss = TAB_C_4, na.rm = TRUE) %>%
  mutate(TAB_C_MoreWork = TAB_C_5, na.rm = TRUE) %>%
  mutate(TAB_S_JobLoss = TAB_S_4, na.rm = TRUE) %>%
  mutate(TAB_S_MoreWork = TAB_S_5, na.rm = TRUE) %>%
  mutate(TAB_C_Soc = ((TAB_C_7 + TAB_C_8 + TAB_C_9) / 3), na.rm = TRUE) %>%
  mutate(TAB_S_Soc = ((TAB_S_7 + TAB_S_8 + TAB_S_9) / 3), na.rm = TRUE)

### ---
### Subset desired variables in one dataframe to correlate
### ---

TAB <- EC %>%
  select(TAB_CS_Ach,
         TAB_CS_Soc,
         TAB_CS_JobLoss,
         TAB_CS_MoreWork,
         TAB_CS,
         TAB_C_Ach,
         TAB_C_JobLoss,
         TAB_C_MoreWork,
         TAB_C_Soc,
         TAB_S_Ach,
         TAB_S_JobLoss,
         TAB_S_MoreWork,
         TAB_S_Soc,
         TAB_C,
         TAB_S,
         SelfB,
         BE_1,
         BAQ_CS,
         BAQ_CS_Ach,
         BAQ_CS_Soc,
         BAQ_S_Ach,
         BAQ_S_Sec,
         BAQ_S_Soc,
         BAQ_C_Ach,
         BAQ_C_Sec,
         BAQ_C_Soc,
         BAQ_S,
         BAQ_C,
         SB_freq,
         CB_freq,
         RF,
         V_1,
         V_2,
         V_3,
         CM,
         CM_teamsize,
         TP_P,
         TP_U,
         PSC,
         IM_SP,
         IM_IG,
         IM_E,
         IM_ID,
         IM_S,
         WV_CS,
         WV_CG,
         WV_SI,
         SJS,
         JS,
         EE,
         BO,
         JI,
         Age,
         Race,
         Gender,
         Edu,
         Org_Ind,
         Occupation,
         Occ_tenure,
         Org_gendermakeup,
         Org_size,
         Org_tenure,
         Sup_count,
         Hours_perweek,
         Pay_style,
         COVID_1)

### ---
### Run Correlation using corstars function (retrieved online)
### First run function to put it in the workspace
### ---

corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "**  ", ifelse(p < .001, "*   ", ifelse(p < .01, "*   ", ifelse(p < .05, "*   ", "    "))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 3))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 

### ---
### Now use function to make correlation table
### ---

corstar1 <- corstars(as.matrix(TAB), result = "html")

### ---
### Print Correlation Table in Viewer for exporting
### ---
htmlTable(corstar1)

