###################################################################
#
# DataPipe.r - v0.5  - Individual file version
# 
# carlo fanara - July 2018
#
# 
# Input: a tabular or csv file
# 1. could be an 'Elia type' ("untidy") dataframe (.csv/ single xlsx sheet see below)
#  or 
# 2. standard example from R - like 'diamonds' which is mixed (num, cat) type and contains 50k observations
#  or
# 3. a file fit for classification (binary target variable)  for example 
#  from a portuguese bank witjh about 40k observations and some imbalance, ref. 
#  https://rstudio-pubs-static.s3.amazonaws.com/99994_83cedeb7c99a4ebcbe482cde82c17537.html

# output: an individual clean data file and a report


# What it does: sequence of functions to profile data until the
# "pre-modeling",  following the CRISP pipeline:
#  
# 1. ingestion
# 2. formal validation
# 3. dictionaries   NNNNNNNNNNNNNN -- need to compile external tables so TBD
# 4. Exploration
# 5. Missing values
# 6. Outliers and anomalies
# 7. Umbalanced data
# 8. Dimensionality reduction (FA, PCA)
# 9. normalisation, dummification, and Stanrdadization
# 10.tidying (according to Wikham)
# 11. Associations/correlations
# 12. variable importances via simple RF (variable importance measures - VIMs)

# THE MULTIFILE VERSION WILL INCLUDE Joining multiple datasets
#
# ---------------------------------- Individual file version

#------------ EXAMPLES ----------- FILES USED TO CONSTRUCT THIS SCRIPT

# A. ftype="ELIA"
# Original Elia Energy Generation data are stored either as: 
#     1. CSV with all the types following, starting with total - but without
# any field specifying the energy type (e.g. total, nuclear fuel, water...) so one has 
# to rely on non missing records, else the sequence would be totally disrupted
#     2. or, as xlsx fiels, one per month, but each including one sheet per energy source type

# Start with a sheet taken from Elia - energy produced during a month in - say - 2018
# where one sheet has been saved into a CSV (january 2018, total energy produced)
# 
# Ongoing: 1. generalization to all the sheets per workbook - the types per month 
#          2. and further to all months of the year into a year grand total
#          3. to the other situation of several files in a folder structure (2 levels, say year - month or similar)
# 
# In Elia file, following info/aggregations are therefore possible:
# per day, week, month, hour of the day - within 15 min also per Energy type - AND
# once generalized:  the same with 1-year span - plus allow aggregations
# we onsider further real world complications:
# - multifile treatment - separately
# - or chaining mutiple files

# B. ftype="DIAM"
# The diamonds dataset (cf ggplot2) 

# C. ftype="BANK"
# A file from a an AWS post, on a portuguese bank, with about 40k observations and some imbalance, ref. 
#  https://rstudio-pubs-static.s3.amazonaws.com/99994_83cedeb7c99a4ebcbe482cde82c17537.html

# IN THIS WORK IN PROGRESS VERSION, we CAN SELECT BELOW with a code and branch according to the type
# the reason is that for now we cannot be so general to incorprate all possibilities.
# However, there are conmmon steps which are executed at the start.

# ultimate goal:
# - deliver a set of realistic working examples - not the usual minitoys 
# - using as many as possible of the tested "data warangling" procs available in R

# therefore this script is likely to branch into versions according to the model of data used

# mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm   MAIN   mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm 


# ------------- libraries -----------------------------------
# MAY GET "maximal number of DLLs reached"(too many on single file? THEN 
# load locally, e.g. within func and/or use :: operator when possible?)

# Also: several of the commented need to be tested yet

#----- validation, dictionaries and rules for these: should utilize on of these, but we need a pre-dict  or
# rules file beforehand (defy putrpose?)

#library(datacheck)  # need object with data quality rules
#library(dataMeta)   # build a data dictionary
#library(dataMaid)
#library(janitor)    
#library(validate)   # need to construct indicator wirh 'rule'


library(DataExplorer) # useful set of functions
library(xray)  # some anomalies but also distibution or continuous plots

library(standardize)

# ----- missing values
# library(Amelia)
# library(mice)
# library(mi)
# library(Hmisc)
# library(missForest)
library(VIM)
#library(VIMGUI)

# ----- re/structure data and tidy format
library(tidyverse)   # for the non tidy format 
library(dplyr)
library(magrittr)
#library(tibble)

# ----- graphics
library(ggplot2) 
library(dygraphs)
library(htmlTable)
library(htmltools)
library(htmlwidgets)
library(grDevices)

# ----- for PCA and correlations
library(GGally)
library(FactoMineR)
library(ggfortify)
library(factoextra)
library(corrplot)

# ----- anomalies, outliers, extreme values
library(extremevalues)
library(OutliersO3)

# ----- quick ingestion - ever needed?
library(rio)
# excel like ingestion- for the multifile/multisheet version cf data Elia and similar
#library(readxl)
#library(XLConnect)
#library(xlsx)

# ----- umb/imbalance
library(ROSE)
library(DMwR2)
library(imbalance) # (comprehensive)

# ----- we need some for PRE-modeling 
library(caret)
library(rpart)
library(lattice)
library(randomForest)  # worth checking the cforest package perhaps - NUMBER OF PARAMS may be too high
#library(ROCR)
 
## --------------------------  step 0.  paths, dirs, filenames
# mpath<-getwd()
# mpath<-paste0(mpath,"/data/")
# setwd(mpath)

# for testing
where<-"C:/Users/cfanara/Documents/PROJECTS/DQual/data/"

# so, let's CHOOSE one file type - interactively
ftype<-"ELIA"
ftype<-"BANK" 
# FOR NOW, LAST IS THE ONE
ftype<-"DIAM"

if(ftype=="ELIA") {   fname<-"Gen_Jan2018" }

if(ftype=="BANK") {   fname<-"bank-full"   }

if(ftype=="DIAM") {   fname<-"diamonds"   }


# extensions
extIN<-".csv"
extOUT<-".Rds"  # if we wish to write compactly at some point

fprof<-paste0(where,fname, extIN)  # the file to be profiled 
fout<-paste0(where,fname, extOUT)  # the file to be written 

# the potuguese bank - ok also for later binary modeling
if(ftype=="BANK") {
  fname<-"bank-full"
  system.time(ftidy<-fread(fprof, stringsAsFactors=TRUE))  # we have several categorical vars
}

# elia is for the energy - structure needs tidy intervntion (cols, rows)
if(ftype=="ELIA") {
  fname<-"Gen_Jan2018"
  ftidy<-single_tidy(fprof)  
}


# use already tidy dataset diamonds
if(ftype=="DIAM")  {
  data(ftidy<-diamonds)
  ftidy<-diamonds
}
# so we end up with an 'ftidy' anyway

# basic stats
str(ftidy)
head(ftidy, 10)
summary(ftidy)

# need to handle datatype before NA!



## ---------------------------- step 1.   NA values

# if there is at least 1 NA it plots the % of NA and then calls manage_NA()
if(anyNA(ftidy))  {
  print(plot_missing(ftidy), title=paste(fname, "missing values")) 
  nmode<-"drop"
  dfT<-Manage_NA(ftidy, nmode)  ### call function to manage NA's, nmode is "drop" for now
  ftidy<-dfT
}

# so the tidy is now deprived of NA
# p<-ftidy[!complete.cases(ftidy), ]  # just check during debugging

### ---------------------------- step 2a.  Sequence of EDA - DataExplorer function calls
# structure, categorical and continuous plots, correlations...

res<-Dexplore(ftidy, ftype, fname)  # pass the file, its type and its name

### some funcs inside do not display even when print reiterated??


### ---------------------------- step 2b.  Specific on Correlation using Ggally
# rich - nice to show - a bit SLOW ('many' secs for 54k points), so we select a 'smallds'...May comment  though
# Need to find a way to pre-select vars, perhaps from previous step via simple GUI (list the vars to be used, 
# including dependent / target variable). For now, we just set reasonable choice for the relevant files. 

if(ftype=="DIAM")  {
  smallds = subset(ftidy, select=c("x", "y", "z", "clarity", "price"))  # only some cols
  ggpairs(smallds, mapping = aes(color = clarity), 
        diag=list(continuous="density", discrete="bar"), axisLabels="show")
}

if(ftype=="BANK")  {
  smallds = subset(ftidy, select=c("age", "education", "duration", "balance"))  # only some 
  ggpairs(smallds, mapping = aes(color = education), 
          diag=list(continuous="density", discrete="bar"), axisLabels="show")
}


if(ftype=="ELIA")  {
  # smallds = subset(ftidy, select=c("zzzz","yyyy"))  # only some 
  # ggpairs(smallds, mapping = aes(color = clarity), 
  #         diag=list(continuous="density", discrete="bar"), axisLabels="show")
}
###----------------------------- step 3.  PCA: get dependencise and variables contributions  

# Used to get the important variables and correlations using  PCA (thus linear).
mtitle=fname  # pass filename for plotting
done<-CorrPCA(ftidy, mtitle) # run plotting sequence based on pca done with factoMine /factoextra R library


###----------------------------- step 4.  Outliers and anomalies  
# NB ANOMALIES, for TS and multivar, see separate anomaliesDetection.R


## 1. anomalies with extremevalues package #### 
# plot output not very clear to me, need to check better
# from extremevalues, 

if(ftype=="DIAM")  {  # file diamonds
  y = ftidy$price # just choose one variable, but depending on var may ask for the target explicitly
}

if(ftype=="BANK")  {  # file BANK
  y = ftidy$balance # just choose one variable, but depending on var may ask for the target explicitly
}
  
# the chosen y 
res1 <- getOutliers(y = y, method =  "I")
outlierPlot(y, res1, mode="qq", title="outliers", xlab="a.u")
# [if we have reasons to believe one var has a dependency different than 
# normal - cf results of the xray steps then may ask user which distribution]

res2 <- getOutliers(y = y, method =  "II")
outlierPlot(y, res2, mode="residual", title="outliers-Method II - residuals", xlab="a.u")
###

## 2. XRAY use formal with the various profiling etc
# library(xray)

# anomalies in xray: for test data like diamonds could dirtify the file see general notes
# next version , make diamonds dirty (NA, point anomalies)
dfT<-ftidy
xray::anomalies(dfT) 

## 3. probab distrib funcs (pdf)
xray::distributions(dfT)  # Warning message: In eval(xpr, envir = envir) :

# Ignoring variable time: Unsupported type for visualization for the Elia or bank data.
# in this case:
# timebased(data_analyze, date_variable, time_unit = "auto",
#           nvals_num_to_cat = 2)
# -----------
# or, if we have several vars and wish to save plots in an external folder as .png
# timebased(data_analyze, date_variable, time_unit = "auto",
#           nvals_num_to_cat = 2, outdir)
# ----------
# for diamonds data, shows one distrib per each of the cont vars.
# better to use a grid system to show all on one page?

# for data with time var - not nec TS - can ask user to check timebase
# it gives the time dep of all other variables. Ex using Elia data ...


## 4. with comprehensive outliers package O3 #######
# just a note: objects of type ts are treated inherently slowly, probably
# devised when ts included just a few hundreds of points at max, for monthly or
# so type of data...


## 5. with LOF package  

#--------------------------------------------------------------------------
# preceding can be the 1-var part of anomaliesDetection - separate project
# once progressed there, modif put inti here
#--------------------------------------------------------------------------


###----------------------------- step 5.   UN/IM/BALANCED DATA? 

# First need to establish target variable. 
# For DIAMONDS field 'price' 
# FOR BANK
# for ELIA Generation, it could be the delivered power per type - thus multi-class problem - NOT NOW

# for now check imbalance specifying file and class
# NB for binary vars!
# so only for file banks
if(ftype=="BANK")  { 
  iratio<-imbalanceRatio(ftidy, classAttr = "y")
  }
print(paste("Data imbalanced by:", substr(as.character(iratio*100),1,5), "%"))

# we may set the arbitrary limit of umbalance at 15% [very arbitrary]
# we end up with a test, asking - interactively - whether we deal with umbalance via one of the three
# (four) methods known up/down/both or Rose or Smote packages in R, for ex.
# https://shiring.github.io/machine_learning/2017/04/02/unbalanced
# library(ROSE)
# library(DMwR2)


# ---------------- step 6.   Simple Random Forest Classifier (variable importance measures - VIMs)
## we use this to get variable importance  
# for now only binary target (thus file BANK).
# [Later may parametrize, perhaps again interactively (e.g. regression or binary classification)]

# PART A. We assume BALANCED dataset, e.g. prev. section does not balance yet [but it might be too far
# for a profiling/data quality purpose]. TBD
# 
#
# OR, BASED ON:
# Janitza et al. BMC Bioinformatics 2013, 14:119
# http://www.biomedcentral.com/1471-2105/14/119
# PART B - WE USE a modified VIM AUC based iteration to get decent VIM's even WITHOUT ADDRESSING IMBALANCE 
# 
# tHIS PART IS EXPERIMENTAL AND IT IS worth checking the difference in the VIMs - e.g aside of the modelling
# using some pre-assessed set of data - where imbalance management has been performed. If succesful,
# the method could be a useful shortcutin those cases where imbalance is present
# (and perhaps also in 'normal balanced situations?)


# -------------PART A ---------------------------------- 
# rf - random forest classifier 400 decision trees and entropy criterion for splits. First creates a file to train in the subsequent function

set.seed(41235)  

# Note, for this ML alg we do not scale (could smooth out nonlinearity)

mtitle="All Variables"

Trn<-ModRF1(ftidy, mtitle) # call the func. NB the Trn will be the train for EITHER OF THE TWO types


# -------------PART B ---------------------------------- 
# modified VIM
# rf - random forest classifier 400 decision trees and entropy criterion for splits. First creates a file to train in the subsequent function

set.seed(41235)  

mtitle="All Variables"


# mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm     end MAIN     mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm




# ffffffffffffffffffffffffffffffffffffffff     FUNCTIONS    ffffffffffffffffffffffffffffffffffffff



##############################################
# FUNCTION:  CorrPCA 

# INPUT: individual file path and name, untidy
# OUTPUT: 0 if ok - later do we handle errors (try etc)?

# What: 
# - Dunmmifies received file 
# - makes correlation matrix
# - does PCA (factoMineR)
# - plots: 
#       correlation, scree, individual and global contribution, correlation circle
# - kmeans: to show groups of variables - in different colors and 
#           puts these on another correlation circle

# Might add: correspondence analysis with description in output
###############################################

CorrPCA<-function(Dfil, mtitle)  # when calling, same title as in rf
{
  
  library(FactoMineR)
  Dfil<-dummify(Dfil, maxcat = 9L) # we used 9 > max nr of cat, so ALL will be dummified
  
  # NB only numric values, so need selection
  Dfil<-select_if(Dfil, is.numeric)
  
  cor.mat <- round(cor(NDfil[,2:dim(Dfil)[2]]), 2) # last 2: nr. digits after the comma
  head(cor.mat[, 1:10])
  corrplot(cor.mat, type="upper", order="hclust", 
           tl.col="black", tl.srt=45)
  
  res.pca <- PCA(Dfil, graph = FALSE)  # run the PCA, no plot, use following
  # print(res.pca) # just in case, do we print its results...
  # eig.val <- get_eigenvalue(res.pca) #...and the eigenvalues?
  # print(eig.val)
  
  
  # ----------Generate variables for correlation plots and circle
  var <- get_pca_var(res.pca)
  # var # these are: coordinate, corr btw vars and dim of pca, cos2, contrib to PC1,2
  # print(var) # just for check
  
  # ---------- Quality of representation
  # corr plot: oma not working, so margins are rubbish !!!!!!!!
  corrplot(var$cos2, is.corr=FALSE, oma=c(2,2,4,2)) 
  title("PCA - Plot 1 - cos2", oma=c(3,2,2,1), col.main = "blue", cex.main = 1.5, font.main= 4, line = 2)
  
  # ---------- Contributions of variables to PCs 
  corrplot(var$contrib, is.corr=FALSE) 
  title("PCA - Plot 2 - VAriable Contributions", col.main = "blue", cex.main = 1.5, font.main= 4, line = 2)
   
  # (preceding two could get together side by side with par() or grid package)
  
  # Contributions of the variables to PC1 
  print(fviz_contrib(res.pca, choice = "var", title=paste(mtitle," PCA - Plot 3 - Contrib. to PC1"), axes = 1, top = 10))
   
  
  # Contributions of the variables to PC2
  print(fviz_contrib(res.pca, choice = "var", title=paste(mtitle," PCA - Plot 4 - Contrib. to PC2"), axes = 2, top = 10))
  
  
  # total contribution of variables to both dimensions
  print(fviz_contrib(res.pca, choice = "var", title=paste(mtitle," PCA - Plot 5 - Contributions to Both "), axes = 1:2, top = 10, 
                     col.main = "blue", cex.main = 1.5, font.main = 14))
  
  
  # ---------- Correlation circle with color on high important var
  print(fviz_pca_var(res.pca, col.var = "contrib", title=paste(mtitle," PCA - Plot 6 - Corr. Circle "),
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")))

  
  #----------- color by grouping: grouping using kmeans, but need estimation of groups (k) from previous step
  # show table / rank with main k elements and assign these to ctrs?
  head(var$coord, 10) # coordinates of variables
  head(var$cos2) # cos2 of variables
  head(var$contrib) # contributions of variables
  
  ctrs=10  # so, how to assign this value?
  set.seed(123)
  res.km <- kmeans(var$coord, centers = ctrs, nstart = 25)
  grp <- as.factor(res.km$cluster)
  # (following: here we only have cont variables - Color variables by groups (we have k=3 types?)
  
  rainbowcols <- rainbow(ctrs)
  # 
  print(fviz_pca_var(res.pca, col.var = grp, title=paste(mtitle," PCA - Plot 6 - Corr. Circle with clusters"), palette=rainbowcols, legend.title = "Cluster"))

  
  # following for correspondence analysis - qualitative vars, which perhaps should be run also
  # find most significantly associated vars with a given principal component, limiting to 1 and 2
  # res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
  # Description of dimension 1
  # res.desc$Dim.1
  ##### check why p values so low: un/reliable?
  
  # may want error handling (for all funcs actually (try check...))
  return 
  
}





##############################################
# FUNCTION: Dexplore

# INPUT: individual file path and name 
# OUTPUT: 0 if ok - later do we handle errors (try etc)?

# What: 
#  1. NOTE "create_report(ftidy, y = "price") #" directly, fails at 71%, with error:
#  << Quitting from lines 154-163 (report.rmd) ... Rerun with Debug
#  Error in `[.tbl_df`(data, , -response, with = FALSE) : 
#  unused argument (with = FALSE) >>

# So, then
# 2. Performs sequence of function calls within the DataExplorer package
# - Sequence:
#     plots Data structure (diagonal by default), 
#     
#     then it plots:
#     - barplot of categorical
#     - barplot versus a target - if chosen (e.g. 'price' in data diamonds
#     - continuous variables: bar and density plots
#     - overall correlation heatmap
#     - discrete (only) and continuous (only) correlation plots
#     - scatter plot of continuous - upon selection of continuous vars
#     - (could manage overplotting)
#     - group per chosen category (barplots)
#     - dummify (1-hot encode) and plot structures - both
# NOTE
#    We could use DataExplore pca functions but we have a dedicated function. Still, may uncomment final part
#    get a quick viz - no variable selection is done though.
# 
###############################################

Dexplore<-function(fexpl, ftype, fname) {
  
  # 0. initial data structure
  print(plot_str(fexpl,type="diagonal", fontSize = '16', textColour='#0008B' ), title=fname)
  
  
  # 1. bar plots for each factor variable
  # (diamonds: cut, color, clarity), three frequency (default) or "price"
  # BANK : with maxcat = 15, we get all, e.g. job, marital, education, housing, loan, contact, month
  print(plot_bar(fexpl, maxcat = 15, ggtheme = theme_light()))
  
  # if specified - barplots summing over the continuous variable, "price" for diamonds, "balance" for bank
  # (again arbitrary!!!)
  
  if(ftype=="DIAM") {print(plot_bar(fexpl, with="price", ggtheme = theme_light())) }
  
  if(ftype=="BANK") {print(plot_bar(fexpl, with="balance", ggtheme = theme_light())) }

  
  # decorations
  # theme_config = list(
  #   "plot.background" = element_rect(fill = "orange"),
  #   "aspect.ratio" = 1
  # ))
  
  ## 2. View distribution of all continuous variables
  print(plot_histogram(fexpl, title = "Frequencies for continuous features"))
  
  # or to emphasize continuity, curves instead
  print(plot_density(fexpl))
  
  ## 3a. Correlation heatmaps, can do the total - overall - but also keep the 
  # discrete separate from continuous, or hihlight individual features ("y~x")
  
  ##### View overall correlation heatmap
  print(plot_correlation(fexpl))
  
  ## 3b. discrete only
  print(plot_correlation(fexpl, type="discrete", 
                         ggtheme = theme_gray(base_size = 13, base_family = "serif"),
                         theme_config = list("text" = element_text(color = "blue"))))
  ## 3c. continuous only 
  print(plot_correlation(fexpl, type="continuous", ggtheme = theme_gray()))
  
  ## 4. View  continuous bivariate distribution based on `price`
  # again, need generalization to select the dep. variable 
  if(ftype=="DIAM")     { 
    print(plot_boxplot(ftidy, by = "price")) 
    
    partial<-ftidy[,c(1,6,7)] # or, select some features and make a 
    ## 5. Scatterplot `price` with all other features
    print(plot_scatterplot(partial, by = "price"))
    
  }
  
  if(ftype=="BANK")     { 
    print(plot_boxplot(ftidy, by = "balance"))
    # factors are c(2:5, 7:9,11)
    partial<-ftidy[,c(1, 6, 10, 12)] # or, select some features and make a 
    ## 5. Scatterplot `balance` with all other features
    print(plot_scatterplot(partial, by = "balance"))
    
  }
  
  # overplotting? # geom_count()  or geom_point() - could be remedy
  # geom_count(mapping = NULL, data = NULL, stat = "sum",
  #            position = "identity", ..., na.rm = FALSE, show.legend = NA,
  #            inherit.aes = TRUE)
  
  ## Apply DataExplorer group category on selected categorical (for diamond: "color")
  if(ftype=="DIAM")     { 
    group_category(ftidy, "color", .35, update=FALSE) # for diamonds: color
    plot_bar(ftidy$color,
           theme_config = list(
             "plot.background" = element_rect(fill = "grey"),
             "aspect.ratio" = 1
           ))
  }
  
  
  if(ftype=="BANK")     { 
    group_category(ftidy, "education", .35, update=FALSE) # for bank: education
    plot_bar(ftidy$education,
             theme_config = list(
               "plot.background" = element_rect(fill = "grey"),
               "aspect.ratio" = 1
             ))
  }
  
  # if update=TRUE, update and plot the bars again:
  #group_category(ftidy, "color", .35, update=TRUE)
  
  ## dummyfy the categorical - also "one hot encoding" -  and plot structure for both original 
  # as done at beginning - and dummyfied tree:
  plot_str(
    list(
      "original" = ftidy,
      "dummified" = dummify(ftidy, maxcat = 5L)
    ), fontSize = '16', textColour='#0008B'
  )
  
  # may want error handling (for all funcs actually (try check...))
  return 
  
}




##############################################
# FUNCTION:  ModRF1

# Why modelling here!? aren't we in EDA still?! Yes, but we explore the variable
# dependencies taking the VarImportance output as hint for further analysis - very much 
# like the PCA and correlation results

# INPUT: train file
# OUTPUT: 0 if ok - later do we handle errors (try etc)?

# What: 
# - creates data partition train test validation
# - does randomForest, default from package randomForest
# - does prediction on validation plots: 
#       ROC, AUC and Variable Importance
# NB binary classification for now, needs param to test
#    either classification or regression and modify accordingly
# but look at RF library defualts: it can decide autonomously

###############################################
ModRF1<-function(DatatoPass, type, mtitle)
{
  # ----------------- RF - preparation: the data partitions ("splitting")
  # for debugging, get small file
  DatatoPass<-DatatoPass[1:1000,]
  inBuild<-createDataPartition(y=DatatoPass$y, p=0.7, list=FALSE) 
  Val<-DatatoPass[-inBuild,]        # take out the 30% validation
  buildData<-DatatoPass[inBuild,]   # the remaining 70% will be split into train and test
  
  inTrain<-createDataPartition(y=buildData$y, p=0.3, list=FALSE) # train will be 70%, test 30%
  Train<-buildData[-inTrain,]
  Test<-buildData[inTrain,]
  
  # ----------------- RF - train 
  ft1 = randomForest(y ~ ., data = Train, ntree=500, importance=TRUE) # classLabel against all other variables
  print(ft1)
  
  # ----------------- RF - run on validation
  # run model on validation and assess results: predict with the "fit" just trained, forestTrain, on the 
  # validation data. 
  forestVal = predict(ft1, newdata=Val)                # this is the predition object and we seek the probability
  table(forestVal, Val$y)
  
  forest.Val = predict(ft1, type='prob', newdata=Val)  # predict using the "fit" from Train, mode is 'prob'
  forestpred = ROCR::prediction(forest.Val[,2], Val$y)       # select second column of predicted
  
  forestperf = performance(forestpred, 'tpr', 'fpr')   # evaluate true positive and false positive rates
  
  # par(mfrow=c(1,1)) # want it big
  plot(forestperf, main='ROC', colorize=T)
  abline(a=0,b=1,lwd=2,lty=2,col="gray")               # the 50-50 diagonal 
  
  # ---------------- RF - the AUC (Area under the curve)
  Pred.Val.AUC<-performance(forestpred, measure="auc")@y.values
  Pred.Val.AUC 
  
  # ---------------- RF - the variable importance
  # plot the mean decrease in node purity and the variable importance
  par(mfrow=c(2,1))
  par(pty="s")
  # plots
  VI1<-varImpPlot(ft1, type=1, pch=19, col=1, cex=.75, main=paste(mtitle," Variable Importance")) # variable importance %MCSE
  print(VI1)
  
  VI2<-varImpPlot(ft1, type=2, pch=19, col=1, cex=.75, main=paste(mtitle," Node Purity")) # node purity, Residual Sum of Squares
  print(VI2)
  
  # could also look at usage of the variables in forest, the vector of integer frequencies
  # varUsed(ft1, by.tree=FALSE, count=TRUE) # uncomment if needed
  
  # par(mfrow=c(1,1)) # re-establish graph param to one figure per page
  
  # given we plot anyway and we are still ecxploratory, we return just the 
  # train used for further analysis - like correlations
  
  return(Train) 
  
}





##############################################
# FUNCTION: single_tidy  

# -- Individual file version

# - it reads the header, used to structure the data.
# - it reads the data, 
# - it addso columns according to the header, where we find hh:mm, 96 times a day - e.g. data taken every 15 minutes
# - once rows are produced, it sorts them
# INPUT: individual file path and name, untidy
# OUTPUT: individual file path and name, tidy, optionally re-written for later aggregation (on choice)
###############################################
single_tidy<-function (lfile) {

  # The header (just one row): used for subsequent tidying up
  hdr<-read.csv2(lfile, sep = ",", quote = "\"", dec = ".", fill = TRUE, skip = 2, nrows=1, stringsAsFactors = FALSE)
  hdr<-hdr[1,- c(3,5,6)] # suppress the empty
  
  hdr<-unite(hdr, ddmmyyyy, X, X.1, X.3, sep="/", remove=TRUE) # to have one datetime column
  
  # the data
  dat<-read.csv2(lfile, sep = ",", quote = "\"", dec = ".", fill = TRUE, skip = 4, stringsAsFactors = FALSE)
  dat<-dat[,- c(2,4,6)] # suppress the empty - specific to elia data!
  
  dat<-unite(dat, ddmmyyyy, X,X.2, X.4, sep = "/", remove = TRUE)
  ## -- VALUES ARE NOW THE BASIS OF THE TIMESTAMP
  
  
  # now gather, to get the long format; first need to rename 'X' variables with their content (hh:mm) - the 'every 15 min bits'
  colnames(dat)<-c( hdr[,1:length(hdr)])
  
  ####-------- reshape the sheet into a tidy format file
  long_dat <- dat %>% gather(hhmm, PrdEnergy, "00:15":"00:00")
  # almost: we need still have all the the dates with the same hhmm; We need instead the day with all the times
  # (96 records per day), so sort by ddmmyyy, then hhmm, then unite the two to finally get the TS structure
  
  # CAUTION: original data badly organized as the "00:00" FOLLOWS the 23:35 of the day (!)
  # so rename all the "00:00"  to " 24:00" beforehand:
  index <- long_dat$hhmm=="00:00"
  long_dat$hhmm[index] <- "24:00" 
  
  # Sort by column 1 then 2 to get the final dataframe:
  df<-long_dat [
    order( long_dat [,1], long_dat [,2] ),
    ]
  
  # now form the date time column
  dfT<-unite(df, 'time', 'dd/mm/yyyy', 'hhmm', sep = " ", remove = TRUE)
  
  frds<-FALSE # frds: do we also save in rds format?
  if(frds==TRUE) { # optionally  write the file - in compact Rds FORMAT
    fout<-paste0(where,fname)
    fout<-paste0(fout,extOUT)
    saveRDS(ftidy, fout)
  }
  
  # may want error handling (for all funcs actually (try check...))
  return(dfT) # return tidy file

}   # ---------------------  end single_tidy  ------- Individual file version




###############################
#  FUNCTION: Manage_NA
#
# called if at least one na is found
# For now, uses only VIM to visualize and it drops the records containig NA (Drastic!)

# input: tidy file
# output: same file without the NA's

# TODOs: 
# 1. allow choice between VIM and Amelia or MICE (better) or non parametric (HMisc?)
# insert variable mode:
#    mode "visu", just visualize on a map (Amelia or VIM style)
#    mode "drop", visualize on a map (Amelia or VIM style) and drop the records
#    mode= "impu"  imputes but will need a sub-selection of the numerous methods 
#    
#    TEST: IF % <20% or other arbitrary threshold, be DRASTIC==> GET RID OF THE CORRESPONDING RECORDS

# However, note, there is a VIM GUI....
###############################
Manage_NA<-function (lfile, NAmode) {

  # for now, we do not manage other than cutting
  # NAmode="drop"
  
  # from library(VIM)
  aggr_plot1<- aggr(lfile, col=c('darkred','wheat'), numbers=TRUE, combined = TRUE, bars=TRUE, sortVars=TRUE, 
                    labels=names(lfile), cex.axis=.7, gap=3, ylab=c("Histogram Missing data","missing Pattern"))
  
  print(aggr_plot1)  # "print" here: eeh, needs to be explicit inside funcs....
  
  #... continue with a deletion of the observations without values (arguably introduced by format conversion of data)
  if(NAmode=="drop") { 
    Clfil<-lfile[complete.cases(lfile), ]
    }
  print(summary(Clfil))
  # other mode/s here: SMOTE
  # if(NAmode=="smot") { lfile<-lfile[XXXXX ]
  #                    }
  
  # whatever the method, we plot again
  aggr_plot2<- aggr(Clfil, col=c('darkgreen','wheat'), numbers=TRUE, combined = TRUE, bars=TRUE, sortVars=TRUE, 
                    labels=names(Clfil), cex.axis=.7, gap=3, ylab=c("Histogram Missing data","missing Pattern"))
  print(aggr_plot2)  # ... explicit inside funcs....
  
  
  # may want error handling (for all funcs actually (try check...))   
  return(Clfil)

} 


################################## THIS IS THE END (MY FRIEND)  #######################

