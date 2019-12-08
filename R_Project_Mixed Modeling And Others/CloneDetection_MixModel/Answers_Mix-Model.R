library(tidyverse)
library(lubridate)
# theme_tq()
library(tidyquant)
# Excel Files
library(readxl)
library(writexl)
library(dplyr)
library(stringr)
library(tidyverse)
library(lubridate)
library(forcats)
library(forcats)
library(dplyr)
library(ggplot2)
library(purrr)
library(recipes)

library(car)
library(MASS)
library(lme4)
library(mlmRev)
library(agridat)
library(MCMCglmm)
library(mlmRev)
library(ggplot2)
library(scapeMCMC)
library(nlme)
#install.packages("plotMCMC")
#install.packages('plotMCMC',repos='http://cran.us.r-project.org')
library(plotMCMC)


Answers_Mix_Model_tbl = readr::read_csv("Data_CSV_CloneDetection/asnwersFactorsNoZeroVariance.csv")
ZeroVariance_Functions = readr::read_csv("Data_ExportedDatafromR_Zero_Variance/AnswersExportZeroVarianceFunctions.csv")
ZeroVariance_Blocks = readr::read_csv("Data_ExportedDatafromR_Zero_Variance/AnswersExportZeroVarianceGroupsBlocks.csv")
explanatoryVariable = Answers_Mix_Model_tbl

# data wrangglingin 
NoVarianceBlocks = ZeroVariance_Blocks %>%
    filter(CloneGroupVariance == 0) %>%
    select(CloneGroupType,X2 , CloneGroupVariance)

groupsInVectorBlocks = pull(ZeroVariance_Blocks %>%
    filter(CloneGroupVariance == 0) %>%
    select(CloneGroupType))
                     

NoVarianceFunctions = ZeroVariance_Functions %>%
    filter(CloneGroupVariance == 0) %>%
    select(CloneGroupType,X2,CloneGroupVariance)

groupsInVectorFunctions = ZeroVariance_Functions %>%
    filter(CloneGroupVariance == 0) %>%
    select(CloneGroupType)


CleanExplanatoryVariable1 = Answers_Mix_Model_tbl %>%
    left_join(NoVarianceBlocks, by = c("CloneClassnumber"="CloneGroupType", "Grunality"="X2")) 
    #filter(Grunality == "Blocks") 

CleanExplanatoryVariable2 = Answers_Mix_Model_tbl %>%
    filter(Grunality == "Functions") %>%
    left_join(NoVarianceFunctions, by = c("CloneClassnumber"="CloneGroupType"))


CleanExplanatoryVariable1$CloneGroupVariance = CleanExplanatoryVariable1$CloneGroupVariance %>% replace_na(1)
CleanExplanatoryVariable2$CloneGroupVariance = CleanExplanatoryVariable2$CloneGroupVariance %>% replace_na(1)

CleanExplanatoryVariable1 %>%
    filter(CloneGroupVariance == 0) %>%
    select(CloneClassnumber, CloneGroupVariance)


explanatoryVariable1 = CleanExplanatoryVariable1 %>%
    filter((CloneGroupVariance == 1) & (Grunality == "Blocks"))
    select(CleanExplanatoryVariable1, -c(CloneGroupVariance))
    
explanatoryVariable2 = CleanExplanatoryVariable2 %>%
        filter((CloneGroupVariance == 1) & (Grunality == "Functions"))
    select(CleanExplanatoryVariable2, -c(CloneGroupVariance))
#bind both table after removing the zero variance
explanatoryVariable <- rbind(explanatoryVariable1, explanatoryVariable2)


#%>%
#mutate(Q_Score_Updated = Q_Score) %>%
#dplyr::
#select(CodeLength,Q_NooffavouriteCount,Q_length,Q_CreateDate,Q_TtleLength,
# Q_NoofViewCount,Q_NoOfTags,Q_Score )

#CHECK DATA DISTRIBUTION
names(explanatoryVariable)
summary(explanatoryVariable)
skewness(explanatoryVariable$A_length)
kurtosis(explanatoryVariable$A_length)

plot(hist(explanatoryVariable$A_Score)) #left Skewed
plot(hist(explanatoryVariable$A_TotalCodeLength))  #left Skewed
plot(hist(explanatoryVariable$A_NoOfBoldTags)) #left Skewed
plot(hist(explanatoryVariable$A_NoOfComments)) # replace null value with 0
plot(hist(explanatoryVariable$A_LinkTags)) #left Skewed
plot(hist(explanatoryVariable$A_NoOfEditOrRevision)) #left Skewed
plot(hist(explanatoryVariable$A_CodeRatioPercentage)) #right skewed

plot(hist(explanatoryVariable$Answerer_ReputationScore)) #left Skewed
plot(hist(explanatoryVariable$A_ItalicTags)) #left Skewed
plot(hist(explanatoryVariable$A_length)) #left Skewed
plot(hist(explanatoryVariable$A_NoOfBoldTags)) #left Skewed
plot(hist(explanatoryVariable$A_NoOfComments)) # replace null value with 0
plot(hist(explanatoryVariable$A_LinkTags)) #left Skewed
plot(hist(explanatoryVariable$A_NoOfEditOrRevision)) #left Skewed
plot(hist(explanatoryVariable$A_CodeRatioPercentage)) #right skewed
plot(hist(explanatoryVariable$A_NoOfCodeSnippets)) #left Skewed
plot(hist(explanatoryVariable$Answerer_PostedAnswers)) #left Skewed
plot(hist(explanatoryVariable$Answerer_PostedQuestions)) #left Skewed
plot(hist(explanatoryVariable$Answerer_DownVotes)) #left Skewed
plot(hist(explanatoryVariable$Answerer_Upvotes)) #left Skewed


basic.lm <- lm( A_Score_Updated~A_length , data = explanatoryVariable)
summary(basic.lm)
library(ggplot2)  # load the package
(prelim_plot <- ggplot(explanatoryVariable, aes(x = A_length, y = A_Score_Updated)) +
        geom_point() +
        geom_smooth(method = "lm"))


str(explanatoryVariable)
head(explanatoryVariable)


##### What probability distribution best fits your data?
#### find the distribuition of the variable and best fits with data....
explanatoryVariable$A_TotalCodeLength.t <- explanatoryVariable$A_TotalCodeLength + 1
qqp(explanatoryVariable$A_TotalCodeLength.t, "norm")
# lnorm means lognormal
qqp(explanatoryVariable$A_TotalCodeLength.t, "lnorm")


explanatoryVariable$A_Score.t <- explanatoryVariable$A_Score + 1
qqp(explanatoryVariable$A_Score.t, "norm")
# lnorm means lognormal
qqp(explanatoryVariable$A_Score.t, "lnorm")



# qqp requires estimates of the parameters of the negative binomial, Poisson
# and gamma distributions. You can generate estimates using the fitdistr
# function. Save the output and extract the estimates of each parameter as I
# have shown below.
nbinom <- fitdistr(explanatoryVariable$A_Score_Updated, "Negative Binomial") # 
#for score its not working because it has negative data
qqp(explanatoryVariable$A_Score_Updated, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])

poisson <- fitdistr(explanatoryVariable$A_Score_Updated, "Poisson")
qqp(explanatoryVariable$A_Score_Updated, "pois", poisson$estimate ) 
# this statement did not worked may be worked for other variable who have now negative score

gamma <- fitdistr(explanatoryVariable$A_Score_Updated, "gamma")
qqp(explanatoryVariable$A_Score_Updated, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])

gamma <- fitdistr(explanatoryVariable$A_TotalCodeLength, "gamma")
qqp(explanatoryVariable$A_TotalCodeLength, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])

# Check the distribution of data
summary(explanatoryVariable$CodeLength)
plot(hist(explanatoryVariable$CodeLength)) # all data are left skewed, except the number of tags, title length





####### if the data is not normally distributed then follow this procedure
#replace with 0 values who have negative data 
explanatoryVariable = explanatoryVariable %>%
    mutate(A_Score_Updated = A_Score) 
explanatoryVariable$A_Score_Updated <- 
    replace(explanatoryVariable$A_Score_Updated, which(explanatoryVariable$A_Score_Updated < 0), 0)

# increase by 1 in each column because with zero value the function can not be run it will provide error
explanatoryVariable$A_NoOfBoldTags = explanatoryVariable$A_NoOfBoldTags + 1

explanatoryVariable$A_NoOfCodeSnippets = explanatoryVariable$A_NoOfCodeSnippets + 1
explanatoryVariable$A_NoOfComments = explanatoryVariable$A_NoOfComments + 1
explanatoryVariable$A_NoOfEditOrRevision = explanatoryVariable$A_NoOfEditOrRevision + 1
explanatoryVariable$Answerer_ReputationScore = explanatoryVariable$Answerer_ReputationScore + 1
explanatoryVariable$Answerer_Upvotes = explanatoryVariable$Answerer_Upvotes + 1
explanatoryVariable$Answerer_DownVotes = explanatoryVariable$Answerer_DownVotes + 1
explanatoryVariable$Answerer_PostedQuestions = explanatoryVariable$Answerer_PostedQuestions + 1
explanatoryVariable$Answerer_PostedAnswers = explanatoryVariable$Answerer_PostedAnswers + 1
explanatoryVariable$Answerer_NoOfRevision = explanatoryVariable$Answerer_NoOfRevision + 1
explanatoryVariable$Answerer_NoOfRevisionInAnswer = explanatoryVariable$Answerer_NoOfRevisionInAnswer + 1
explanatoryVariable$A_CodeRatioPercentage = explanatoryVariable$A_CodeRatioPercentage + 1
explanatoryVariable$A_LinkTags = explanatoryVariable$A_LinkTags + 1
explanatoryVariable$A_ItalicTags = explanatoryVariable$A_ItalicTags + 1
explanatoryVariable$A_TotalCodeLength = explanatoryVariable$A_TotalCodeLength + 1

explanatoryVariable$A_codeRatio = explanatoryVariable$A_codeRatio + 1
explanatoryVariable$Answerer_25PercentileVotes = explanatoryVariable$Answerer_25PercentileVotes + 1
explanatoryVariable$Answerer_MinVotes = explanatoryVariable$Answerer_MinVotes + 1
explanatoryVariable$Answerer_MeanVotes = explanatoryVariable$Answerer_MeanVotes + 1
explanatoryVariable$Answerer_MedianVotes = explanatoryVariable$Answerer_MedianVotes + 1
explanatoryVariable$Answerer_MaxVotes = explanatoryVariable$Answerer_MaxVotes + 1
explanatoryVariable$Answerer_VarianceVotes = explanatoryVariable$Answerer_VarianceVotes + 1

explanatoryVariable$Answerer_25PercentileDwnVotes = explanatoryVariable$Answerer_25PercentileDwnVotes + 1
explanatoryVariable$Answerer_MinDwnVotes = explanatoryVariable$Answerer_MinDwnVotes + 1
explanatoryVariable$Answerer_MeanDwnVotes = explanatoryVariable$Answerer_MeanDwnVotes + 1
explanatoryVariable$Answerer_MedianDwnVotes = explanatoryVariable$Answerer_MedianDwnVotes + 1
explanatoryVariable$Answerer_MaxDwnVotes = explanatoryVariable$Answerer_MaxDwnVotes + 1
explanatoryVariable$Answerer_VarianceDwnVotes = explanatoryVariable$Answerer_VarianceDwnVotes + 1

explanatoryVariable$A_Score_Updated = explanatoryVariable$A_Score_Updated + 1


#### PQL is a flexible technique that can deal with non-normal data, unbalanced design, and crossed random effects.
## using poisson familly     
A_PQL0 <- glmmPQL(A_Score_Updated ~ A_NoOfBoldTags + A_length+ A_NoOfCodeSnippets +A_NoOfComments+ A_NoOfEditOrRevision+ 
                      A_LinkTags + A_LinkTags + A_ItalicTags + A_TotalCodeLength + Answerer_ReputationScore +
                      Answerer_Upvotes + Answerer_DownVotes+ Answerer_PostedQuestions+Answerer_PostedAnswers+Answerer_NoOfRevision+Answerer_NoOfRevisionInAnswer+
                      A_CodeRatioPercentage + Answerer_25PercentileVotes +
                      Answerer_MinVotes +   Answerer_MeanVotes + Answerer_MedianVotes + Answerer_MaxVotes + Answerer_VarianceVotes + 
                      Answerer_25PercentileDwnVotes + Answerer_MinDwnVotes + Answerer_MeanDwnVotes + Answerer_MedianDwnVotes + Answerer_MaxDwnVotes
                  + Answerer_VarianceDwnVotes ,
                  ~  1 | A_NoOfComments/A_TotalCodeLength, 
                  family = poisson(link = "log"), data = explanatoryVariable, verbose = FALSE)

## using gaussian familly     
A_PQL1 <- glmmPQL(A_Score_Updated ~ A_NoOfBoldTags + A_length+ A_NoOfCodeSnippets +A_NoOfComments+ A_NoOfEditOrRevision+ 
                A_LinkTags + A_LinkTags + A_ItalicTags + A_TotalCodeLength + Answerer_ReputationScore +
               Answerer_Upvotes + Answerer_DownVotes+ Answerer_PostedQuestions+Answerer_PostedAnswers+Answerer_NoOfRevision+Answerer_NoOfRevisionInAnswer+
               A_CodeRatioPercentage + Answerer_25PercentileVotes +
               Answerer_MinVotes +   Answerer_MeanVotes + Answerer_MedianVotes + Answerer_MaxVotes + Answerer_VarianceVotes + 
               Answerer_25PercentileDwnVotes + Answerer_MinDwnVotes + Answerer_MeanDwnVotes + Answerer_MedianDwnVotes + Answerer_MaxDwnVotes
               + Answerer_VarianceDwnVotes ,
               ~  1 | A_NoOfComments/A_TotalCodeLength, 
               family = gaussian(link = "log"), data = explanatoryVariable, verbose = FALSE)
# PQL1, from this model and with this family we got 11 factors


#### After removing not statistically significant variables from the model
A_PQL2 <- glmmPQL(A_Score_Updated ~ A_length+ A_NoOfCodeSnippets +A_NoOfComments+ A_NoOfEditOrRevision+ 
                   A_LinkTags + A_LinkTags + A_TotalCodeLength + Answerer_ReputationScore +
                   Answerer_DownVotes+ Answerer_PostedQuestions+Answerer_PostedAnswers +
                   Answerer_NoOfRevisionInAnswer + 
                   Answerer_MinVotes +   Answerer_MeanVotes + Answerer_MaxVotes + Answerer_VarianceVotes + 
                   Answerer_25PercentileDwnVotes + Answerer_MinDwnVotes + Answerer_MaxDwnVotes
                   + Answerer_VarianceDwnVotes ,
                   ~  1 | A_NoOfComments/A_TotalCodeLength, 
                   family = gaussian(link = "log"), data = explanatoryVariable, verbose = FALSE)


summary(A_PQL1)

summary(A_PQL2)


Anova(A_PQL1)
Anova(A_PQL2)







