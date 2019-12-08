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
library(plotMCMC)


QuestionClonedFactorsPart1 = readr::read_csv("Data_CSV_CloneDetection/QuestionClonedFactorsData1.csv")
QuestionClonedFactorsPart2 = readr::read_csv("Data_CSV_CloneDetection/QuestionClonedFactorsData2.csv")

QuestionClonedFactors <- rbind(QuestionClonedFactorsPart1, QuestionClonedFactorsPart2)

Questions_Mix_Model_tbl =QuestionClonedFactorsPart1 #QuestionClonedFactors

ZeroVariance_Functions = readr::read_csv("Data_ExportedDatafromR_Zero_Variance/QuestionsExportZeroVarianceFunctions.csv")
ZeroVariance_Blocks = readr::read_csv("Data_ExportedDatafromR_Zero_Variance//QuestionsExportZeroVarianceGroupsBlocks.csv")

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



CleanQ_explanatoryVariable1 = Questions_Mix_Model_tbl %>%
    left_join(NoVarianceBlocks, by = c("CloneClassnumber"="CloneGroupType", "Grunality"="X2")) 
    #filter(Grunality == "Blocks") 

CleanQ_explanatoryVariable2 = Questions_Mix_Model_tbl %>%
    filter(Grunality == "Functions") %>%
    left_join(NoVarianceFunctions, by = c("CloneClassnumber"="CloneGroupType"))


CleanQ_explanatoryVariable1$CloneGroupVariance = CleanQ_explanatoryVariable1$CloneGroupVariance %>% replace_na(1)
CleanQ_explanatoryVariable2$CloneGroupVariance = CleanQ_explanatoryVariable2$CloneGroupVariance %>% replace_na(1)

CleanQ_explanatoryVariable1 %>%
    filter(CloneGroupVariance == 0) %>%
    select(CloneClassnumber, CloneGroupVariance)


Q_explanatoryVariable1 = CleanQ_explanatoryVariable1 %>%
    filter((CloneGroupVariance == 1) & (Grunality == "Blocks"))
    select(CleanQ_explanatoryVariable1, -c(CloneGroupVariance))
    
Q_explanatoryVariable2 = CleanQ_explanatoryVariable2 %>%
        filter((CloneGroupVariance == 1) & (Grunality == "Functions"))
    select(CleanQ_explanatoryVariable2, -c(CloneGroupVariance))
#bind both table after removing the zero variance
Q_explanatoryVariable <- rbind(Q_explanatoryVariable1, Q_explanatoryVariable2)

# backup 
Q_explanatoryVariable = Questions_Mix_Model_tbl
view(Questions_Mix_Model_tbl)
#%>%
#mutate(Q_Score_Updated = Q_Score) %>%
#dplyr::
#select(CodeLength,Q_NooffavouriteCount,Q_length,Q_CreateDate,Q_TtleLength,
# Q_NoofViewCount,Q_NoOfTags,Q_Score )

#CHECK DATA DISTRIBUTION
names(Q_explanatoryVariable)
skewness(Q_explanatoryVariable$Q_length)
kurtosis(Q_explanatoryVariable$Q_length)

plot(hist(Q_explanatoryVariable$Q_NoofViewCount)) #left Skewed
plot(hist(Q_explanatoryVariable$Q_NoOfFavCount)) #left Skewed
plot(hist(Q_explanatoryVariable$Asker_ReputationScore)) #left Skewed
plot(hist(Q_explanatoryVariable$Q_NoOfAnswers)) #left Skewed
plot(hist(Q_explanatoryVariable$Q_ItalicTags)) #left Skewed
plot(hist(Q_explanatoryVariable$Q_length)) #left Skewed
plot(hist(Q_explanatoryVariable$Q_TtleLength))  #Normally dis
plot(hist(Q_explanatoryVariable$Q_NoOfBoldTags)) #left Skewed
plot(hist(Q_explanatoryVariable$Q_NoOfComments)) # replace null value with 0
plot(hist(Q_explanatoryVariable$Q_LinkTags)) #left Skewed
plot(hist(Q_explanatoryVariable$Q_NoOfEditOrRevision)) #left Skewed
plot(hist(Q_explanatoryVariable$Q_CodeRatioPercentage)) #right skewed
plot(hist(Q_explanatoryVariable$Q_NoOfCodeSnippets)) #left Skewed
plot(hist(Q_explanatoryVariable$Q_NoOfTags)) #normally dis
plot(hist(Q_explanatoryVariable$Asker_PostedAnswers)) #left Skewed
plot(hist(Q_explanatoryVariable$Asker_PostedQuestions)) #left Skewed
plot(hist(Q_explanatoryVariable$Asker_DownVotes)) #left Skewed
plot(hist(Q_explanatoryVariable$Asker_Upvotes)) #left Skewed


####### if the data is not normally distributed then follow this procedure
#replace negative score with 0 because it throw error during the modeling. 
Q_explanatoryVariable = Q_explanatoryVariable %>%
    mutate(Q_Score_Updated = Q_Score) 
Q_explanatoryVariable$Q_Score_Updated <- 
    replace(Q_explanatoryVariable$Q_Score_Updated, which(Q_explanatoryVariable$Q_Score_Updated < 0), 0)

# increase by 1 in each column because with zero value the function can not be run it will provide error
Q_explanatoryVariable$Q_NoOfFavCount = Q_explanatoryVariable$Q_NoOfFavCount + 1
Q_explanatoryVariable$Q_TtleLength = Q_explanatoryVariable$Q_TtleLength + 1
Q_explanatoryVariable$Q_NoofViewCount = Q_explanatoryVariable$Q_NoofViewCount + 1

Q_explanatoryVariable$Q_NoOfBoldTags = Q_explanatoryVariable$Q_NoOfBoldTags + 1
Q_explanatoryVariable$Q_NoOfCodeSnippets = Q_explanatoryVariable$Q_NoOfCodeSnippets + 1
Q_explanatoryVariable$Q_NoOfComments = Q_explanatoryVariable$Q_NoOfComments + 1
Q_explanatoryVariable$Q_NoOfEditOrRevision = Q_explanatoryVariable$Q_NoOfEditOrRevision + 1
Q_explanatoryVariable$Asker_ReputationScore = Q_explanatoryVariable$Asker_ReputationScore + 1
Q_explanatoryVariable$Asker_Upvotes = Q_explanatoryVariable$Asker_Upvotes + 1
Q_explanatoryVariable$Asker_DownVotes = Q_explanatoryVariable$Asker_DownVotes + 1
Q_explanatoryVariable$Asker_PostedQuestions = Q_explanatoryVariable$Asker_PostedQuestions + 1
Q_explanatoryVariable$Asker_PostedAnswers = Q_explanatoryVariable$Asker_PostedAnswers + 1
Q_explanatoryVariable$Asker_NoOfRevision = Q_explanatoryVariable$Asker_NoOfRevision + 1
Q_explanatoryVariable$Asker_NoOfRevisionInQ = Q_explanatoryVariable$Asker_NoOfRevisionInQ + 1
Q_explanatoryVariable$Q_CodeRatioPercentage = Q_explanatoryVariable$Q_CodeRatioPercentage + 1
Q_explanatoryVariable$Q_LinkTags = Q_explanatoryVariable$Q_LinkTags + 1
Q_explanatoryVariable$Q_ItalicTags = Q_explanatoryVariable$Q_ItalicTags + 1
Q_explanatoryVariable$Q_TotalCodeLength = Q_explanatoryVariable$Q_TotalCodeLength + 1

Q_explanatoryVariable$Q_codeRatio = Q_explanatoryVariable$Q_codeRatio + 1
Q_explanatoryVariable$Asker_25PercentileVotes = Q_explanatoryVariable$Asker_25PercentileVotes + 1
Q_explanatoryVariable$Asker_MinVotes = Q_explanatoryVariable$Asker_MinVotes + 1
Q_explanatoryVariable$Asker_MeanVotes = Q_explanatoryVariable$Asker_MeanVotes + 1
Q_explanatoryVariable$Asker_MedianVotes = Q_explanatoryVariable$Asker_MedianVotes + 1
Q_explanatoryVariable$Asker_MaxVotes = Q_explanatoryVariable$Asker_MaxVotes + 1
Q_explanatoryVariable$Asker_VarianceVotes = Q_explanatoryVariable$Asker_VarianceVotes + 1

Q_explanatoryVariable$Asker_25PercentileDwnVotes = Q_explanatoryVariable$Asker_25PercentileDwnVotes + 1
Q_explanatoryVariable$Asker_MinDwnVotes = Q_explanatoryVariable$Asker_MinDwnVotes + 1
Q_explanatoryVariable$Asker_MeanDwnVotes = Q_explanatoryVariable$Asker_MeanDwnVotes + 1
Q_explanatoryVariable$Asker_MedianDwnVotes = Q_explanatoryVariable$Asker_MedianDwnVotes + 1
Q_explanatoryVariable$Asker_MaxDwnVotes = Q_explanatoryVariable$Asker_MaxDwnVotes + 1
Q_explanatoryVariable$Asker_VarianceDwnVotes = Q_explanatoryVariable$Asker_VarianceDwnVotes + 1
Q_explanatoryVariable$Q_Score_Updated = Q_explanatoryVariable$Q_Score_Updated + 1


Q_PQL0 <- glmmPQL(Q_Score_Updated ~ Q_NoOfComments + Q_length +  Q_NoofViewCount + Q_TtleLength + Q_NoOfFavCount +
                      Q_TotalCodeLength + Q_NoOfCodeSnippets +  Q_NoOfEditOrRevision + Q_LinkTags + Q_ItalicTags + 
                      Asker_Upvotes + Asker_DownVotes + Asker_PostedQuestions+ Asker_PostedAnswers + 
                      Asker_ReputationScore + Asker_Upvotes + Asker_DownVotes  + Q_NoOfBoldTags + 
                      Q_CodeRatioPercentage + Asker_MedianVotes +
                      Asker_MaxVotes + Asker_VarianceVotes + Asker_MinDwnVotes + Asker_MeanDwnVotes +
                      Asker_MedianDwnVotes + Asker_MaxDwnVotes + Asker_VarianceDwnVotes + Asker_25PercentileVotes +
                      Asker_25PercentileDwnVotes,
                  ~ 1 | Q_NoOfComments/Q_TtleLength, 
                  family = gaussian(link = "log"), data = Q_explanatoryVariable, verbose = FALSE)
summary(Q_PQL0)
Anova(Q_PQL0)

# after removing more not statistically significant variables
Q_PQL1 <- glmmPQL(Q_Score_Updated ~ Q_NoOfFavCount + Q_TtleLength + Q_NoofViewCount + Q_NoOfAnswers + 
                      Asker_ReputationScore + Asker_Upvotes + Asker_DownVotes + Asker_PostedQuestions + 
                      Asker_PostedAnswers + Q_NoOfTags + Q_CodeRatioPercentage + Q_LinkTags + Q_ItalicTags +
                      Q_TotalCodeLength + Q_NoOfBoldTags + Q_NoOfCodeSnippets + Q_NoOfComments +
                      Q_NoOfEditOrRevision + Asker_NoOfRevision + Asker_NoOfRevisionInQ + Q_NoOfFavCount,
                  ~ 1 | Q_NoOfComments/Q_TtleLength, 
                  family = gaussian(link = "log"), data = Q_explanatoryVariable, verbose = FALSE)
summary(Q_PQL1)
Anova(Q_PQL1)

# after removing more non statistically significant variables
Q_PQL2 <- glmmPQL(Q_Score_Updated ~  Q_NoofViewCount + Q_NoOfFavCount +
                      Q_NoOfCodeSnippets +  Q_NoOfEditOrRevision + Q_LinkTags 
                       + Asker_DownVotes + Asker_PostedQuestions+ Asker_PostedAnswers + 
                      Asker_ReputationScore + Q_NoOfTags +
                      Q_CodeRatioPercentage + Q_NoOfAnswers,
                  ~ 1 | Q_NoOfComments/Q_TtleLength, 
                  family = gaussian(link = "log"), data = Q_explanatoryVariable, verbose = FALSE)


summary(Q_PQL2)
Anova(Q_PQL2)





