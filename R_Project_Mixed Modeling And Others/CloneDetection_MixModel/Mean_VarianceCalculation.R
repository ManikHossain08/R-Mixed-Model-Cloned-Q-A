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
#install.packages("entropy")
library(entropy)

#############------------------Answers-------------##########
Mix_Model_csv_tbl = readr::read_csv("Data_CSV_CloneDetection/A_Data_Mix_Model_CloneDetection.csv")

#Mix_Model_csv_tbl = readr::read_csv("/Users/manikhossain/Downloads/Code Clone Paper/Data_Mix_Model_CloneDetection.csv") 
# UPPER LINE WILL ALSO BE WORKED BUT I KEEP THE PROJECTS FILE IN THE SAME FOLDER
# Example, For retrieving data for functions and cloneType-2


Mix_Model_csv_tbl %>%
    dplyr::select( CloneClassType, CreateDate, Scores, Granularity) %>%
    filter((CloneClassType == 2) & (Granularity == 'Functions'))

#Example for cloning changing the group and show the data
mixData = pull(Mix_Model_csv_tbl %>%
    filter((CloneClassType == 7) & (Granularity == 'Functions')) %>%
    select(Scores))
print(mixData)

#https://rstudio-pubs-static.s3.amazonaws.com/455435_30729e265f7a4d049400d03a18e218db.html
#compute Shannon entropy using functions
entropy <- function(target) {
    freq <- table(target)/length(target)
    # vectorize
    vec <- as.data.frame(freq)[,2]
    #drop 0 to avoid NaN resulting from log2
    vec<-vec[vec>0]
    #compute entropy
    -sum(vec * log2(vec))
}


shannonEntropy <- function(counts.table, log.base = getOption("GeneFamilies.entropy.log.base",
                                                              base::exp(1))) {
    if (length(counts.table) <= 1)
        return(0)
    c.t.s <- sum(counts.table)
    -sum(sapply(counts.table, function(x) x/c.t.s * log(x/c.t.s, base = log.base)))/log(length(counts.table),
                                                                                        base = log.base)
}


##### ------- Answers Functions Clones ---------- #######
distinctClsFunc <- count(Mix_Model_csv_tbl %>%
                             filter((Granularity == 'Functions')) %>%
                             distinct(CloneClassType)) 
print(distinctClsFunc)
str(abc)
abc = quantile(mixData)
boxplot(abc,
        main = "Mean ozone in parts per billion at Roosevelt Island",
        xlab = "Parts Per Billion",
        ylab = "Ozone",
        col = "orange",
        border = "brown",
        horizontal = FALSE,
        notch = FALSE
        #xlim = c(0, 5000), ylim = c(0, 5000)
)


dat=matrix(nrow=2804,ncol=14)
dat=data.frame(dat)
for (i in 1:2804){
    scoresInVector = pull(Mix_Model_csv_tbl %>%
                              filter((CloneClassType == i) & (Granularity == 'Functions')) %>%
                              dplyr::select(Scores)) 
    print(length(scoresInVector))
    dat[i,1]<- i
    dat[i,2]<- 'Functions'
    dat[i,3]<- format(round( as.numeric(format(quantile(scoresInVector,c(0.25)) , scientific=FALSE)), 2), nsmall = 2)  
    dat[i,4]<- format(round( as.numeric(format(max(scoresInVector) , scientific=FALSE)), 2), nsmall = 2)  
    dat[i,5]<- format(round( as.numeric(format(min(scoresInVector) , scientific=FALSE)), 2), nsmall = 2) 
    dat[i,6]<- format(round( as.numeric(format(mean(scoresInVector) , scientific=FALSE)), 2), nsmall = 2) 
    dat[i,7]<- format(round( as.numeric(format(median(scoresInVector) , scientific=FALSE)), 2), nsmall = 2)   
    # if(i == 553) {
    #     dat[i,8]<- 0 
    #     print(i)
    # }
    # else 
    dat[i,8]<- format(round( as.numeric(format(var(scoresInVector) , scientific=FALSE)), 2), nsmall = 2) 
    dat[i,9]<- as.numeric(format(sqrt(var(scoresInVector)), scientific=FALSE));  
    dat[i,10]<- length(scoresInVector)
    dat[i,11]<- entropy(scoresInVector)
    dat[i,12]<- shannonEntropy (scoresInVector,2)
    
} 




extractdataZeroVariance = dat 
    #%>%
    #filter((X8=="0.00")) 
    # %>% count() = 835
extractdataZeroVariance %>% write_csv("Data_ExportedDatafromR_Zero_Variance/AnswersExportZeroVarianceFunctions.csv")

AnswerFunctionsDF = dat %>% 
        rename(
            CloneGroupType = X1,
            CloneGroupMax = X4,
            CloneGroupMin = X5,
            CloneGroupVariance = X8,
            EachGroup_SD = X9,
            EachGroup_Mean = X6,
            EachGroup_Median = X7,
            EachGroup_25Percentile = X3,
            ShanonEntropy = X11,
            NorShanonEntropy = X12
        ) 
AnswerFunctionsDF %>% write_csv("Data_ExportedDatafromR_Zero_Variance/AnswersExportZeroVarianceGroups.csv")

boxplot(as.numeric(AnswerFunctionsDF$EachGroup_25Percentile), 
        main = "Answers Functions: Boxlpot with each group 25% (PERCENTILE)",xlab = "25Percentile")
boxplot(as.numeric(AnswerFunctionsDF$CloneGroupMin), 
        main = "Answers Functions: Boxlpot with each group MIN",xlab = "MIN")
boxplot(as.numeric(AnswerFunctionsDF$CloneGroupMax), 
        main = "Answers Functions: Boxlpot with each group MAX",xlab = "MAX")
boxplot(as.numeric(AnswerFunctionsDF$EachGroup_Mean), 
        main = "Answers Functions: Boxlpot with each group MEAN",xlab = "MEAN")
boxplot(as.numeric(AnswerFunctionsDF$EachGroup_Median), 
        main = "Answers Functions: Boxlpot with each group MEDIAN",xlab = "MEDIAN")
boxplot(as.numeric(AnswerFunctionsDF$CloneGroupVariance), 
        main = "Answers Functions: Boxlpot with 2,804 group's VARIANCE",xlab = "2,804 group's", ylab = "Each Group votes VARIANCE")
boxplot(as.numeric(AnswerFunctionsDF$EachGroup_SD), 
        main = "Answers Functions: Boxlpot with each group STANDARD DAVIATION",xlab = "STANDARD DAVIATION")
boxplot(as.numeric(AnswerFunctionsDF$ShanonEntropy), 
        main = "Answer Functions: Boxlpot with 2,804 Group's Shannon Normalized Entropy",
        xlab = "2,804 Groups", ylab = "Each Group's Entropy of Votes ")
boxplot(as.numeric(AnswerFunctionsDF$NorShanonEntropy), 
        main = "Answer Functions: Boxlpot with 2,804 Group's Variance with Normalized Shannon Entropy",
        xlab = "2,804 Groups", ylab = "Each Group Variance (Normalized Shannon Entropy) of Votes ")

boxplot(as.numeric(AnswerFunctionsDF$CloneGroupVariance), 
        main = " Boxlpot with 2,804 Group's Vote Variance",
        xlab = "2,804 Groups", ylab = "Each Group Vote Variances ")



##### ------- Answers Blocks Clones  ---------- #######
distinctClsFunc <- count(Mix_Model_csv_tbl %>%
                             filter((Granularity == 'Blocks')) %>%
                             distinct(CloneClassType)) 
print(distinctClsFunc)


dat=matrix(nrow=852,ncol=11)
dat=data.frame(dat)
for (i in 1:852){
    scoresInVector = pull(Mix_Model_csv_tbl %>%
                              filter((CloneClassType == i) & (Granularity == 'Blocks')) %>%
                              dplyr::select(Scores)) 
    print(length(scoresInVector))
    dat[i,1]<- i
    dat[i,2]<- 'Blocks'
    dat[i,3]<- format(quantile(scoresInVector,c(0.25)) , scientific=FALSE); 
    dat[i,4]<- format(min(scoresInVector), scientific=FALSE); 
    dat[i,5]<- format(max(scoresInVector), scientific=FALSE); 
    dat[i,6]<- format(mean(scoresInVector), scientific=FALSE);  
    dat[i,7]<- format(median(scoresInVector), scientific=FALSE);  
    # if(format(var(scoresInVector), scientific=FALSE) >= 10000) {
    #     dat[i,8]<- 0 
    #     print(i)
    # }
    # else
    dat[i,8]<- format(var(scoresInVector), scientific=FALSE); 
    dat[i,9]<- format(sqrt(var(scoresInVector)), scientific=FALSE);  
    dat[i,10]<- length(scoresInVector)
    dat[i,11]<- shannonEntropy (scoresInVector,2)
    
} 

extractdataZeroVariance = dat %>%
    filter((X8=="0")) #184 with zero variance
extractdataZeroVariance %>% write_csv("Data_ExportedDatafromR_Zero_Variance/ExportZeroVarianceGroupsBlocks.csv")


AnswerFunctionsDF = dat %>% 
    rename(
        CloneGroupType = X1,
        CloneGroupMin = X4,
        CloneGroupMax = X5,
        CloneGroupVariance = X8,
        EachGroup_SD = X9,
        EachGroup_Mean = X6,
        EachGroup_Median = X7,
        EachGroup_25Percentile = X3,
        ShanonEntropy = X11
    ) 
AnswerFunctionsDF %>% write_csv("Data_ExportedDatafromR_Zero_Variance/AnswersExportZeroVarianceGroupsBlocks.csv")

boxplot(as.numeric(AnswerFunctionsDF$EachGroup_25Percentile), 
        main = "Answers Blocks: Boxlpot with each group 25% (PERCENTILE)",xlab = "25Percentile")
boxplot(as.numeric(AnswerFunctionsDF$CloneGroupMin), 
        main = "Answers Blocks: Boxlpot with each group MIN",xlab = "MIN")
boxplot(as.numeric(AnswerFunctionsDF$CloneGroupMax), 
        main = "Answers Blocks: Boxlpot with each group MAX",xlab = "MAX")
boxplot(as.numeric(AnswerFunctionsDF$EachGroup_Mean), 
        main = "Answers Blocks: Boxlpot with each group MEAN",xlab = "MEAN")
boxplot(as.numeric(AnswerFunctionsDF$EachGroup_Median), 
        main = "Answers Blocks: Boxlpot with each group MEDIAN",xlab = "MEDIAN")
boxplot(as.numeric(AnswerFunctionsDF$CloneGroupVariance), 
        main = "Answers Blocks: Boxlpot with 852 clone group's VARIANCE",xlab = "VARIANCE")
boxplot(as.numeric(AnswerFunctionsDF$EachGroup_SD), 
        main = "Answers Blocks: Boxlpot with each group STANDARD DAVIATION",xlab = "STANDARD DAVIATION")
boxplot(as.numeric(AnswerFunctionsDF$ShanonEntropy), 
        main = "Answer Blocks: Boxlpot with 852 Group's Normalized Shannon  Entropy",
        xlab = "852 Groups", ylab = "Each Group's Entropy of Votes ")






##### ------- Questions Blocks Clones  ---------- #######
Q_Mix_Model_csv_tbl = readr::read_csv("Data_CSV_CloneDetection/Q_Data_Mix_Model_CloneDetection.csv")
distinctClsFunc <- count(Q_Mix_Model_csv_tbl %>%
                             filter((Granularity == 'Blocks')) %>%
                             distinct(CloneClassType)) 
print(distinctClsFunc)


dat=matrix(nrow=2185,ncol=11)
dat=data.frame(dat)
for (i in 1:2185){
    scoresInVector = pull(Q_Mix_Model_csv_tbl %>%
                              filter((CloneClassType == i) & (Granularity == 'Blocks')) %>%
                              dplyr::select(Scores)) 
    #print(length(scoresInVector))
    dat[i,1]<- i
    dat[i,2]<- 'Blocks'
    dat[i,3]<- format(quantile(scoresInVector,c(0.25)) , scientific=FALSE); 
    dat[i,4]<- format(min(scoresInVector), scientific=FALSE); 
    dat[i,5]<- format(max(scoresInVector), scientific=FALSE); 
    dat[i,6]<- format(mean(scoresInVector), scientific=FALSE);  
    dat[i,7]<- format(median(scoresInVector), scientific=FALSE);  
    variance = as.numeric(format(var(scoresInVector), scientific=FALSE))
    # if(variance >= 5000) {
    #     dat[i,8]<- 0
    #     print(log2(variance))
    #     print(variance)
    # }
    # else
    # if(variance == 0) {
    #     dat[i,8]<- 0
    #     print(log2(variance))
    #     print(variance)
    # }
    # else
    #     dat[i,8]<- log2(variance); 
    dat[i,8]<- format(var(scoresInVector), scientific=FALSE); 
    dat[i,9]<- format(sqrt(var(scoresInVector)), scientific=FALSE);  
    dat[i,10]<- length(scoresInVector)
    dat[i,11]<- shannonEntropy (scoresInVector,2)
    
} 


AnswerFunctionsDF = dat %>% 
    rename(
        CloneGroupType = X1,
        CloneGroupMin = X4,
        CloneGroupMax = X5,
        CloneGroupVariance = X8,
        EachGroup_SD = X9,
        EachGroup_Mean = X6,
        EachGroup_Median = X7,
        EachGroup_25Percentile = X3,
        ShannonEntropy = X11
    ) 

boxplot(as.numeric(AnswerFunctionsDF$EachGroup_25Percentile), 
        main = "Questions Blocks: Boxlpot with each group 25% (PERCENTILE)",xlab = "25Percentile")
boxplot(as.numeric(AnswerFunctionsDF$CloneGroupMin), 
        main = "Questions Blocks: Boxlpot with each group MIN",xlab = "MIN")
boxplot(as.numeric(AnswerFunctionsDF$CloneGroupMax), 
        main = "Questions Blocks: Boxlpot with each group MAX",xlab = "MAX")
boxplot(as.numeric(AnswerFunctionsDF$EachGroup_Mean), 
        main = "Questions Blocks: Boxlpot with each group MEAN",xlab = "MEAN")
boxplot(as.numeric(AnswerFunctionsDF$EachGroup_Median), 
        main = "Questions Blocks: Boxlpot with each group MEDIAN",xlab = "MEDIAN")
boxplot(as.numeric(AnswerFunctionsDF$CloneGroupVariance), 
        main = "Questions Blocks: Boxlpot with 2,185 group's using variance) ",xlab = "2,185 groups",ylab= "Each Group votes VARIANCE")
boxplot(as.numeric(AnswerFunctionsDF$EachGroup_SD), 
        main = "Question Blocks: Boxlpot with each group STANDARD DAVIATION",xlab = "STANDARD DAVIATION")
boxplot(as.numeric(AnswerFunctionsDF$ShannonEntropy), 
        main = "Question Blocks: Boxlpot with 2,185 groups using Shannon Normalized Entropy ",
        xlab = "2,185 Groups",ylab= "Each Group's Entropy of Votes")




##### ------- Questions Functions Clones  ---------- #######
distinctClsFunc <- count(Q_Mix_Model_csv_tbl %>%
                             filter((Granularity == 'Functions')) %>%
                             distinct(CloneClassType)) 
print(distinctClsFunc)

dat=matrix(nrow=16516,ncol=11)
dat=data.frame(dat)
for (i in 1:16516){
    scoresInVector = pull(Q_Mix_Model_csv_tbl %>%
                              filter((CloneClassType == i) & (Granularity == 'Functions')) %>%
                              dplyr::select(Scores)) 
    #print(length(scoresInVector))
    dat[i,1]<- i
    dat[i,2]<- 'Functions'
    dat[i,3]<- format(quantile(scoresInVector,c(0.25)) , scientific=FALSE); 
    dat[i,4]<- format(min(scoresInVector), scientific=FALSE); 
    dat[i,5]<- format(max(scoresInVector), scientific=FALSE); 
    dat[i,6]<- format(mean(scoresInVector), scientific=FALSE);  
    dat[i,7]<- format(median(scoresInVector), scientific=FALSE);  
    variance = as.numeric(format(var(scoresInVector), scientific=FALSE))
    if(variance >= 5000) {
        dat[i,8]<- 0 
        print(variance)
    }
    else
    dat[i,8]<- variance; 
    dat[i,9]<- format(sqrt(var(scoresInVector)), scientific=FALSE);  
    dat[i,10]<- length(scoresInVector)
    dat[i,11]<- shannonEntropy (scoresInVector,2)
    
} 

AnswerFunctionsDF = dat %>% 
    rename(
        CloneGroupType = X1,
        CloneGroupMin = X4,
        CloneGroupMax = X5,
        CloneGroupVariance = X8,
        EachGroup_SD = X9,
        EachGroup_Mean = X6,
        EachGroup_Median = X7,
        EachGroup_25Percentile = X3,
        ShannonEntropy = X11
    ) 


boxplot(as.numeric(AnswerFunctionsDF$EachGroup_25Percentile), 
        main = "Questions Functions: Boxlpot with each group 25% (PERCENTILE)",xlab = "25Percentile")
boxplot(as.numeric(AnswerFunctionsDF$CloneGroupMin), 
        main = "Questions Functions: Boxlpot with each group MIN",xlab = "MIN")
boxplot(as.numeric(AnswerFunctionsDF$CloneGroupMax), 
        main = "Questions Functions: Boxlpot with each group MAX",xlab = "MAX")
boxplot(as.numeric(AnswerFunctionsDF$EachGroup_Mean), 
        main = "Questions Functions: Boxlpot with each group MEAN",xlab = "MEAN")
boxplot(as.numeric(AnswerFunctionsDF$EachGroup_Median), 
        main = "Questions Functions: Boxlpot with each group MEDIAN",xlab = "MEDIAN")
boxplot(round(as.numeric(AnswerFunctionsDFaa$CloneGroupVariance),digits=2), 
        main = "Questions Functions: Boxlpot with 16,516 group's VARIANCE",xlab = "16,516 groups",ylab= "Each Group votes VARIANCE")
boxplot(as.numeric(AnswerFunctionsDF$EachGroup_SD), 
        main = "Questions Functions: Boxlpot with each group STANDARD DAVIATION",xlab = "STANDARD DAVIATION")
boxplot(as.numeric(AnswerFunctionsDF$X10), 
        main = "Questions Functions: Boxlpot with each group STANDARD DAVIATION",xlab = "STANDARD DAVIATION")
boxplot(as.numeric(AnswerFunctionsDF$ShannonEntropy), 
        main = "Question Functions: Boxlpot with 16,516 groups variance using Shannon Normalized Entropy ",
        xlab = "16,516 Groups",ylab= "Each Group vote variance in Shannon Normalized Entropy")

boxplot(as.numeric(AnswerFunctionsDF$CloneGroupVariance), 
        main = "Question Functions: Boxlpot with 16,516 Group's Vote Variance",
        xlab = "16,516 Groups", ylab = "Each Group Vote Variances ")




