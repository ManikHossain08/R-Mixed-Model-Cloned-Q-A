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

#############------------------Answers-------------##########
Mix_Model_csv_tbl = readr::read_csv("CloneDetection/A_Data_Mix_Model_CloneDetection.csv")
#Mix_Model_csv_tbl = readr::read_csv("/Users/manikhossain/Downloads/Code Clone Paper/Data_Mix_Model_CloneDetection.csv") 
# UPPER LINE WILL ALSO BE WORKED BUT I KEEP THE PROJECTS FILE IN THE SAME FOLDER
# Example, For retrieving data for functions and cloneType-2
Mix_Model_csv_tbl %>%
    dplyr::select( CloneClassType, CreateDate, Scores, Granularity) %>%
    filter((CloneClassType == 2) & (Granularity == 'Functions'))

#Example for cloning changing the group and show the data
mixData = Mix_Model_csv_tbl %>%
    filter((CloneClassType == 7) & (Granularity == 'Functions')) %>%
    select(Scores)
print(mixData)

# For functions and cloneType-2
scoresForType_2 = pull(Mix_Model_csv_tbl %>%
                           filter((CloneClassType == 1) & (Granularity == 'Functions')) %>%
                           select(Scores)) 
print(scoresForType_2) 
print(length(scoresForType_2))
#calculation for group-2
CloneType2.quantile = quantile(scoresForType_2)
CloneType2.25Percentile = quantile(scoresForType_2, c(0.25))
CloneType2.min = min(scoresForType_2)
CloneType2.max = max(scoresForType_2)
CloneType2.Mean = mean(scoresForType_2)
CloneType2.Median = median(scoresForType_2)
CloneType2.Varience = var(scoresForType_2)
CloneType2.SD = sqrt(var(scoresForType_2))

boxplot(CloneType2.Mean)
boxplot(CloneType2.Median)
boxplot(CloneType2.Varience)
boxplot(CloneType2.25Percentile)
boxplot(CloneType2.quantile)

# For functions and cloneType-3
scoresForType_3 = pull(Mix_Model_csv_tbl %>%
                           filter((CloneClassType == 3) & (Granularity == 'Functions')) %>%
                           select(Scores)) 
print(scoresForType_3) 
#calculation for group-3
CloneType3.quantile = quantile(scoresForType_3)
CloneType3.25Percentile = quantile(scoresForType_3,c(0.25))
CloneType3.min = min(scoresForType_3)
CloneType3.max = max(scoresForType_3)
CloneType3.Mean = mean(scoresForType_3)
CloneType3.Median = median(scoresForType_3)
CloneType3.Varience = var(scoresForType_3)
CloneType3.SD = sqrt(var(scoresForType_3))

boxplot(CloneType3.Mean)
boxplot(CloneType3.Median)
boxplot(CloneType3.Varience)
boxplot(CloneType3.25Percentile)
boxplot(CloneType3.quantile)


##### --------------------------------####

# For functions and cloneType-3, directly converted into vector.
scoresForType_4 = pull(Mix_Model_csv_tbl %>%
                           filter((CloneClassType == 7) & (Granularity == 'Functions')) %>%
                           select(Scores))
print(scoresForType_4)    
#calculation for group-7
CloneType4.quantile = quantile(scoresForType_4)
CloneType4.25Percentile = quantile(scoresForType_4,c(0.25))
CloneType4.min = min(scoresForType_4)
CloneType4.max = max(scoresForType_4)
CloneType4.Mean = mean(scoresForType_4)
CloneType4.Median = median(scoresForType_4)
CloneType4.Varience = var(scoresForType_4)
CloneType4.SD = sqrt(var(scoresForType_4))

boxplot(CloneType4.Mean)
boxplot(CloneType4.Median)
boxplot(CloneType4.Varience)
boxplot(CloneType4.25Percentile)
boxplot(CloneType4.quantile)


#Experiments
boxplot(CloneType2.quantile,
        main = "Mean ozone in parts per billion at Roosevelt Island",
        xlab = "Parts Per Billion",
        ylab = "Ozone",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)



#######-----------------------Questions-----------------########
Q_Mix_Model_csv_tbl = readr::read_csv("CloneDetection/Q_Data_Mix_Model_CloneDetection.csv")
# Example, For retrieving data for functions and cloneType-2
Q_Mix_Model_csv_tbl %>%
    select(AnswertId, CloneClassType, CreateDate, Scores, Granularity) %>%
    filter((CloneClassType == 2) & (Granularity == 'Functions'))

#Example for cloning changing the group and show the data
Q_mixData = Q_Mix_Model_csv_tbl %>%
    filter((CloneClassType == 7) & (Granularity == 'Functions')) %>%
    select(Scores)
print(Q_mixData)

# For functions and cloneType-2
Q_scoresForType_2 = pull(Q_Mix_Model_csv_tbl %>%
                             filter((CloneClassType == 2) & (Granularity == 'Functions')) %>%
                             select(Scores)) 
print(Q_scoresForType_2) 
print(length(Q_scoresForType_2))
#calculation for group-2
CloneType2.quantile = quantile(Q_scoresForType_2)
CloneType2.25Percentile = quantile(Q_scoresForType_2, c(0.25))
CloneType2.min = min(Q_scoresForType_2)
CloneType2.max = max(Q_scoresForType_2)
CloneType2.Mean = mean(Q_scoresForType_2)
CloneType2.Median = median(Q_scoresForType_2)
CloneType2.Varience = var(Q_scoresForType_2)
CloneType2.SD = sqrt(var(Q_scoresForType_2))

boxplot(CloneType2.Mean)
boxplot(CloneType2.Median)
boxplot(CloneType2.Varience)
boxplot(CloneType2.25Percentile)
boxplot(CloneType2.quantile)

# For functions and cloneType-3
Q_scoresForType_3 = pull(Q_Mix_Model_csv_tbl %>%
                             filter((CloneClassType == 5) & (Granularity == 'Functions')) %>%
                             select(Scores)) 
print(Q_scoresForType_3) 
#calculation for group-3
CloneType3.quantile = quantile(Q_scoresForType_3)
CloneType3.25Percentile = quantile(Q_scoresForType_3,c(0.25))
CloneType3.min = min(Q_scoresForType_3)
CloneType3.max = max(Q_scoresForType_3)
CloneType3.Mean = mean(Q_scoresForType_3)
CloneType3.Median = median(Q_scoresForType_3)
CloneType3.Varience = var(Q_scoresForType_3)
CloneType3.SD = sqrt(var(Q_scoresForType_3))

boxplot(CloneType3.Mean)
boxplot(CloneType3.Median)
boxplot(CloneType3.Varience)
boxplot(CloneType3.25Percentile)
boxplot(CloneType3.quantile)


##### --------------------------------####

# For functions and cloneType-3, directly converted into vector.
Q_scoresForType_4 = pull(Q_Mix_Model_csv_tbl %>%
                             filter((CloneClassType == 30) & (Granularity == 'Functions')) %>%
                             select(Scores))
print(Q_scoresForType_4)    
#calculation for group-7
CloneType4.quantile = quantile(Q_scoresForType_4)
CloneType4.25Percentile = quantile(Q_scoresForType_4,c(0.25))
CloneType4.min = min(Q_scoresForType_4)
CloneType4.max = max(Q_scoresForType_4)
CloneType4.Mean = mean(Q_scoresForType_4)
CloneType4.Median = median(Q_scoresForType_4)
CloneType4.Varience = var(Q_scoresForType_4)
CloneType4.SD = sqrt(var(Q_scoresForType_4))

boxplot(CloneType4.Mean)
boxplot(CloneType4.Median)
boxplot(CloneType4.Varience)
boxplot(CloneType4.25Percentile)
boxplot(CloneType4.quantile)


#Experiments
boxplot(CloneType2.quantile,
        main = "Mean ozone in parts per billion at Roosevelt Island",
        xlab = "Parts Per Billion",
        ylab = "Ozone",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)


##### ------- Answers Functions Clones ---------- #######
distinctClsFunc <- count(Mix_Model_csv_tbl %>%
                             filter((Granularity == 'Functions')) %>%
                             distinct(CloneClassType)) 
print(distinctClsFunc)


dat=matrix(nrow=2804,ncol=10)
dat=data.frame(dat)
for (i in 1:2804){
    scoresInVector = pull(Mix_Model_csv_tbl %>%
                              filter((CloneClassType == i) & (Granularity == 'Functions')) %>%
                              dplyr::select(Scores)) 
    print(length(scoresInVector))
    dat[i,1]<- i
    dat[i,2]<- 'Functions'
    dat[i,3]<- as.numeric(format(quantile(scoresInVector,c(0.25)) , scientific=FALSE)); 
    dat[i,4]<- as.numeric(format(min(scoresInVector), scientific=FALSE)); 
    dat[i,5]<- as.numeric(format(max(scoresInVector), scientific=FALSE)); 
    dat[i,6]<- as.numeric(format(mean(scoresInVector), scientific=FALSE));  
    dat[i,7]<- as.numeric(format(median(scoresInVector), scientific=FALSE));  
    dat[i,8]<- as.numeric(format(var(scoresInVector), scientific=FALSE)); 
    dat[i,9]<- as.numeric(format(sqrt(var(scoresInVector)), scientific=FALSE));  
    dat[i,10]<-length(scoresInVector)
} 

# 7.2 export to CSV ----
dat %>% write_csv("CloneDetection/CalcAnswersFunction.csv")

AnswerFunctionsDF = dat %>% 
        rename(
            CloneGroupType = X1,
            CloneGroupMin = X4,
            CloneGroupMax = X5,
            CloneGroupVariance = X8,
            EachGroup_SD = X9,
            EachGroup_Mean = X6,
            EachGroup_Median = X7,
            EachGroup_25Percentile = X3
        ) %>%
        arrange((CloneGroupVariance)) 

#Experiments

AnswerFunctionsCalcTerms =pull(AnswerFunctionsDF %>%
                                    #filter((CloneClassType == 30) & (Granularity == 'Functions')) %>%
                                    select(CloneGroupVariance)) 
                                    




boxplot(AnswerFunctionsCalcTerms,
        main = "Mean ozone in parts per billion at Roosevelt Island",
        xlab = "Parts Per Billion",
        ylab = "Ozone",
        col = "orange",
        border = "brown",
        horizontal = FALSE,
        notch = TRUE
)
    
#filter((X9 > 0)) %>%
#slice(2:1000) 

# Store the graph
box_plot <- ggplot(funcvariance, aes(x = CloneGroupType, y = EachGroup_Variance))
# Add the geometric object box plot
box_plot +
    geom_boxplot()

# Store the graph
box_plot <- ggplot(funcvariance, aes(x = CloneGroupType, y = EachGroup_Mean))
# Add the geometric object box plot
box_plot +
    geom_boxplot()

# Store the graph
box_plot <- ggplot(funcvariance, aes(x = CloneGroupType, y = EachGroup_Median))
# Add the geometric object box plot
box_plot +
    geom_boxplot()

# Store the graph
box_plot <- ggplot(funcvariance, aes(x = CloneGroupType, y = EachGroup_25Percentile))
# Add the geometric object box plot
box_plot +
    geom_boxplot()






##### ------- Answers Blocks Clones  ---------- #######
distinctClsFunc <- count(Mix_Model_csv_tbl %>%
                             filter((Granularity == 'Blocks')) %>%
                             distinct(CloneClassType)) 
print(distinctClsFunc)


dat=matrix(nrow=852,ncol=10)
dat=data.frame(dat)
for (i in 1:852){
    scoresInVector = pull(Mix_Model_csv_tbl %>%
                              filter((CloneClassType == i) & (Granularity == 'Blocks')) %>%
                              select(Scores)) 
    print(length(scoresInVector))
    dat[i,1]<- i
    dat[i,2]<- 'Blocks'
    dat[i,3]<- format(quantile(scoresInVector,c(0.25)) , scientific=FALSE); 
    dat[i,4]<- format(min(scoresInVector), scientific=FALSE); 
    dat[i,5]<- format(max(scoresInVector), scientific=FALSE); 
    dat[i,6]<- format(mean(scoresInVector), scientific=FALSE);  
    dat[i,7]<- format(median(scoresInVector), scientific=FALSE);  
    dat[i,8]<- format(var(scoresInVector), scientific=FALSE); 
    dat[i,9]<- format(sqrt(var(scoresInVector)), scientific=FALSE);  
    dat[i,10]<- length(scoresInVector)
    
} 

# 7.2 export to CSV ----
dat %>% write_csv("CloneDetection/CalcAnswersBlocks.csv")


funcvariance = dat %>%
    rename(
        CloneGroupType = X1,
        EachGroup_Variance = X8 ,
        EachGroup_Mean = X6,
        EachGroup_Median = X7,
        EachGroup_25Percentile = X3
    ) #%>%
#filter((X9 > 0)) %>%
#slice(2:1000) 

# Store the graph
box_plot <- ggplot(funcvariance, aes(x = CloneGroupType, y = EachGroup_Variance))
# Add the geometric object box plot
box_plot +
    geom_boxplot()

# Store the graph
box_plot <- ggplot(funcvariance, aes(x = CloneGroupType, y = EachGroup_Mean))
# Add the geometric object box plot
box_plot +
    geom_boxplot()

# Store the graph
box_plot <- ggplot(funcvariance, aes(x = CloneGroupType, y = EachGroup_Median))
# Add the geometric object box plot
box_plot +
    geom_boxplot()

# Store the graph
box_plot <- ggplot(funcvariance, aes(x = CloneGroupType, y = EachGroup_25Percentile))
# Add the geometric object box plot
box_plot +
    geom_boxplot()






##### ------- Questions Blocks Clones  ---------- #######
distinctClsFunc <- count(Q_Mix_Model_csv_tbl %>%
                             filter((Granularity == 'Blocks')) %>%
                             distinct(CloneClassType)) 
print(distinctClsFunc)


dat=matrix(nrow=2185,ncol=10)
dat=data.frame(dat)
for (i in 1:2185){
    scoresInVector = pull(Q_Mix_Model_csv_tbl %>%
                              filter((CloneClassType == i) & (Granularity == 'Blocks')) %>%
                              select(Scores)) 
    print(length(scoresInVector))
    dat[i,1]<- i
    dat[i,2]<- 'Blocks'
    dat[i,3]<- format(quantile(scoresInVector,c(0.25)) , scientific=FALSE); 
    dat[i,4]<- format(min(scoresInVector), scientific=FALSE); 
    dat[i,5]<- format(max(scoresInVector), scientific=FALSE); 
    dat[i,6]<- format(mean(scoresInVector), scientific=FALSE);  
    dat[i,7]<- format(median(scoresInVector), scientific=FALSE);  
    dat[i,8]<- format(var(scoresInVector), scientific=FALSE); 
    dat[i,9]<- format(sqrt(var(scoresInVector)), scientific=FALSE);  
    dat[i,10]<- length(scoresInVector)
    
} 

# 7.2 export to CSV ----
dat %>% write_csv("CloneDetection/CalcQuestionsBlocks.csv")


funcvariance = dat %>%
    rename(
        CloneGroupType = X1,
        EachGroup_Variance = X8 ,
        EachGroup_Mean = X6,
        EachGroup_Median = X7,
        EachGroup_25Percentile = X3
    ) #%>%
#filter((X9 > 0)) %>%
#slice(2:1000) 

# Store the graph
box_plot <- ggplot(funcvariance, aes(x = CloneGroupType, y = EachGroup_Variance))
# Add the geometric object box plot
box_plot +
    geom_boxplot()

# Store the graph
box_plot <- ggplot(funcvariance, aes(x = CloneGroupType, y = EachGroup_Mean))
# Add the geometric object box plot
box_plot +
    geom_boxplot()

# Store the graph
box_plot <- ggplot(funcvariance, aes(x = CloneGroupType, y = EachGroup_Median))
# Add the geometric object box plot
box_plot +
    geom_boxplot()

# Store the graph
box_plot <- ggplot(funcvariance, aes(x = CloneGroupType, y = EachGroup_25Percentile))
# Add the geometric object box plot
box_plot +
    geom_boxplot()






##### ------- Questions Functions Clones  ---------- #######
distinctClsFunc <- count(Q_Mix_Model_csv_tbl %>%
                             filter((Granularity == 'Functions')) %>%
                             distinct(CloneClassType)) 
print(distinctClsFunc)


dat=matrix(nrow=16516,ncol=10)
dat=data.frame(dat)
for (i in 1:16516){
    scoresInVector = pull(Q_Mix_Model_csv_tbl %>%
                              filter((CloneClassType == i) & (Granularity == 'Functions')) %>%
                              select(Scores)) 
    print(length(scoresInVector))
    dat[i,1]<- i
    dat[i,2]<- 'Functions'
    dat[i,3]<- format(quantile(scoresInVector,c(0.25)) , scientific=FALSE); 
    dat[i,4]<- format(min(scoresInVector), scientific=FALSE); 
    dat[i,5]<- format(max(scoresInVector), scientific=FALSE); 
    dat[i,6]<- format(mean(scoresInVector), scientific=FALSE);  
    dat[i,7]<- format(median(scoresInVector), scientific=FALSE);  
    dat[i,8]<- format(var(scoresInVector), scientific=FALSE); 
    dat[i,9]<- format(sqrt(var(scoresInVector)), scientific=FALSE);  
    dat[i,10]<- length(scoresInVector)
    
} 

# 7.2 export to CSV ----
dat %>% write_csv("CloneDetection/CalcQuestionsFunctions.csv")



funcvariance = dat %>%
    rename(
        CloneGroupType = X1,
        EachGroup_Variance = X8 ,
        EachGroup_Mean = X6,
        EachGroup_Median = X7,
        EachGroup_25Percentile = X3
    ) #%>%
#filter((X9 > 0)) %>%
#slice(2:1000) 

# Store the graph
box_plot <- ggplot(funcvariance, aes(x = CloneGroupType, y = EachGroup_Variance))
# Add the geometric object box plot
box_plot +
    geom_boxplot()

# Store the graph
box_plot <- ggplot(funcvariance, aes(x = CloneGroupType, y = EachGroup_Mean))
# Add the geometric object box plot
box_plot +
    geom_boxplot()

# Store the graph
box_plot <- ggplot(funcvariance, aes(x = CloneGroupType, y = EachGroup_Median))
# Add the geometric object box plot
box_plot +
    geom_boxplot()

# Store the graph
box_plot <- ggplot(funcvariance, aes(x = CloneGroupType, y = EachGroup_25Percentile))
# Add the geometric object box plot
box_plot +
    geom_boxplot()



