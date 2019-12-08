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

votes_tbl_one = readr::read_csv("Data_CSV_CloneDetection/usersUpAndDownVotesAnswers_1.csv")
votes_tbl_two = readr::read_csv("Data_CSV_CloneDetection/usersUpAndDownVotesAnswers_2.csv")

AnswerersUpAndDownVotes <- rbind(votes_tbl_one, votes_tbl_two)
#df %>% add_row(hello = "hola", goodbye = "ciao") to add single row use this for tidyverse.
#source : https://stackoverflow.com/questions/28467068/add-row-to-dataframe


#group the data but you can not get the distinct data from group but you can get distinct data from distinct function 
 uniqueAnswers = AnswerersUpAndDownVotes %>%
     distinct(PostId, UserId, VoteTypeId) #%>%
    #summarise(voteCount = count(VoteTypeId)) %>%
    #ungroup() 

 #count row using group by functions
 uniqueAnswers = AnswerersUpAndDownVotes %>%
     group_by(PostId, UserId, VoteTypeId) %>%
     summarise(VotesInCounts = n()) %>% ungroup() 
 
 uniqueUsersByPost = uniqueAnswers %>%
     distinct(UserId) 
     
#loop over dataframe of all the column 
# df <- votes_tbl_two # data
# 
# for (i in colnames(df)){
#     print(df[[i]])
# }
 CaclVote=matrix(nrow = nrow(uniqueUsersByPost),ncol=10)
 CaclVote=data.frame(CaclVote)
for (row in 1:nrow(uniqueUsersByPost)) {
    UserId_  = uniqueUsersByPost[row, "UserId"];
    scoresInVector = pull(uniqueAnswers %>%
                              filter((UserId == as.double(paste(UserId_))) & (VoteTypeId == 2)) %>% #Upvotes = 2 , Downvotes = 3 
                              select(VotesInCounts)) 
    
    
    CaclVote[row,1]<- UserId_;
    CaclVote[row,2]<- 'Downvotes';
    if(length(scoresInVector) > 0){
    CaclVote[row,3]<- format(quantile(scoresInVector,c(0.25)) , scientific=FALSE); 
    CaclVote[row,4]<- format(min(scoresInVector), scientific=FALSE); 
    CaclVote[row,5]<- format(max(scoresInVector), scientific=FALSE); 
    CaclVote[row,6]<- format(mean(scoresInVector), scientific=FALSE);  
    CaclVote[row,7]<- format(median(scoresInVector), scientific=FALSE);  
    } else {
        CaclVote[row,3]<- 0; 
        CaclVote[row,4]<- 0; 
        CaclVote[row,5]<- 0; 
        CaclVote[row,6]<- 0;  
        CaclVote[row,7]<- 0;  
    }
    if(length(scoresInVector) > 1)
        {
    CaclVote[row,8]<- format(var(scoresInVector), scientific=FALSE); 
    CaclVote[row,9]<- format(sqrt(var(scoresInVector)), scientific=FALSE);  
    } 
    else {
        CaclVote[row,8]<- 0; 
        CaclVote[row,9]<- 0;  
    }
    CaclVote[row,10]<- length(scoresInVector);
    
    
    print(paste("On", UserId_, "the stock price was", UserId_))
    
}
 
 
 CaclVote = CaclVote %>% 
    rename(
        UserId = X1,
        VoteTypeId = X2,
        Percentile25 = X3,
        minValue = X4,
        CloneGroupMax = X5,
        EachGroup_Mean = X6,
        EachGroup_Median = X7,
        CloneGroupVariance = X8,
        EachGroup_SD = X9,
        elementNo = X10
        
    ) 

# 7.2 export to CSV ----
 CaclVote %>% write_csv("Data_CSV_CloneDetection/CalculatedAnswerersUpvote.csv")



