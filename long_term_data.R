library(readr)
library(dplyr)
library(tidyverse)
FBVAR <- read_csv("TEVAR_PROC.csv")

LT_09<- read_csv("20210901.csv")
LT_07<-read_csv("20210701.csv")
LT_01<-read_csv("20210101.csv")


#combine all dataframe Concatenating then choose the variable with unique ltf id
combine2<-bind_rows(LT_07, LT_09,LT_01) %>%
  distinct(LTF_ID, .keep_all = TRUE)

###
#TEVAR_LTF <- bind_rows(LT_01[! LT_01$LTF_ID  %in% LT_07$LTF_ID,],
#                   LT_07)
#TEVAR_LTF <- bind_rows(TEVAR_LTF[! TEVAR_LTF$LTF_ID  %in% LT_09$LTF_ID,],
#                       LT_09)


# extract rows that match a list of subject ID
a<-combine2[combine2$PATIENTID %in% FBVAR$PATIENTID,]



# show rows that contain number that one column larger than another
reint2<-filter(a, LTF_DAYS>PROC_SURVIVALDAYS)%>%select (PATIENTID,PRIMPROCID,LTF_DAYS,DEAD,PROC_SURVIVALDAYS)



#transform to wide format
a1<-a%>%select (PATIENTID,PRIMPROCID,LTF_ID,LTF_DAYS,DEAD,PROC_SURVIVALDAYS, LTF_NUM_REINT)

#h<-df1 %>%
#  arrange(LTF_DAYS, LTF_NUM_REINT)%>%
#  rowid_to_column() %>%
#  arrange(PRIMPROCID,rowid)


#first_reint_time is the first reintervention occured LTF_DAYS,
#second_reint_time is the second reintervention occured LTF_DAYS

df1 <- a1 %>%
  arrange(LTF_DAYS, LTF_NUM_REINT)%>%
  group_by(PATIENTID,PRIMPROCID) %>% 
  mutate(first_reint_time = LTF_DAYS[which(LTF_NUM_REINT == '1' |
                                        LTF_NUM_REINT == '2')[1]],
         second_reint_time = LTF_DAYS[which(LTF_NUM_REINT == '2')[1]]) %>%
  ungroup

# group based on patient id only
#a2<-df1 %>%
#  arrange(LTF_DAYS) %>%
#  rowid_to_column() %>%
#  gather(var, val, -c(PATIENTID,rowid)) %>%
#  arrange(rowid,PATIENTID,var) %>%
#  group_by(PATIENTID) %>%
#  mutate(var = paste(var, gl(n_distinct(rowid), n_distinct(var)), sep = "_")) %>%
#  select(-rowid) %>%
#  spread(var, val, convert = TRUE) 



# group based on patient id  and procedure id, patient id will be repeated
#  not sure if the LTF ID is increased based on the time documented the 
a3<-df1 %>%
  arrange(LTF_DAYS) %>%
  rowid_to_column() %>%
  gather(var, val, -c(PATIENTID,PRIMPROCID,rowid, first_reint_time, second_reint_time)) %>%
  arrange(rowid,PATIENTID,PRIMPROCID,var) %>%
  group_by(PATIENTID,PRIMPROCID) %>%
  mutate(var = paste(var, gl(n_distinct(rowid), n_distinct(var)), sep = "_")) %>%
  select(-rowid) %>%
  spread(var, val, convert = TRUE) 

# long_survdays is the longest survival days after the procedure
# long_fudays is the longest follow up days after the procedure
# new_surv is the survival days if the longest follow up day is larger than the longest survival days
# total_reint is the total number of reintervention for that procudure
a4<-a3%>% rowwise() %>% mutate(long_survdays = max(PROC_SURVIVALDAYS_1,PROC_SURVIVALDAYS_2,
                                                   PROC_SURVIVALDAYS_3,PROC_SURVIVALDAYS_4,
                                                   PROC_SURVIVALDAYS_5,PROC_SURVIVALDAYS_6,
                                                   PROC_SURVIVALDAYS_7,PROC_SURVIVALDAYS_8, 
                                                   na.rm = TRUE),
                               long_fudays = max(LTF_DAYS_1,LTF_DAYS_2,
                                                 LTF_DAYS_3,LTF_DAYS_4,
                                                 LTF_DAYS_5,LTF_DAYS_6,
                                                 LTF_DAYS_7,LTF_DAYS_8, 
                                                 na.rm = TRUE),
                               new_surv = max(long_survdays,long_fudays, 
                                                 na.rm = TRUE),
                               total_reint = max(c(LTF_NUM_REINT_1, LTF_NUM_REINT_2,
                                 LTF_NUM_REINT_3,LTF_NUM_REINT_4,
                                 LTF_NUM_REINT_5,LTF_NUM_REINT_5,
                                 LTF_NUM_REINT_7,LTF_NUM_REINT_8),na.rm = TRUE),
                               dead = max(c(DEAD_1, DEAD_2,
                                                   DEAD_3,DEAD_4,
                                                   DEAD_5,DEAD_5,
                                                   DEAD_7,DEAD_8),na.rm = TRUE)
                               )
a4$total_reint[a4$total_reint =="-Inf"] <- NA

write.csv(a4, file = "raw_LT.csv")

final<-a4%>%select(PATIENTID,PRIMPROCID,dead,long_survdays,long_fudays, new_surv,total_reint, first_reint_time, second_reint_time)
write.csv(final, file = "LT.csv")



#another way to choose the maximum
#library("matrixStats")
#a4$max_value <- rowMaxs(as.matrix(a4[, c("LTF_NUM_REINT_1", "LTF_NUM_REINT_2", 
#                                         "LTF_NUM_REINT_3","LTF_NUM_REINT_4",
#                                         "LTF_NUM_REINT_5","LTF_NUM_REINT_5",
#                                         "LTF_NUM_REINT_7","LTF_NUM_REINT_8")]), na.rm = TRUE)






#replace the largest date, the time scanned reintervention





#check if the participant has multiple procedure
proc<-a%>%select(PATIENTID, PRIMPROCID)
D <- unique(data.frame(proc))           # get unique rows
dupids <- with(D, PATIENTID[duplicated(PATIENTID)])   # duplicate ids
m<-subset(D, PATIENTID %in% dupids)               # rows for duplicate ids









#the following code are just data checking

FBVAR$LTF_NUM_REINT

reint<-filter(FBVAR, LTF_NUM_REINT %in% c("1", "2")) 

#FBVAR[FBVAR$LTF_NUM_REINT %in% c("1", "2"), ] 
reint%>%select(LTF_NUM_REINT)

table(reint$LTF_NUM_REINT)
table(FBVAR$LTF_NUM_REINT)


n_occur <- data.frame(table(reint$PATIENTID))
n_occur[n_occur$Freq > 1,]

reint[reint$PATIENTID %in% n_occur$Var1[n_occur$Freq > 1],]

reint<-filter(FBVAR, ) 
FBVAR$PROC_SURVIVALDAYS
FBVAR$LTF_DAYS

PROC <- read_csv("TEVAR_PROC.csv")

FU<- read_csv("~/Desktop/stlp new laptop/Capstone/Past 1/Comparative-analysis-of-treatments-of-CAA-main/TEVAR_International_20210901/TEVAR_International_LTF_r12_2_14_20210901.csv")
reint<-filter(FU, LTF_NUM_REINT %in% c("1", "2")) 

n_occur <- data.frame(table(reint$PATIENTID))
n_occur[n_occur$Freq > 1,]
reint[reint$PATIENTID %in% n_occur$Var1[n_occur$Freq > 1],]
reint2<-filter(reint, LTF_DAYS>PROC_SURVIVALDAYS) 

reint3<-reint2%>% select(PATIENTID,LTF_DAYS,PROC_SURVIVALDAYS)
A<-reint3$PATIENTID
B<-PROC$PATIENTID

C= A[(A %in% B)]
D = B[(B %in% A)]

all(unique(C) == unique(D))
C <- intersect(unique(A), unique(B))
C[C %in% A]

reint3[reint3$PATIENTID %in%c('510979', '558643', '259097', '603199' ,'246350', '598705', '554182'),]



