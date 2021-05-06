library(data.table)
library(randomForest)
library(plyr)

setwd('/Users/knar/Desktop/ComputationalLabs/Project/Data')
rawdat <- fread("step2processed.csv")
data <- data.table(rawdat)


#Add Feature to Get Note Count  

#count # entries by subject id
note_count <- as.data.table(table(data$SUBJECT_ID))
#rename columns
colnames(note_count) <- c("SUBJECT_ID","NOTECOUNT")
#turn into #'s
note_count$SUBJECT_ID <- as.numeric(note_count$SUBJECT_ID)
note_count$NOTECOUNT <- as.numeric(note_count$NOTECOUNT)
#add to main df
data1 <- merge(data,note_count, by = "SUBJECT_ID", all.x = TRUE)


#Add in # of diagnoses by hospital stay (hadmid)
icd.codes <- fread("DIAGNOSES_ICD.csv")
icd.table <- data.table(icd.codes)

#get # icd per hospital stay
icd.counts <- as.data.table(table(icd.table$HADM_ID))

#rename columns, turn into #'s, merge with main df
names(icd.counts) <- c("HADM_ID", "ICD_COUNTS")
icd.counts$ICD_COUNTS <- as.numeric(icd.counts$ICD_COUNTS)
icd.counts$HADM_ID <- as.numeric(icd.counts$HADM_ID)

data2 <- merge(data1,icd.counts,by="HADM_ID", all.x=TRUE)


#Add in # of services by hospital stay (hadmid)


services <- fread("SERVICES.csv")
service.table <- data.table(services)
#get # services by hospital stay
service.counts <- as.data.table(table(service.table$HADM_ID))

#rename columns, turn into #'s, merge with main df
names(service.counts) <- c("HADM_ID", "SERVICE_COUNTS")
service.counts$HADM_ID <- as.numeric(service.counts$HADM_ID)

data3 <- merge(data2,service.counts,by="HADM_ID", all.x=TRUE)


write.csv(data3, file = "step3processed.csv", row.names=FALSE)
