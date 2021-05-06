# Kamileh Narsinh
# UNI: kn2404
# Project

## Script used to clean caregiver labels

library(data.table)
library(lubridate)
library(SnowballC)
library(tm)
library(dplyr)

#feed in processed data from step1 script
setwd('/Users/knar/Desktop/ComputationalLabs/Project/Data')
wordprocessed <- fread("step1processed.csv")
#wordprocessed <- fread("testingclean1.notes.csv")
data <- data.table(wordprocessed)

cg.label <- VCorpus(VectorSource(data$CG_LABEL))


cg.label <- tm_map(cg.label, removePunctuation) #remove punctuation
cg.label <- tm_map(cg.label, content_transformer(tolower)) #convert to lowercase
cg.label <- tm_map(cg.label, stripWhitespace) #strip white space
cg.label.table <- data.table(CG_LABEL=sapply(cg.label, as.character), stringsAsFactors=FALSE)

########## NORMALIZE THE LABELS

cg.label.table$CG_LABEL <- gsub("\\<srt\\>", "StudentRespTherapist", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<srn\\>", "StudentNurse", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<coopst\\>", "CoOpStudent", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<coops\\>", "CoOpStudent", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<cphat\\>", "CertPharmTech", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<crt\\>", "RespirTherapist", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<rrt\\>", "RespirTherapist", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<rrts\\>", "RespirTherapist", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<rt\\>", "RespirTherapist", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<rth\\>", "RespirTherapist", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<respst\\>", "RespirTherapist", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<cowk\\>", "CoWorker", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<cowkr\\>", "CoWorker", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<cowor\\>", "CoWorker", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<cowker\\>", "CoWorker", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<cowkr\\>", "CoWorker", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<cowork\\>", "CoWorker", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<fell\\>", "Fellow", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<md\\>", "MD", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<dr\\>", "MD", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<mds\\>", "MedStudent", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<meds\\>", "MedStudent", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<medstu\\>", "MedStudent", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<medsty\\>", "MedStudent", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<stnmd\\>", "MedStudent", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<nstude\\>", "StudentNurse", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<nsgst\\>", "StudentNurse", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<nustud\\>", "StudentNurse", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<nurst\\>", "StudentNurse", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<rnstu\\>", "StudentNurse", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<stnrs\\>", "StudentNurse", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<stunur\\>", "StudentNurse", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<sturn\\>", "StudentNurse", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<phstu\\>", "PharmStudent", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<phstud\\>", "PharmStudent", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<phastu\\>", "PharmStudent", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<stpha\\>", "PharmStudent", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<rts\\>", "StudentRespTherapist", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<rtst\\>", "StudentRespTherapist", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<rtstu\\>", "StudentRespTherpaist", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<std\\>", "Student", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<stn\\>", "Student", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<stu\\>", "Student", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<stud\\>", "Student", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<studen\\>", "Student", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<student\\>", "Student", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<ua\\>", "UnitA", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<unita\\>", "UnitA", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<rn\\>", "Nurse", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<nurse\\>", "Nurse", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<nurs\\>", "Nurse", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<medres\\>", "Resident", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<res\\>", "Resident", cg.label.table$CG_LABEL)
cg.label.table$CG_LABEL <- gsub("\\<ns\\>", "StudentNurse", cg.label.table$CG_LABEL)

View(cg.label.table)

data$CG_LABEL <- cg.label.table$CG_LABEL


write.csv(data, file = "step2processed.csv", row.names=FALSE)

