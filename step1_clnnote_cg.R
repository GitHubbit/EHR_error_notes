# Kamileh Narsinh, Katie LaRow
# UNI: kn2404, kel2158
# Project

#Script 1: Clean notes table, concat caregive information

#install.packages("data.table")
#install.packages("lubridate")
#install.packages("SnowballC")
#install.packages("tm")
#install.packages("dplyr")
library(data.table)
library(lubridate)
library(SnowballC)
library(tm)
library(dplyr)

setwd('/Users/knar/Desktop/ComputationalLabs/Project/Data')

noteevents <- fread("NOTEEVENTS.csv")
caregivers <- fread("CAREGIVERS.csv")

noteevents.frame <- data.table(noteevents)
caregivers.frame <- data.table(caregivers)

dim(noteevents.frame) # we have 2,083,180 notes total (including duplicates)

noteevents.frame <- sub
# ****** DELETE  NOTES MARKED AS ERRORS IN CATEGORY OR DESCRIPTION ****** #

#find the notes where Error is marked in the DESCRIPTION and CATEGORY
descrip.errors <- data.frame(noteevents.frame[tolower(DESCRIPTION) %like% "error"]) 
#descrip.errors
cat.errors <- data.frame(noteevents.frame[tolower(CATEGORY) %like% "error"]) 
# cat.errors

#get rid of notes that have "error" in DESCRIPTION, no instances where marked in category
clean.notes <- noteevents.frame[!(noteevents.frame$ROW_ID %in% descrip.errors$ROW_ID)] 
# dim(clean.notes)

# check to see if any values in CATEGORY col are blank
clean.notes[CATEGORY=="", ]

# check to see if any values in CATEGORY col are NULL
clean.notes[is.na(clean.notes$CATEGORY)]


# ****** GET THE CAREGIVER LABELS AND DESCRIPTIONS ****** #

# Merge caregivers table with the noteevents/notes
#Notice in many notes the CGID is missing.  
 

names(caregivers.frame) <- c("CG_ROW_ID", "CGID", "CG_LABEL", "CG_DESCRIPTION")

# remove row_id column, not needed
caregivers.frame$CG_ROW_ID <- NULL 

# caregiver IDs in notes table are mapped to their Description and Label now
combined <- merge(clean.notes,caregivers.frame,by="CGID", all.x=TRUE) 

# see the types of caregivers labels and description combinations
# unique_cg <- as.data.frame(unique(caregivers.frame[,c("CG_LABEL","CG_DESCRIPTION")]))  


# ****** GET THE STORETIME - CHARTTIME FEATURE READY ****** #

# get the difference between the STORETIME and CHARTTIME (diff is in seconds)
combined$TIME.CHARTED <- ymd_hms(combined$CHARTTIME, tz = "US/Eastern")
combined$TIME.STORED <- ymd_hms(combined$STORETIME, tz = "US/Eastern")
combined$TIME.DIFF <- combined$TIME.STORED - combined$TIME.CHARTED

# ******  DATA CLEANING ****** #


# convert text, category, description, and caregiver.label columns to corpuses
text <- VCorpus(VectorSource(combined$TEXT))
des <- VCorpus(VectorSource(combined$DESCRIPTION))
cat <- VCorpus(VectorSource(combined$CATEGORY))


# lower-case, remove
#for text remove stop words
# for description and category also stem since we are just matching/collapsing classes

text <- tm_map(text, removePunctuation) #remove punctuation
text <- tm_map(text, content_transformer(tolower)) #convert to lowercase
text <- tm_map(text, removeWords, stopwords("english")) #remove stopwords

des <- tm_map(des, removePunctuation) #remove punctuation
des <- tm_map(des, content_transformer(tolower)) #convert to lowercase
des <- tm_map(des, stemDocument) #stem

cat <- tm_map(cat, removePunctuation) #remove punctuation
cat <- tm_map(cat, content_transformer(tolower)) #convert to lowercase
cat <- tm_map(cat, stemDocument) #stem


#Build dataframe with cleaned description, text and category columns
#probably should have just dropped the old category description and 


clean.notes <- data.table(CGID=combined$CGID,
                          ROW_ID=combined$ROW_ID,
                          SUBJECT_ID=combined$SUBJECT_ID,
                          HADM_ID=combined$HADM_ID,
                          CHARTDATE=combined$CHARTDATE,
                          CHARTTIME=combined$CHARTTIME,
                          STORETIME=combined$STORETIME,
                          CATEGORY=sapply(cat, as.character), stringsAsFactors=FALSE,
                          DESCRIPTION=sapply(des, as.character),
                          ISERROR=combined$ISERROR,
                          CG_LABEL=combined$CG_LABEL,
                          TEXT=sapply(text, as.character),
                          CG_DESCRIPTION=combined$CG_DESCRIPTION,
                          TIMECHARTED=combined$TIME.CHARTED,
                          TIMESTORED=combined$TIME.STORED,
                          TIMEDIFF=combined$TIME.DIFF)

write.csv(clean.notes, file = "step1processed.csv", row.names=FALSE)

