2 Folders are present: Scripts and ProcessedDataFinalModel

RawData: 

Please download raw data tables in CSV format from MIMIC III. The tables are the following: NOTEEVENTS, CAREGIVERS, ADMISSIONS, SERVICES, DIAGNOSES_ICD. 

Scripts: contains 6 scripts 

4 for cleaning the raw data
Broken into portions as running it all at once is time-consuming and eats memory

step1_clnnote_cg.R output feeds into step2_cglabels.R script
step2_cglabels.R output feeds into step3_addfeatures.R script
step3_addfeatures.R output feeds into step4_cosine_addfeatures.ipynb.
Result from step4_cosine_addfeatures.ipynb script feeds into the forest_final.R

2 Scripts for training the classifier and error analysis
The results from forest_final.R feed into ErrorAnalysisFeatureExploration.ipynb

ProcessedDataFinalModel:
contains the final trained model:mdel.RData
and our final clean dataset:model_finalv1