#######Created for Analyzing Enotional Valence data over 2 sessions 
#for 2019 VM Pilot study
##Casey 08/14/2019

##Import Data
inputdir='/Users/casey/Desktop/InvFaceNavon/RO1_Analysis_InvertedFaces_EmotionalValence/Data_used_Inverted_analysis_2/Analysis_2_BIDs/data/source/'
files=list.files(path='/Users/casey/Desktop/InvFaceNavon/RO1_Analysis_InvertedFaces_EmotionalValence/Data_used_Inverted_analysis_2/Analysis_2_BIDs/data',
                 pattern="*emoval(.*)csv$", recursive = T, full.names = TRUE)
emolist=lapply(files, read.csv)
emodf=do.call(rbind, emolist)
str(emodf)
outputdir='/Users/casey/Desktop/InvFaceNavon/RO1_Analysis_InvertedFaces_EmotionalValence/Data_used_Inverted_analysis_2/Analysis_2_BIDs/data/derivatives/'
numsubs=5 #total number subjects
numses=2 #total number of sessions

##as factor
emodf=within(emodf, {
  Sub=factor(Sub)
  Session=factor(Session)
  Valence=as.numeric(as.character(Valence)) 
  #because not saved as NA, need to convert from 
  #factor to character then numeric in order to force out NAs
})
sum(is.na(emodf$Valence))

####add row for MS conversion
convert=period_to_seconds(hms(emodf$Video.Time))
class(convert)
head(convert)
tail(convert)
emodf$TimeMs=convert

#export as large tsv
write_tsv(emodf, file.path(outputdir, "task-emoval_ses-both_beh.tsv"))


#######Looking at the entire session
##Simple Statistics
temp1= emodf %>%
  group_by(Sub, Session) %>%
  summarise_at("Valence", funs(mean,sd), na.rm=TRUE)
temp2=emodf %>%
  group_by(Session) %>%
  summarise_at("Valence", funs(mean,sd), na.rm=TRUE)
Alltime_Estats=bind_rows(temp1, temp2)
Alltime_Estats$Sub=ifelse(is.na(Alltime_Estats$Sub), "Overall", 
                              Alltime_Estats$Sub)
rm(temp2)

#print output as df
write_tsv(Alltime_Estats, file.path(outputdir, "task-emoval_ses-both_acq-alltime_stats-meansd.tsv"))


##T Test and GLMM
t.test(mean~Session, alternative="less", data=temp1, na.rm=TRUE,
       paired=TRUE)
cohensD(mean~Session, data=temp1)
rm(temp1)

summary(lmer(Valence~Session+(1+Session|Sub), data=emodf))


#######Looking at only natural faces (no fixation, instructions, pauses, scrambles)
#Select only the times of interest
#31.0-48.1 #1860-2886ms
#77.8-95.0 #4668-5700ms
#125.0-142.0 # 7500-8250ms
#we only use 2.5 minutes of data
#2.5min *60*60=9000 ms
emodfilt=subset(emodf, select=c(Sub, Session, TimeMs, Valence))
emodfilt=filter(emodfilt, 
                (emodfilt$TimeMS>=1860 & emodfilt$TimeMS<=2886) | 
                (emodfilt$TimeMS>=4668 & emodfilt$TimeMS<=5700) | 
                (emodfilt$TimeMS>=7500 & emodfilt$TimeMS<=8250))

##Simple Statistics
temp1= emodf %>%
  group_by(Sub, Session) %>%
  summarise_at("Valence", funs(mean,sd), na.rm=TRUE)
temp2=emodf %>%
  group_by(Session) %>%
  summarise_at("Valence", funs(mean,sd), na.rm=TRUE)
Alltime_Estats=bind_rows(temp1, temp2)
Alltime_Estats$Sub=ifelse(is.na(Alltime_Estats$Sub), "Overall", 
                          Alltime_Estats$Sub)
rm(temp2)

#print output as df
write_tsv(Alltime_Estats, file.path(outputdir, "task-emoval_ses-both_acq-alltime_stats-meansd.tsv"))


##T Test and GLMM
t.test(mean~Session, alternative="less", data=temp1, na.rm=TRUE,
       paired=TRUE)
cohensD(mean~Session, data=temp1)
rm(temp1)

summary(lmer(Valence~Session+(1+Session|Sub), data=emodf))