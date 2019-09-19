#######Created for Analyzing Enotional Valence data over 2 sessions 
#for 2019 VM Pilot study, Fast Face Intervention
##Casey 08/14/2019

##Import Data
inputdir='/Users/casey/Desktop/navonanalysis-master/data/source/fast'
files=list.files(path='/Users/casey/Desktop/navonanalysis-master/data/source/fast',
                 pattern="*emoval(.*)csv$", recursive = T, full.names = TRUE)
emolist=lapply(files, read.csv)
emodf=do.call(rbind, emolist)
str(emodf)
outputdir='/Users/casey/Desktop/navonanalysis-master/data/derivatives/fast'
numsubs=5 #total number subjects
numses=2 #total number of sessions

#subset, select, and factor relevant portions
emodf$Sub=emodf$Participant.Name
emodf=subset(emodf, select=c(Sub, Session, Video.Time, Valence))
str(emodf)
emodf=within(emodf, {
  Sub=factor(Sub)
  Session=factor(Session)
  Valence=as.numeric(as.character(Valence)) 
  #because not saved as NA, need to convert from 
  #factor to character then numeric in order to force out NAs
})
sum(is.na(emodf$Valence))

####add row for MS conversion
library(lubridate)
emodf$Time=ms(emodf$Video.Time)

#export as large tsv
write_tsv(emodf, file.path(outputdir, "task-emoval_ses-both_beh.tsv"))

#######Looking at the entire session

#identify functions of interest
funs=list(mean=~mean(., na.rm=TRUE), sd=~sd(., na.rm=TRUE))

##Simple Statistics
temp1= emodf %>%
  group_by(Sub, Session) %>%
  summarise_at("Valence", funs)
temp2=emodf %>%
  group_by(Session) %>%
  summarise_at("Valence", funs)
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
#rm(temp1)

summary(lmer(Valence~Session+(1+Session|Sub), data=emodf))
#summary(aov(Valence~Session+Error(Sub/Session), data=emodf)) (checking it's same, can't tell but should be?)

#Plot
xyplot(mean~Session, temp1, groups = Sub, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, auto.key = TRUE, 
       main="Average Valence by Subject", ylab="Average Valence") 

#######Looking at only natural faces (no fixation, instructions, pauses, scrambles)
#Select only the times of interest
#31.0-48.1 
seg1start=ms("00:31")
seg1end=ms("00:48.1")
#77.8-95.0 
seg2start=ms("01:17.8")
seg2end=ms("01:35")
#125.0-142.0 
seg3start=ms("02:05")
seg3end=ms("02:22")
#we only use 2.5 minutes of data
#2.5min 
emofilt=ungroup(emodf)
emofilt=subset(emodf, select=c(Sub, Session, Time, Valence))
emofilt=filter(emofilt, Time>=seg1start & Time<=seg1end | Time>=seg2start & Time<=seg2end | Time>=seg3start & Time<=seg3end)

##Simple Statistics
ttemp3= emofilt %>%
  group_by(Sub, group) %>%
  summarise_at("Valence", funs)
temp4=emofilt %>%
  group_by(group) %>%
  summarise_at("Valence", funs)
Selecttime_Eselectstats=bind_rows(temp3, temp4)
Selecttime_Eselectstats$Sub=ifelse(is.na(Selecttime_Eselectstats$Sub), "Overall", 
                                   Selecttime_Eselectstats$Sub)
rm(temp4)

#print output as df
write_tsv(Selecttime_Eselectstats, file.path(outputdir, "task-emoval_ses-both_acq-selecttime_stats-meansd.tsv"))


##T Test and GLMM
t.test(mean~Session, alternative="less", data=temp3, na.rm=TRUE,
       paired=TRUE)
cohensD(mean~Session, data=temp1)
#rm(temp3)

summary(lmer(Valence~Session+(1+Session|Sub), data=emodfilt))

#Plot
xyplot(mean~Session, temp3, groups = Sub, type=c('p','l'),
       par.settings=ggplot2like(),axis=axis.grid, auto.key = TRUE, 
       main="Average Valence by Subject", ylab="Average Valence") 
