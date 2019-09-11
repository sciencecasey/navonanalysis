#######Created for Analyzing Inverted Faces data over 2 sessions 
#for 2019 VM Pilot study, Attention Modulation Intervention
##Casey 09/10/2019

####################DATA CLEANING ########################################
#import all IF files as a DF
inputdir='/Users/casey/Desktop/navonanalysis-master/data/source/attmod'
files=list.files(path='/Users/casey/Desktop/navonanalysis-master/data/source/attmod',
                 pattern="*task-invertedfaces(.*)csv$", recursive = T, full.names = TRUE)
invertlist=lapply(files, read.csv)
invertdf=do.call(rbind, invertlist)
str(invertdf)
outputdir='/Users/casey/Desktop/navonanalysis-master/data/derivatives/attmod'
numsubs=3 #total number subjects
numses=2 #total number of sessions

#dataset specific!
#some of the data is saved as Inv some and Inverse, change all to Inv
invertdf$Direction[invertdf$Direction=="Inverse"]="Inv"
#some subjects saved without 2 at the end for time 2
invertdf$Subject[invertdf$Subject=="5031"]="50312"
invertdf$Subject[invertdf$Subject=="5020"]="50202"

#subset only the LONG trials
invertdf=subset(invertdf, Time=='l')
invertdf[["Time"]] #just checking

#add task column
invertdf$task=rep("invertedfaces", length(invertdf$Subject))
#add intervention column
invertdf$intervention=rep("fastface", length(invertdf$Subject))
#add Trial counter Column for only the long
invertdf$trial=factor(rep(seq(1, length(navondf$Session)/(numsubs*numses)), numsubs))

#subset, select, and factor relevant portions
#using the subsetted trial counter rather than original
invertdf=subset(invertdf, select=c(Subject, Session, task, intervention, trial, Direction, 
                                   Comparison.ACC, Comparison.RT))
invertdf=within(invertdf, {
  Subject=factor(Subject)
  Session=factor(Session)
  Trial=factor(Trial)
  task=factor(task)})
str(invertdf)

#recode non-repsponse as NA response time
sum(invertdf$Comparison.RT=="0") #checking amount
is.na(invertdf$Comparison.RT)=invertdf$Comparison.RT=="0" #recode
is.na(invertdf$Comparison.ACC)=is.na(invertdf$Comparison.RT) #recode
sum(is.na(invertdf$Comparison.ACC)) #checking nonresponse
sum(invertdf$Comparison.ACC=="0", na.rm=T) #check that nonresponse different than inacc

#add column for log RT
invertdf$logRT=log(invertdf$Comparison.RT)
sum(is.na(invertdf$logRT) && !is.na(invertdf$Comparison.RT)) #check no new NAs created

#export the wrangled DF for safekeeping and BIDs
write_tsv(invertdf, file.path(outputdir, "task-invertedfaces-navon_beh.tsv"))

########### Simple Statistics
#identify functions of interest
funs=list(mean=~mean(., na.rm=TRUE), sd=~sd(., na.rm=TRUE))

#RT for all answered questions separated by Direction, regardless of ACC
##check where we are
g=sum(invertdf$Comparison.ACC==0, na.rm = T) #55
w=sum(invertdf$Comparison.ACC==1, na.rm = T) #485
q=(length(invertdf$logRT)-sum(is.na(invertdf$logRT))) #1327 [total should be the 2 above]
ifelse (q!=g+w, 
        glue('RT CODED WRONG check output'),
        glue('RT coded for all responses'))

Allresp_temp1= invertdf %>%
  group_by(Subject, Session, Direction) %>%
  summarise_at(c("Comparison.ACC", "logRT"), funs)
Allresp_temp2=invertdf %>%
  group_by(Session, Direction) %>%
  summarise_at(c("Comparison.ACC", "logRT"), funs)
Allresp_Istats=bind_rows(Allresp_temp1, Allresp_temp2)
Allresp_Istats$Subject=ifelse(is.na(Allresp_Istats$Subject), "Overall", 
                              Allresp_Istats$Subject)
rm(Allresp_temp1, Allresp_temp2)

#print output as df
write_tsv(Allresp_Istats, file.path(outputdir, "task-invertedfaces_ses-both_acq-allresp_stats-meansd.tsv"))

###############Repeated Measures Anovas and GLMMs
####RT, all answered Repeated Measures
summary(aov(logRT~Direction+Error(Subject*Session/Direction), data=invertdf))
#lmer
summary(lmer(logRT~1+Direction+Session+Direction:Session+(1+Session|Subject), data=invertdf))
anova(lmer(logRT~1+Direction+Session+Direction:Session+(1+Session|Subject), data=invertdf))

##########RT ANALYSIS only ACC answers
#recode ACC=0 as RT=NA 
sum(is.na(invertdf$logRT))
is.na(invertdf$logRT)=invertdf$Comparison.ACC==0
sum(is.na(invertdf$logRT)) #checking
sum(is.na(invertdf$Comparison.ACC)) #now should be less NA for ACC than RT (showing incorrect are also NA RT)

##check where we are
w=sum(invertdf$Comparison.ACC==1, na.rm = T) #1297
q=length(invertdf$logRT)-sum(is.na(invertdf$logRT)) #1327 [total should be the 2 above]
ifelse (q!=w, 
        glue('RT CODED WRONG check output'),
        glue('RT coded only for correct responses'))

###Simple Statistics
Allresp_temp1= invertdf %>%
  group_by(Subject, Session, Direction) %>%
  summarise_at(c("Comparison.ACC", "logRT"), funs)
Allresp_temp2=invertdf %>%
  group_by(Session, Direction) %>%
  summarise_at(c("Comparison.ACC", "logRT"), funs)
Allaccresp_Istats=bind_rows(Allresp_temp1, Allresp_temp2)
Allaccresp_Istats$Subject=ifelse(is.na(Allaccresp_Istats$Subject), "Overall", 
                                 Allaccresp_Istats$Subject)
rm(Allresp_temp1, Allresp_temp2)

#print output as  df
write_tsv(Allaccresp_Istats, file.path(outputdir, "task-invertedfaces_ses-both_acq-accresp_stats-meansd.tsv"))

###############Repeated Measures Anovas and GLMMs
####RT, all answered Repeated Measures
summary(aov(logRT~Direction+Error(Subject*Session/Direction), data=invertdf))
#lmer
summary(lmer(logRT~1+Direction+Session+Direction:Session+(1+Session|Subject), data=invertdf))
anova(lmer(logRT~1+Direction+Session+Direction:Session+(1+Session|Subject), data=invertdf))

########## ACC Analysis
###
###############Repeated Measures Anovas and GLMMs
####RT, all answered Repeated Measures
summary(aov(Comparison.ACC~Direction+Error(Subject*Session/Direction), data=invertdf))
#lmer
summary(lmer(Comparison.ACC~1+Direction+Session+Direction:Session+(1+Session|Subject), data=invertdf))
anova(lmer(Comparison.ACC~1+Direction+Session+Direction:Session+(1+Session|Subject), data=invertdf))

