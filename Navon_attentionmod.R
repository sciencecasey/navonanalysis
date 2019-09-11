#######Created for Analyzing Navon data over 2 sessions for 
#2019 VM Pilot study Fast Face
##Casey 08/01/2019
####################DATA CLEANING ########################################
#import all navon files as a DF
inputdir='/Users/casey/Desktop/navonanalysis-master/data/source/attmod'
files=list.files(path='/Users/casey/Desktop/navonanalysis-master/data/source/attmod',
                 pattern="*task-navon(.*)csv$", recursive = T, full.names = TRUE)
navonlist=lapply(files, read_csv)
navondf=do.call(rbind, navonlist)
str(navondf)
outputdir='/Users/casey/Desktop/navonanalysis-master/data/derivatives/attmod'
numsubs=3 #total number subjects
numses=2 #total number of sessions

#remove practice block
navondf=navondf[!navondf$Block=="P",]
sum(navondf$Block=="P")
str(navondf) 

#add task column for export if grand DF ever desired
navondf$task=rep("navon", length(navondf$Subject))
#add group column for export if grand DF ever desired
navondf$intervention=rep("attentionmod", length(navondf$Subject))

#add Trial counter Column
navondf$trial=factor(rep(seq(1, length(navondf$Session)/(numsubs*numses)), numsubs))

#Remove irrelevant variables columns and reorder
navondf=subset(navondf, select= c(Subject, Session, task, intervention, trial, TargetLocation, 
                                  Configuration, Stimuli.ACC, Stimuli.RT))

#change needed columns to factors for grouping
navondf=within(navondf, {
  Session=as.factor(Session)
  Subject=as.factor(Subject)
  task=as.factor(task)
  intervention=as.factor(intervention)
})
str(navondf)

#recode non-repsponse as NA response time
sum(navondf$Stimuli.RT=="0") #checking amount (10)
is.na(navondf$Stimuli.RT)=navondf$Stimuli.RT=="0" #recode
is.na(navondf$Stimuli.ACC)=is.na(navondf$Stimuli.RT) #recode
sum(is.na(navondf$Stimuli.ACC)) #checking nonresponse (now there are 10)
sum(navondf$Stimuli.ACC=="0", na.rm=T) #check that nonresponse different than inacc

#add column for RT log (for calculations)
navondf$logRT=log(navondf$Stimuli.RT)
sum(is.na(navondf$logRT) && !is.na(navondf$Stimuli.RT)) #check that no new NAs created

#make additional switch/nonswitch condition (based on configuration)
navondf$Switch=as.factor(ifelse(navondf$Configuration=="GLS" |
                                  navondf$Configuration=="LGS", "switch", "nonswitch"))

#export the wrangled DF for safekeeping and BIDs
write_tsv(navondf, file.path(outputdir, "task-navon_beh.tsv"))

########### Simple Statistics
###Save functions of interest
funs=list(mean = ~mean(., na.rm=TRUE), sd = ~sd(., na.rm = TRUE))

#RT for all answered questions separated by Switch/Nonswitch, regardless of ACC
##check where we are
g=sum(navondf$Stimuli.ACC==0, na.rm = T)#53
w=sum(navondf$Stimuli.ACC==1, na.rm = T) #1227
q=length(navondf$logRT)-sum(is.na(navondf$logRT)) #1280 [total should be the 2 above]
ifelse (q!=g+w, 
        glue('RT CODED WRONG {\n}check output'),
        glue('RT coded for all responses'))

Allresp_temp1= subset(navondf, select=c(Subject, Session, Switch, Stimuli.ACC, logRT)) %>%
  group_by(Subject, Switch, Session) %>%
  summarise_at(c("Stimuli.ACC", "logRT"), funs)
Allresp_temp2=subset(navondf, 
                     select=c(Subject, Session, Switch, Stimuli.ACC, logRT)) %>%
  group_by(Switch, Session) %>%
  summarise_at(c("Stimuli.ACC", "logRT"), funs)
Allresp_switchstats=bind_rows(Allresp_temp1, Allresp_temp2)
Allresp_switchstats$Subject=ifelse(is.na(Allresp_switchstats$Subject), "Overall", 
                                   Allresp_switchstats$Subject)
#rm(Allresp_temp1, Allresp_temp2)

Allresp_switch_temp1= navondf %>%
  group_by(Subject, Session, Switch) %>%
  summarise_at(c("Stimuli.ACC", "logRT"), funs)
Allresp_switch_temp2=navondf %>%
  group_by(Session, Switch) %>%
  summarise_at(c("Stimuli.ACC", "logRT"), funs)
Allresp_switch_stats=bind_rows(Allresp_switch_temp1, Allresp_switch_temp2)
Allresp_switch_stats$Subject=ifelse(is.na(Allresp_switch_stats$Subject), "Overall", 
                                    Allresp_switch_stats$Subject)
#rm(Allresp_switch_temp1, Allresp_switch_temp2)

#RT for all answered questions separated by TARGET LOCATION(global/local), regardless of ACC
Allresp_localglobal_temp1= subset(navondf, 
                                  select=c(Subject, Session, TargetLocation, Stimuli.ACC, logRT)) %>%
  group_by(Subject, Session, TargetLocation) %>%
  summarise_at(c("Stimuli.ACC", "logRT"), funs)
Allresp_localglobal_temp2=navondf %>%
  group_by(Session, TargetLocation) %>%
  summarise_at(c("Stimuli.ACC", "logRT"), funs)
Allresp_localglobal_stats=bind_rows(Allresp_localglobal_temp1, Allresp_localglobal_temp2)
Allresp_localglobal_stats$Subject=ifelse(is.na(Allresp_localglobal_stats$Subject), "Overall", 
                                         Allresp_localglobal_stats$Subject)
#rm(Allresp_localglobal_temp1, Allresp_localglobal_temp2)

#print output as larg df
Allresp_navonstats=bind_cols(Allresp_switch_stats, Allresp_localglobal_stats)
write_tsv(Allresp_navonstats, file.path(outputdir, "task-navon_ses-both_acq-allresp_stats-meansd.tsv"))

###############Repeated Measures Anovas and GLMMs
####RT, all answered Repeated Measures Switch
summary(aov(logRT~Switch+Error(Subject*Session/Switch), data=navondf))
#lmer
summary(lmer(logRT~1+Switch+Session+Switch:Session+(1+Session|Subject), data=navondf)) 
anova(lmer(logRT~1+Switch+Session+Switch:Session+(1+Session|Subject), data=navondf))

####RT, all answered Repeated Measures Local/Global
summary(aov(logRT~TargetLocation+Error(Subject*Session/TargetLocation), data=navondf))
#lmer
summary(lmer(logRT~1+TargetLocation+Session+TargetLocation:Session+(1+Session|Subject), data=navondf)) 
anova(lmer(logRT~1+TargetLocation+Session+TargetLocation:Session+(1+Session|Subject), data=navondf))

##########RT ANALYSIS only ACC answers
#recode ACC=0 as RT=NA 
sum(is.na(navondf$logRT))
is.na(navondf$logRT)=navondf$Stimuli.ACC==0
sum(is.na(navondf$logRT)) #checking
sum(is.na(navondf$Stimuli.ACC)) #now should be less NA for ACC than RT (showing incorrect are also NA RT)

##check where we are
w=sum(navondf$Stimuli.ACC==1, na.rm = T) #1297
q=length(navondf$logRT)-sum(is.na(navondf$logRT)) #1327 [total should be the 2 above]
ifelse (q!=w, 
        glue('RT CODED WRONG {\n}check output'),
        glue('RT coded only for correct responses'))

###Simple Statistics
Allresp_temp1= navondf %>%
  group_by(Subject, Switch, Configuration, TargetLocation, Session) %>%
  summarise_at(c("Stimuli.ACC", "logRT"), funs)
Allresp_temp2=navondf %>%
  group_by(Switch, Configuration, TargetLocation, Session) %>%
  summarise_at(c("Stimuli.ACC", "logRT"), funs)
Allaccresp_stats=bind_rows(Allresp_temp1, Allresp_temp2)
Allaccresp_stats$Subject=ifelse(is.na(Allaccresp_stats$Subject), "Overall", 
                                Allaccresp_stats$Subject)
#rm(Allresp_temp1, Allresp_temp2)

Allresp_switch_temp1= navondf %>%
  group_by(Subject, Session, Switch) %>%
  summarise_at(c("Stimuli.ACC", "logRT"), funs)
Allresp_switch_temp2=navondf %>%
  group_by(Session, Switch) %>%
  summarise_at(c("Stimuli.ACC", "logRT"), funs)
Allaccresp_switch_stats=bind_rows(Allresp_switch_temp1, Allresp_switch_temp2)
Allaccresp_switch_stats$Subject=ifelse(is.na(Allaccresp_switch_stats$Subject), "Overall", 
                                       Allaccresp_switch_stats$Subject)
#rm(Allresp_switch_temp1, Allresp_switch_temp2)

#RT for all answered questions separated by TARGET LOCATION(global/local), regardless of ACC
Allresp_localglobal_temp1= navondf %>%
  group_by(Subject, Session, TargetLocation) %>%
  summarise_at(c("Stimuli.ACC", "logRT"), funs)
Allresp_localglobal_temp2=navondf %>%
  group_by(Session, TargetLocation) %>%
  summarise_at(c("Stimuli.ACC", "logRT"), funs)
Allaccresp_localglobal_stats=bind_rows(Allresp_localglobal_temp1, Allresp_localglobal_temp2)
Allaccresp_localglobal_stats$Subject=ifelse(is.na(Allaccresp_localglobal_stats$Subject), "Overall", 
                                            Allaccresp_localglobal_stats$Subject)
#rm(Allresp_localglobal_temp1, Allresp_localglobal_temp2)

#print output as larg df
Allaccresp_stats=bind_cols(Allaccresp_switch_stats, Allaccresp_localglobal_stats)
write_tsv(Allaccresp_stats, file.path(outputdir, "task-navon_ses-both_acq-accresp_stats-meansd.tsv"))

###############Repeated Measures Anovas and GLMMs
####RT, acc answered Repeated Measures Switch
summary(aov(logRT~Switch+Error(Subject*Session/Switch), data=navondf))
#lmer
summary(lmer(logRT~1+Switch+Session+Switch:Session+(1+Session|Subject), data=navondf)) 
anova(lmer(logRT~1+Switch+Session+Switch:Session+(1+Session|Subject), data=navondf))

####RT, all answered Repeated Measures Local/Global
summary(aov(logRT~TargetLocation+Error(Subject*Session/TargetLocation), data=navondf))
#lmer
summary(lmer(logRT~1+TargetLocation+Session+TargetLocation:Session+(1+Session|Subject), data=navondf)) 
anova(lmer(logRT~1+TargetLocation+Session+TargetLocation:Session+(1+Session|Subject), data=navondf))


########## ACC Analysis
###
###############Repeated Measures Anovas and GLMMs
####Repeated Measures Switch
summary(aov(Stimuli.ACC~Switch+Error(Subject*Session/Switch), data=navondf))
#lmer
summary(lmer(Stimuli.ACC~1+Switch+Session+Switch:Session+(1+Session|Subject), data=navondf)) 
anova(lmer(Stimuli.ACC~1+Switch+Session+Switch:Session+(1+Session|Subject), data=navondf))

####RT, all answered Repeated Measures Local/Global
summary(aov(Stimuli.ACC~TargetLocation+Error(Subject*Session/TargetLocation), data=navondf))
#lmer
summary(lmer(Stimuli.ACC~1+TargetLocation+Session+TargetLocation:Session+(1+Session|Subject), data=navondf)) 
anova(lmer(Stimuli.ACC~1+TargetLocation+Session+TargetLocation:Session+(1+Session|Subject), data=navondf))

