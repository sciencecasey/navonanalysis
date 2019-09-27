#######Created for Analyzing BABS/BISS/STAI data over 2 sessions 
#for 2019 VM Pilot study, Attention Modulation
##Casey 09/11/2019

##Import Data
inputdir='/Users/casey/Desktop/navonanalysis-master/data/source/attmod'
bbsdf=read.csv('/Users/casey/Desktop/navonanalysis-master/data/source/attmod/BABS_BISS_STAI_POMS_formatted.csv')
str(bbsdf)
outputdir='/Users/casey/Desktop/navonanalysis-master/data/derivatives/attmod/'
rawdir='/Users/casey/Desktop/navonanalysis-master/data/raw/attmod/'
numsubs=3 #total number subjects
numses=2 #total number of sessions

#Interested in "babs_total", "biss_total_score_day1", "biss_total_score_beforescan1", "stai_total_score"
#"stai_total_score_v2", "stai_total_score_t2", "stai_total_score_v2_t2", "stai_total_score_t3"
#"stai_total_score_v2_t3", "biss_total_score_v2"
BBS2=subset(bbsdf, select = c(subject_id, babs_total, biss_total_score_day1, stai_total_score, 
                              stai_total_score_v2, stai_total_score_t2, stai_total_score_v2_t2,
                              stai_total_score_t3, stai_total_score_v2_t3, biss_total_score_v2))
str(BBS2)
#BISS T4-T1
BBS2$BissDifference=(BBS2$biss_total_score_v2-BBS2$biss_total_score_day1)
(BBS2$BissDifference)
(mean(BBS2$BissDifference))
#STAI T3V2-T1V1
(BBS2$StaiWidestDifference=BBS2$stai_total_score_v2_t3-BBS2$stai_total_score)
(mean(BBS2$StaiWidestDifference, na.rm=TRUE))
#Stai Each Diff by day
BBS2$StaiDiffDay1=BBS2$stai_total_score_v2-BBS2$stai_total_score 
BBS2$StaiDiffDay2=BBS2$stai_total_score_v2_t2-BBS2$stai_total_score_t2 
BBS2$StaiDiffDay3=BBS2$stai_total_score_v2_t3-BBS2$stai_total_score_t3 

#export as tsv
write_tsv(BBS2, file.path(rawdir, "task-babsbissstai_demographics.tsv"))

#Descriptives
Overall=c("Overall", mean(BBS2$BissDifference, na.rm=TRUE), mean(BBS2$StaiWidestDifference, na.rm=TRUE), 
          mean(BBS2$StaiDiffDay1, na.rm=TRUE), mean(BBS2$StaiDiffDay2, na.rm=TRUE), 
          mean(BBS2$StaiDiffDay3, na.rm=TRUE))
BBSDesc=subset(BBS2, select=c(subject_id, BissDifference, StaiWidestDifference, StaiDiffDay1, StaiDiffDay2,
                              StaiDiffDay3))
BBSDesc=rbind(BBSDesc, Overall)
BBSDesc$subject_id=ifelse(is.na(BBSDesc$subject_id), "Overall", BBSDesc$subject_id)

#Export TSV of stats
write_tsv(BBSDesc, file.path(outputdir, "task-babsbissstai_stats.tsv"))

#Stai V2-V1 Anova
library(ez)
Stai=c(BBS2$StaiDiffDay1, BBS2$StaiDiffDay2, BBS2$StaiDiffDay3)
Staidf=tibble(subject_id=rep(BBS2$subject_id, 3), Stai=Stai, Day=rep(seq(1,3), each=numsubs))
str(Staidf)
Staidf$Day=factor(Staidf$Day)

#Remove subID for incomplete days
Staidf=subset(Staidf, subject_id!="5038_2")

#summary(lmer(Stai~Day+(1+Day|subject_id), data=Staidf))
modelStai=ezANOVA(dv=Stai, within=Day, wid=subject_id, data=Staidf, return_aov = TRUE)
ezDesign(Staidf, Day, subject_id) #checking balance in design due to warning
#modelStai$Mauchly #fits model ????
#modelStai$ANOVA #significant result according to the F Value but not p value
modelStai
#doublechecked outputs with socscistatists.com/tests
#GES is the genearlized effect size
#model2Stai=aov(Stai~Day+Error(subject_id/Day), data=Staidf)
#summary(model2Stai) #prints the same result as return_aov

#post hoc t tests
t2.t1=t.test(BBS2$StaiDiffDay2, BBS2$StaiDiffDay1, paired=TRUE)
t3.t2=t.test(BBS2$StaiDiffDay3, BBS2$StaiDiffDay2, paired=TRUE)
t3.t1=t.test(BBS2$StaiDiffDay3, BBS2$StaiDiffDay1, paired=TRUE)
p.adjust(c(t2.t1$p.value, t3.t2$p.value, t3.t1$p.value), method = "holm") #no sig. differences

#Graphs
#plot lines BISS OT and Stai OT
#make new df with Time, Biss and Stai
forms=c(BBS2$biss_total_score_day1, BBS2$biss_total_score_v2, BBS2$stai_total_score, 
        BBS2$stai_total_score_v2, BBS2$stai_total_score_t2, BBS2$stai_total_score_v2_t2, 
        BBS2$stai_total_score_t3, BBS2$stai_total_score_v2_t3)
time=rep(c(1,5,1,2,3,4,5,6),each= numsubs)
sub=rep(BBS2$subject_id, 8)
test=c(rep("Biss", 2*numsubs), rep("Stai", 6*numsubs))
BBSlong=tibble("Subject"=sub, "Task"=test, "Score"=forms, "Timepoint"=time)
str(BBSlong)
BBSlong$Task=factor(BBSlong$Task)
BBSlong$Timepoint=factor(BBSlong$Timepoint)
str(BBSlong)

#plot each person's Responses Over time
xyplot(Score~Timepoint, groups=Task, subset=Subject=="5036_2", data=BBSlong, 
       type=c('l', 'p'), main="Subject1 Responses", auto.key = list(space="top"), axis=axis.grid)
xyplot(Score~Timepoint, groups=Task, subset=Subject=="5037_2", data=BBSlong, 
       type=c('l', 'p'), main="Subject2 Responses", auto.key = list(space="top"), axis=axis.grid)
xyplot(Score~Timepoint, groups=Task, subset=Subject=="5038_2", data=BBSlong, 
       type=c('l', 'p'), main="Subject3 Responses", auto.key = list(space="top"), axis=axis.grid)
xyplot(Score~Timepoint|Subject, groups=c(Task), layout=c(numsubs,1), aspect=2, data=BBSlong, 
       scales=list(alternating=FALSE), axis=axis.grid, type=c('l', 'p'), 
       main="Subject Responses, Biss and Stai", auto.key = list(space="top"))

#looks like the Biss goes up incrementally and stai down incrementally
#Reflected in the Differences:
BBS2$BissDifference 
BBS2$StaiWidestDifference

#Plot Only Time 1 and 5
xyplot(Score~Timepoint, groups=c(Subject, Task), subset=(Timepoint==1| Timepoint==5) , data=BBSlong, type='p', 
       pch=25, cex=1.5, main="Subject Responses", auto.key = list(space="top"))

BBSfilt=filter(BBSlong, Timepoint=="1"| Timepoint=="5")
xyplot(Score~Timepoint|Task, groups=c(Subject), data=BBSfilt, type=c('l', 'p'), main="First and Last Day", 
       auto.key = list(space="top"), scales=list(alternating=FALSE), axis=axis.grid, layout=c(2,1))

##looking at POMs Data
str(bbsdf)
BBS3=bbsdf %>% select(starts_with("poms"))
#select tension
tension<- c('poms_tense', 'poms_shakey', 'poms_edge', 'poms_panicky', 
           'poms_uneasy', 'poms_restless', 
            'poms_nervous', 'poms_anxious', 'poms_stressed')
tminus='poms_relaxed'
tafter=paste(tension, "_after", sep="")
t2b=paste(tension, "_before_t2", sep="")
t2a=paste(tension, "_after_t2", sep="")
t3b=paste(tension, "_before_t3", sep="")
t3a=paste(tension, "_after_t3", sep="")

#make aggregrate Tension score columns
BBS3$tenscore1.1=BBS3 %>% select(tension) %>% rowSums() %>% -BBS3$poms_relaxed
BBS3$tenscore1.2=BBS3 %>% select(tafter) %>% rowSums() %>% -BBS3$poms_relaxed_after
BBS3$tenscore2.1=BBS3 %>% select(t2b) %>% rowSums() %>% -BBS3$poms_relaxed_before_t2
BBS3$tenscore2.2=BBS3 %>% select(t2a) %>% rowSums() %>% -BBS3$poms_relaxed_after_t2
BBS3$tenscore3.1=BBS3 %>% select(t3b) %>% rowSums() %>% -BBS3$poms_relaxed_before_t3
BBS3$tenscore3.2=BBS3 %>% select(t3a) %>% rowSums() %>% -BBS3$poms_relaxed_after_t3
#select depression
depression <- c('poms_unhappy', 'poms_sad', 'poms_hopeless', 'poms_unworthy', 
                'poms_discouraged', 'poms_lonely', 'poms_miserable', 
                'poms_gloomy', 'poms_desperate', 'poms_helpless', 
                'poms_worthless', 'poms_guilty', 'poms_regretful')
dafter=paste(depression, "_after", sep="")
d2b=paste(depression, "_before_t2", sep="")
d2a=paste(depression, "_after_t2", sep="")
d3b=paste(depression, "_before_t3", sep="")
d3a=paste(depression, "_after_t3", sep="")
#make aggregrate Depression score columns
BBS3$depscore1.1=BBS3 %>% select(depression) %>% rowSums()
BBS3$depscore1.2=BBS3 %>% select(dafter) %>% rowSums()
BBS3$depscore2.1=BBS3 %>% select(d2b) %>% rowSums()
BBS3$depscore2.2=BBS3 %>% select(d2a) %>% rowSums()
BBS3$depscore3.1=BBS3 %>% select(d3b) %>% rowSums()
BBS3$depscore3.2=BBS3 %>% select(d3a) %>% rowSums()
#select anger
anger<- c('poms_angry', 'poms_grouchy', 'poms_spiteful', 'poms_annoyed', 
          'poms_resentful', 'poms_bitter', 'poms_fight', 'poms_rebellious', 
          'poms_decceived', 'poms_furious', 'poms_badtempered')
aafter=paste(anger, "_after", sep="")
a2b=paste(anger, "_before_t2", sep="")
a2a=paste(anger, "_after_t2", sep="")
a3b=paste(anger, "_before_t3", sep="")
a3a=paste(anger, "_after_t3", sep="")
#make aggregrate Anger score columns
BBS3$angscore1.1=BBS3 %>% select(anger) %>% rowSums()
BBS3$angscore1.2=BBS3 %>% select(aafter) %>% rowSums()
BBS3$angscore2.1=BBS3 %>% select(a2b) %>% rowSums()
BBS3$angscore2.2=BBS3 %>% select(a2a) %>% rowSums()
BBS3$angscore3.1=BBS3 %>% select(a3b) %>% rowSums()
BBS3$angscore3.2=BBS3 %>% select(a3a) %>% rowSums()
#select fatigue
fatigue<- c('poms_wornout', 'poms_fatigue', 'poms_exhausted', 'poms_sluggish', 
             'poms_weary', 'poms_drained') 
fafter=paste(fatigue, "_after", sep="")
f2b=paste(fatigue, "_before_t2", sep="")
f2a=paste(fatigue, "_after_t2", sep="")
f3b=paste(fatigue, "_before_t3", sep="")
f3a=paste(fatigue, "_after_t3", sep="")
#make aggregrate Fatigue score columns
BBS3$fatscore1.1=BBS3 %>% select(fatigue) %>% rowSums()
BBS3$fatscore1.2=BBS3 %>% select(fafter) %>% rowSums()
BBS3$fatscore2.1=BBS3 %>% select(f2b) %>% rowSums()
BBS3$fatscore2.2=BBS3 %>% select(f2a) %>% rowSums()
BBS3$fatscore3.1=BBS3 %>% select(f3b) %>% rowSums()
BBS3$fatscore3.2=BBS3 %>% select(f3a) %>% rowSums()
#select confused
confused<- c('poms_confused', 'poms_concentrate', 'poms_muddle', 'poms_bewilder',
             'poms_forgetful', 'poms_uncertainaboutthings', 'poms_drained') 
cminus='poms_efficient'
cafter=paste(confused, "_after", sep="")
c2b=paste(confused, "_before_t2", sep="")
c2a=paste(confused, "_after_t2", sep="")
c3b=paste(confused, "_before_t3", sep="")
c3a=paste(confused, "_after_t3", sep="")
#make aggregate confused scores
BBS3$conscore1.1=BBS3 %>% select(confused) %>% rowSums() %>% -BBS3$poms_efficient
BBS3$conscore1.2=BBS3 %>% select(cafter) %>% rowSums() %>% -BBS3$poms_efficient_after
BBS3$conscore2.1=BBS3 %>% select(c2b) %>% rowSums() %>% -BBS3$poms_efficient_before_t2
BBS3$conscore2.2=BBS3 %>% select(c2a) %>% rowSums() %>% -BBS3$poms_efficient_after_t2
BBS3$conscore3.1=BBS3 %>% select(c3b) %>% rowSums() %>% -BBS3$poms_efficient_before_t3
BBS3$conscore3.2=BBS3 %>% select(c3a) %>% rowSums() %>% -BBS3$poms_efficient_after_t3
#select vigor
vigor<- c('poms_lively', 'poms_active', 'poms_energetic', 'poms_alert',
          'poms_carefree', 'poms_vigorous', 'poms_enthusiastic')
vafter=paste(vigor, "_after", sep="")
v2b=paste(vigor, "_before_t2", sep="")
v2a=paste(vigor, "_after_t2", sep="")
v3b=paste(vigor, "_before_t3", sep="")
v3a=paste(vigor, "_after_t3", sep="")
#make aggregrate Vigot score columns
BBS3$vigscore1.1=BBS3 %>% select(vigor) %>% rowSums()
BBS3$vigscore1.2=BBS3 %>% select(vafter) %>% rowSums()
BBS3$vigscore2.1=BBS3 %>% select(v2b) %>% rowSums()
BBS3$vigscore2.2=BBS3 %>% select(v2a) %>% rowSums()
BBS3$vigscore3.1=BBS3 %>% select(v3b) %>% rowSums()
BBS3$vigscore3.2=BBS3 %>% select(v3a) %>% rowSums()

#put all aggregates into new DF
POMS=BBS3 %>% select(contains('score'))
#Make the TMD score
POMS$TMD1.1<- POMS %>% select(tenscore1.1, depscore1.1, angscore1.1, fatscore1.1, 
                              conscore1.1) %>% rowSums() %>% -POMS$vigscore1.1
POMS$TMD1.2<- POMS %>% select(tenscore1.2, depscore1.2, angscore1.2, fatscore1.2, 
                              conscore1.2) %>% rowSums() %>% -POMS$vigscore1.2
POMS$TMD2.1<- POMS %>% select(tenscore2.1, depscore2.1, angscore2.1, fatscore2.1, 
                              conscore2.1) %>% rowSums() %>% -POMS$vigscore2.1
POMS$TMD2.2<- POMS %>% select(tenscore2.2, depscore2.2, angscore2.2, fatscore2.2, 
                              conscore2.2) %>% rowSums() %>% -POMS$vigscore2.2
POMS$TMD3.1<- POMS %>% select(tenscore3.1, depscore3.1, angscore3.1, fatscore3.1, 
                              conscore3.1) %>% rowSums() %>% -POMS$vigscore3.1
POMS$TMD3.2<- POMS %>% select(tenscore3.2, depscore3.2, angscore3.2, fatscore3.2, 
                              conscore3.2) %>% rowSums() %>% -POMS$vigscore3.2

#Make TMD difference each day
POMS$TMDDiffDay1=POMS$TMD1.2-POMS$TMD1.1
POMS$TMDDiffDay2=POMS$TMD2.2-POMS$TMD2.1
POMS$TMDDiffDay3=POMS$TMD3.2-POMS$TMD3.1
#Make TMD widest difference
POMS$TMDWidestDiff=POMS$TMD3.2-POMS$TMD1.1

#Print Difference DF
#Descriptives
POMS$Sub=levels(BBS2$subject_id)
Overall=c("Overall", mean(POMS$TMDWidestDiff, na.rm=TRUE), mean(POMS$TMDDiffDay1, na.rm=TRUE),
          mean(POMS$TMDDiffDay2, na.rm=TRUE), mean(POMS$TMDDiffDay3, na.rm=TRUE))
POMSDesc=subset(POMS, select=c(Sub, TMDWidestDiff, TMDDiffDay1, TMDDiffDay2, TMDDiffDay3))
POMSDesc=rbind(POMSDesc, Overall)
