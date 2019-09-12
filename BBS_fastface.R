#######Created for Analyzing BABS/BISS/STAI data over 2 sessions 
#for 2019 VM Pilot study, Fast Face Intervention
##Casey 09/11/2019

##Import Data
inputdir='/Users/casey/Desktop/navonanalysis-master/data/source/fast'
bbsdf=read.csv('/Users/casey/Desktop/navonanalysis-master/data/source/fast/BABS-BISS-STAI-formatted.csv')
str(bbsdf)
outputdir='/Users/casey/Desktop/navonanalysis-master/data/derivatives/fast'
numsubs=5 #total number subjects
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
(mean(BBS2$StaiWidestDifference))
#Stai Each Diff by day
BBS2$StaiDiffDay1=BBS2$stai_total_score_v2-BBS2$stai_total_score 
BBS2$StaiDiffDay2=BBS2$stai_total_score_v2_t2-BBS2$stai_total_score_t2 
BBS2$StaiDiffDay3=BBS2$stai_total_score_v2_t3-BBS2$stai_total_score_t3 

#export as tsv
write_tsv(emodf, file.path(outputdir, "task-babsbissstai_demographics.tsv"))

#Descriptives
Overall=c("Overall", mean(BBS2$BissDifference), mean(BBS2$StaiWidestDifference), mean(BBS2$StaiDiffDay1),
          mean(BBS2$StaiDiffDay2), mean(BBS2$StaiDiffDay3))
BBSDesc=subset(BBS2, select=c(subject_id, BissDifference, StaiWidestDifference, StaiDiffDay1, StaiDiffDay2,
                              StaiDiffDay3))
BBSDesc=rbind(BBSDesc, Overall)
BBSDesc$subject_id=ifelse(is.na(BBSDesc$subject_id), "Overall", BBSDesc$subject_id)

#Stai V2-V1 Anova
library(ez)
Stai=c(BBS2$StaiDiffDay1, BBS2$StaiDiffDay2, BBS2$StaiDiffDay3)
Staidf=tibble(subject_id=rep(BBS2$subject_id, 3), Stai=Stai, Day=rep(seq(1,3), each=numsubs))
str(Staidf)
Staidf$Day=factor(Staidf$Day)
#summary(lmer(Stai~Day+(1+Day|subject_id), data=Staidf))
modelStai=ezANOVA(dv=Stai, within=Day, wid=subject_id, data=Staidf)modelStai$Mauchly #fits model
modelStai$ANOVA #significant result according to the F Value but not p value
#doublechecked outputs with socscistatists.com/tests
#GES is the genearlized effect size
model2Stai=aov(Stai~Day+Error(subject_id/Day), data=Staidf)
summary(model2Stai)
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
time=rep(c(1,5,1,2,3,4,5,6),each= 5)
sub=rep(BBS2$subject_id, 8)
test=c(rep("Biss", 10), rep("Stai", 30))
BBSlong=tibble("Subject"=sub, "Task"=test, "Score"=forms, "Timepoint"=time)
str(BBSlong)
BBSlong$Task=factor(BBSlong$Task)
BBSlong$Time=factor(BBSlong$Time)
str(BBSlong)

#plot each person's Responses Over time
xyplot(Score~Timepoint, groups=Task, subset=Subject=="5014_2", data=BBSlong, type=c('l', 'p'), main="Subject1 Responses", auto.key = list(space="top"), axis=axis.grid)
xyplot(Score~Timepoint, groups=Task, subset=Subject=="5019_2", data=BBSlong, type=c('l', 'p'), main="Subject2 Responses", auto.key = list(space="top"), axis=axis.grid)
xyplot(Score~Timepoint, groups=Task, subset=Subject=="5020_2", data=BBSlong, type=c('l', 'p'), main="Subject3 Responses", auto.key = list(space="top"), axis=axis.grid)
xyplot(Score~Timepoint, groups=Task, subset=Subject=="5026_2", data=BBSlong, type=c('l', 'p'), main="Subject4 Responses", auto.key = list(space="top"), axis=axis.grid)
xyplot(Score~Timepoint, groups=Task, subset=Subject=="5031_2", data=BBSlong, type=c('l', 'p'), main="Subject5 Responses", auto.key = list(space="top"), axis=axis.grid)
xyplot(Score~Timepoint|Subject, groups=c(Task), layout=c(5,1), aspect=2, data=BBSlong, scales=list(alternating=FALSE), axis=axis.grid, type=c('l', 'p'), main="Subject Responses, Biss and Stai", auto.key = list(space="top"))

#looks like the Biss goes up incrementally and stai down incrementally
#Reflected in the Differences:
BBS2$BissDifference 


#Plot Only Time 1 and 5
BBSfilt=filter(BBSlong, Timepoint=="1"| Timepoint=="5")
xyplot(Score~Timepoint|Task, groups=c(Subject), data=BBSfilt, type=c('l', 'p'), main="First and Last Day", 
       auto.key = list(space="top"), scales=list(alternating=FALSE), axis=axis.grid, layout=c(2,1))

