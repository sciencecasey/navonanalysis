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
#titles=names(bbsdf)
#pomsindex<-titles[grepl("^poms_", titles)] #grab the titles that start with poms
#BBS3=subset(bbsdf, select = pomsindex)) #select only the cols that are within pomsindex vector
BBS3=bbsdf %>% select(starts_with("poms"))
BBS3$Tension.t1=BBS3$poms_tense+BBS3$poms_shakey+BBS3$poms_edge+BBS3$poms_panicky+BBS3$poms_relaxed+
  BBS3$poms_uneasy+BBS3$poms_uneasy+BBS3$poms_restless+BBS3$poms_nervous+BBS3$poms_anxious
test=BBS3 %>% select(ends_with("tense"|"shakey"|"edge"))
pomsbase="poms_tense, poms_shaky, poms_edge, poms_panicky, poms_relaxed, poms_uneasy,
  poms_restless, poms_nervous, poms_anxious, poms_unhappy, poms_sad, poms_blue, poms_hopeless,
  poms_unworthy, poms_discouraged, poms_lonely, poms_miserable, poms_gloomy, poms_desperate,
  poms_helpless, poms_worthless, poms_terrified, poms_guilty, poms_anger, poms_peeved, poms_grouchy,
  poms_spiteful, poms_annoyed, poms_resentful, poms_bitter, poms_fight, poms_rebellious, poms_deceived,
  poms, furious, poms_badtempered, poms_wornout, poms_listless, poms_fatigued, poms_exhausted,
  poms_sluggish, poms_weary, poms_confused, poms_concentrate, poms_muddled, poms_bewildered,
  poms_efficient, poms_forgetful, poms_uncertainaboutthings, poms_lively, poms_active,
  poms_energetic, poms_cheerful, poms_alert, poms_carefree, poms_vigorous"
pomsbase=paste(dQuote(sub(" ","",unlist(strsplit(pomsbase,split = ",")))),collapse = ", ")
pombase=sapply(strsplit(pomsbase, ','), function(x) toString(dQuote(x)))
tension=c(“poms_tense”, “poms_shaky”, “poms_edge”, “poms_panicky”, “poms_relaxed”, “poms_uneasy”, “poms_restless”, “poms_nervous”, “poms_anxious”)
depresion=c(“poms_unhappy”, “poms_sad”, “poms_blue”, “poms_hopeless”, “poms_unworthy”, “poms_discouraged”, “poms_lonely”, “poms_miserable”, “poms_gloomy”, “poms_desperate”, “poms_helpless”, “poms_worthless”, “poms_terrified”, “poms_guilty”)
anger="poms_angry, poms_grouchy, poms_spiteful, poms_annoyed, poms_resentful, poms_bitter, 
          poms_fight, poms_rebellious, poms_decceived, poms_furious, poms_badtempered"
anger=paste(dQuote(sub(" ","",unlist(strsplit(anger,split = ",")))),collapse = ", ")
fatigue=poms_wornout, poms_fatigue, poms_exhausted, poms_sluggish, poms_weary, 
poms_confused, poms_concentrate, poms_muddle, poms_bewilder, 
           poms_efficient, poms_forgetful, poms_uncertainaboutthings, poms_lively, poms_active, 
           poms_energetic, poms_alert, poms_carefree, poms_vigorous)

