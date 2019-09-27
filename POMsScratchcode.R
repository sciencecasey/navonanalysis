##looking at POMs Data
str(bbsdf)
#titles=names(bbsdf)
#pomsindex<-titles[grepl("^poms_", titles)] #grab the titles that start with poms
#BBS3=subset(bbsdf, select = pomsindex)) #select only the cols that are within pomsindex vector
BBS3=bbsdf %>% select(starts_with("poms"))
BBS3$Tension.t1=BBS3$poms_tense+BBS3$poms_shakey+BBS3$poms_edge+BBS3$poms_panicky+BBS3$poms_relaxed+
  BBS3$poms_uneasy+BBS3$poms_uneasy+BBS3$poms_restless+BBS3$poms_nervous+BBS3$poms_anxious
#select tension
tension<- c('poms_tense', 'poms_shakey', 'poms_edge', 'poms_panicky', 
            'poms_relaxed', 'poms_uneasy', 'poms_restless', 
            'poms_nervous', 'poms_anxious', 'poms_stressed')
tafter=paste(tension, "_after", sep="")
t2b=paste(tension, "_before_t2", sep="")
t2a=paste(tension, "_after_t2", sep="")
t3b=paste(tension, "_before_t3", sep="")
t3a=paste(tension, "_after_t3", sep="")
#make aggregrate Tension score columns
BBS3$tenscore1.1=BBS3 %>% select(tension) %>% rowSums()
BBS3$tenscore1.2=BBS3 %>% select(tafter) %>% rowSums()
BBS3$tenscore2.1=BBS3 %>% select(t2b) %>% rowSums()
BBS3$tenscore2.2=BBS3 %>% select(t2a) %>% rowSums()
BBS3$tenscore3.1=BBS3 %>% select(t3b) %>% rowSums()
BBS3$tenscore3.2=BBS3 %>% select(t3a) %>% rowSums()

try=BBS3 %>% select(contains('score'))



ten=select(BBS3, c(tension, tafter, t2b, t2a, t3b, t3a))
ten$tension1.1=ten$
  
  tension=cbind(ten)
#select depression
depresion=c('poms_unhappy', 'poms_sad', 'poms_hopeless', 'poms_unworthy', 'poms_discouraged', 
            'poms_lonely', 'poms_miserable', 'poms_gloomy', 'poms_desperate', 'poms_helpless', 
            'poms_worthless', 'poms_guilty', 'poms_regretful')


ten=select(BBS3, tension)
#test=subset(BBS3, select(contains('tense|shaky|edge')))
#test=select(BBS3, contains(tense|shaky|edge)))
#BBS3[grepl("tense$|shaky$|edge$"), x=names(BBS3)]
#pomsbase="poms_tense, poms_shaky, poms_edge, poms_panicky, poms_relaxed, poms_uneasy,
#  poms_restless, poms_nervous, poms_anxious, poms_unhappy, poms_sad, poms_blue, poms_hopeless,
#  poms_unworthy, poms_discouraged, poms_lonely, poms_miserable, poms_gloomy, poms_desperate,
#  poms_helpless, poms_worthless, poms_terrified, poms_guilty, poms_anger, poms_peeved, poms_grouchy,
#  poms_spiteful, poms_annoyed, poms_resentful, poms_bitter, poms_fight, poms_rebellious, poms_deceived,
#  poms, furious, poms_badtempered, poms_wornout, poms_listless, poms_fatigued, poms_exhausted,
#  poms_sluggish, poms_weary, poms_confused, poms_concentrate, poms_muddled, poms_bewildered,
#  poms_efficient, poms_forgetful, poms_uncertainaboutthings, poms_lively, poms_active,
# poms_energetic, poms_cheerful, poms_alert, poms_carefree, poms_vigorous"
#pomsbase=paste(dQuote(sub(" ","",unlist(strsplit(pomsbase,split = ",")))),collapse = ", ")
#pombase=sapply(strsplit(pomsbase, ','), function(x) toString(dQuote(x)))



anger="poms_angry, poms_grouchy, poms_spiteful, poms_annoyed, poms_resentful, poms_bitter, 
          poms_fight, poms_rebellious, poms_decceived, poms_furious, poms_badtempered"
anger=paste(dQuote(sub(" ","",unlist(strsplit(anger,split = ",")))),collapse = ", ")
fatigue=poms_wornout, poms_fatigue, poms_exhausted, poms_sluggish, poms_weary, 
poms_confused, poms_concentrate, poms_muddle, poms_bewilder, 
poms_efficient, poms_forgetful, poms_uncertainaboutthings, poms_lively, poms_active, 
poms_energetic, poms_alert, poms_carefree, poms_vigorous)
