#to use, define task
#change the TASK code so that on 
#set the wd to the top level directory with all subjects source data inside 
#(ex: u/Casey/Desktop/working/TMS/beh/)
#insert the colnames for the IVs and DVs (ie: type, location; Stimuli.RT, Target.ACC)
#Inputs::
wd=""
taskname="navon"
files=list.files(path=wd, pattern="*TASK(.*)csv$", recursive = T, full.names = TRUE)
iv1="Configuration"
iv2="TargetLocation"
accuracyVar="Stimuli.ACC"
rtVar="Stimuli.RT"

#set all TASK files as a taskDF
tasklist=lapply(files, read.csv)
taskdf=do.call(rbind, tasklist)
str(taskdf)

#remove practice block if applicable
if (sum(taskdf$Block=="P") != 0) {
  taskdf=taskdf[!taskdf$Block=="P",]
  sum(taskdf$Block=="P")
  str(taskdf)}
  #}}
#check above functioned
ifelse (sum(taskdf$Block=="P") == 0, 
        glue('No practice block'),
        glue('practice block still present'))

#add row for task
taskdf$task=rep(print(taskname), length(taskdf$Subject)) #adds a column the same length as subject

#remove any unneccary variables
ivnames=glue('{iv1}, {iv2}')
taskdf= within(taskdf, subset(select= c(Subject, Session, task, get(ivnames), 
                                        Accuracy=glue('{accuracyVar}'), 
                                        ResponseTime=glue('{rtVar}'))))
