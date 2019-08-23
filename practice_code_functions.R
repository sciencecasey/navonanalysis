#create a df of all the navon files
n <- data.frame(path=
                  list.files(pattern = 'navon_beh.csv', 
                             full.names = T, recursive = T), 
                stringsAsFactors = F)
#adds a row with subject as subj-####
dd =d %>% mutate(data=map(path, ~read_csv(.)), 
                 subject=str_split(path,'/',simplify=T)[,2], 
                 session=str_split(path,'/')[,3])
#change the subject number to remove 'subj-' field 
ddd=dd %>% mutate(subject=str_split(subject,'-',simplify=T)[,2],
                  session=str_split(session, '-')[,2])
#split dataframe to a list, with subject ID as the name of each element
files=setNames(split(ddd,seq(nrow(ddd))),ddd$subject)

#read individual csvs from the list
navon=data.frame(cbind(dd[[2]])) %>% unnest()


#if doing individual subjects
subj=50142
path=glue::glue('./sub-{subj}/ses-1/beh')
path
#ex==list files that start with the word "Product" followed by anything and ending with the word "xlsx":
#ex==list.files(pattern = "^Product(.*)xlsx$")
files=list.files(pattern="*navon(.*)csv$", recursive = T) #more specific than below
#files=list.files(pattern="*navon*", recursive = T)
navonlist=lapply(files, read.csv)
navondf=do.call(rbind, navonlist)
test="casey"
task="event"
glue('name{test}') %>%
  
#prompting for input
  
subject= function(){
  PID=readline(prompt = "Enter the subject number ")
  subj=return(PID)
}
subject()
