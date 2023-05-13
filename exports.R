library(haven)
library(ggplot2)
library(dplyr)
library(grf)
library(gridExtra)
library(stargazer)



########## baseline

# load model 

cs=readRDS("C:/Users/Public/MonjourYoucefi/models/baseline.Rds")
# compute projection given  a set of covariates
proj_base=best_linear_projection(cs,cs$X.orig[,c("tightness","AGE","SEXE", "NENF","NATION","MOBDIST","NIVFOR","CONTRAT2","CONTRAT3")])
# compute average treatment effect
ate_base=average_treatment_effect(cs)


#compute TOC for age education and tightness
toc_age=rank_average_treatment_effect(cs, cs$X.orig[,"AGE"])$TOC
toc_for=rank_average_treatment_effect(cs, cs$X.orig[,"NIVFOR"])$TOC
toc_tight=rank_average_treatment_effect(cs, cs$X.orig[,"tightness"])$TOC

#compute the 95% intervals
toc_age$lower=toc_age$estimate - 1.96*toc_age$std.err
toc_age$upper=toc_age$estimate + 1.96*toc_age$std.err


toc_for$lower=toc_for$estimate - 1.96*toc_for$std.err
toc_for$upper=toc_for$estimate + 1.96*toc_for$std.err


toc_tight$lower=toc_tight$estimate - 1.96*toc_tight$std.err
toc_tight$upper=toc_tight$estimate + 1.96*toc_tight$std.err




#plot it with ggplot
to_plot_toc=ggplot()+geom_line(data=toc_age, aes(x=q,y=estimate,color="Age")) +geom_line(data=toc_age, aes(x=q,y=lower,color="Age"),linetype="dotted")+
  geom_line(data=toc_age, aes(x=q,y=upper,color="Age"),linetype="dotted")+
  geom_line(data=toc_for, aes(x=q,y=estimate,color="Level of Education")) +geom_line(data=toc_for, aes(x=q,y=lower,color="Level of Education"),linetype="dotted")+
  geom_line(data=toc_for, aes(x=q,y=upper,color="Level of Education"),linetype="dotted")+
  geom_line(data=toc_tight, aes(x=q,y=estimate,color="Tightness")) +geom_line(data=toc_tight, aes(x=q,y=lower,color="Tightness"),linetype="dotted")+
  geom_line(data=toc_tight, aes(x=q,y=upper,color="Tightness"),linetype="dotted")+
  geom_hline(yintercept = 0,color="black")+scale_colour_manual(values=c("Age"="blue","Level of Education"="red","Tightness"="green"))+ylab("T.O.C.")


# save
ggsave("C:/Users/Public/MonjourYoucefi/toc_base.png")





#########without training days

cs=readRDS("C:/Users/Public/MonjourYoucefi/models/unlock.Rds")
proj_unlock=best_linear_projection(cs,cs$X.orig[,c("tightness","AGE","SEXE", "NENF","NATION","MOBDIST","NIVFOR","CONTRAT2","CONTRAT3")])
ate_unlock=average_treatment_effect(cs)
toc_age=rank_average_treatment_effect(cs, cs$X.orig[,"AGE"])$TOC
toc_for=rank_average_treatment_effect(cs, cs$X.orig[,"NIVFOR"])$TOC
toc_tight=rank_average_treatment_effect(cs, cs$X.orig[,"tightness"])$TOC


toc_age$lower=toc_age$estimate - 1.96*toc_age$std.err
toc_age$upper=toc_age$estimate + 1.96*toc_age$std.err


toc_for$lower=toc_for$estimate - 1.96*toc_for$std.err
toc_for$upper=toc_for$estimate + 1.96*toc_for$std.err


toc_tight$lower=toc_tight$estimate - 1.96*toc_tight$std.err
toc_tight$upper=toc_tight$estimate + 1.96*toc_tight$std.err





to_plot_toc=ggplot()+geom_line(data=toc_age, aes(x=q,y=estimate,color="Age")) +geom_line(data=toc_age, aes(x=q,y=lower,color="Age"),linetype="dotted")+
  geom_line(data=toc_age, aes(x=q,y=upper,color="Age"),linetype="dotted")+
  geom_line(data=toc_for, aes(x=q,y=estimate,color="Level of Education")) +geom_line(data=toc_for, aes(x=q,y=lower,color="Level of Education"),linetype="dotted")+
  geom_line(data=toc_for, aes(x=q,y=upper,color="Level of Education"),linetype="dotted")+
  geom_line(data=toc_tight, aes(x=q,y=estimate,color="Tightness")) +geom_line(data=toc_tight, aes(x=q,y=lower,color="Tightness"),linetype="dotted")+
  geom_line(data=toc_tight, aes(x=q,y=upper,color="Tightness"),linetype="dotted")+
  geom_hline(yintercept = 0,color="black")+scale_colour_manual(values=c("Age"="blue","Level of Education"="red","Tightness"="green"))+ylab("T.O.C.")



ggsave("C:/Users/Public/MonjourYoucefi/toc_unlock.png")

########## length of formation > 300 ######## hours

cs=readRDS("C:/Users/Public/MonjourYoucefi/models/greater300length_hours.Rds")
proj_longtrain=best_linear_projection(cs,cs$X.orig[,c("tightness","AGE","SEXE", "NENF","NATION","MOBDIST","NIVFOR","CONTRAT2","CONTRAT3")])
ate_long=average_treatment_effect(cs)

toc_age=rank_average_treatment_effect(cs, cs$X.orig[,"AGE"])$TOC
toc_for=rank_average_treatment_effect(cs, cs$X.orig[,"NIVFOR"])$TOC
toc_tight=rank_average_treatment_effect(cs, cs$X.orig[,"tightness"])$TOC


toc_age$lower=toc_age$estimate - 1.96*toc_age$std.err
toc_age$upper=toc_age$estimate + 1.96*toc_age$std.err


toc_for$lower=toc_for$estimate - 1.96*toc_for$std.err
toc_for$upper=toc_for$estimate + 1.96*toc_for$std.err


toc_tight$lower=toc_tight$estimate - 1.96*toc_tight$std.err
toc_tight$upper=toc_tight$estimate + 1.96*toc_tight$std.err





to_plot_toc=ggplot()+geom_line(data=toc_age, aes(x=q,y=estimate,color="Age")) +geom_line(data=toc_age, aes(x=q,y=lower,color="Age"),linetype="dotted")+
  geom_line(data=toc_age, aes(x=q,y=upper,color="Age"),linetype="dotted")+
  geom_line(data=toc_for, aes(x=q,y=estimate,color="Level of Education")) +geom_line(data=toc_for, aes(x=q,y=lower,color="Level of Education"),linetype="dotted")+
  geom_line(data=toc_for, aes(x=q,y=upper,color="Level of Education"),linetype="dotted")+
  geom_line(data=toc_tight, aes(x=q,y=estimate,color="Tightness")) +geom_line(data=toc_tight, aes(x=q,y=lower,color="Tightness"),linetype="dotted")+
  geom_line(data=toc_tight, aes(x=q,y=upper,color="Tightness"),linetype="dotted")+
  geom_hline(yintercept = 0,color="black")+scale_colour_manual(values=c("Age"="blue","Level of Education"="red","Tightness"="green"))+ylab("T.O.C.")



ggsave("C:/Users/Public/MonjourYoucefi/toc_long_train.png")



########## length of formation < 300 ######## hours


df=read_dta('C:/Users/ENSAE05_D_YOUCEFI/Desktop/short_training.dta')




cs=readRDS("C:/Users/Public/MonjourYoucefi/models/less300length_hours.Rds")
proj_shorttrain=best_linear_projection(cs,cs$X.orig[,c("tightness","AGE","SEXE", "NENF","NATION","MOBDIST","NIVFOR","CONTRAT2","CONTRAT3")])
ate_short=average_treatment_effect(cs)






toc_age=rank_average_treatment_effect(cs, cs$X.orig[,"AGE"])$TOC
toc_for=rank_average_treatment_effect(cs, cs$X.orig[,"NIVFOR"])$TOC
toc_tight=rank_average_treatment_effect(cs, cs$X.orig[,"tightness"])$TOC


toc_age$lower=toc_age$estimate - 1.96*toc_age$std.err
toc_age$upper=toc_age$estimate + 1.96*toc_age$std.err


toc_for$lower=toc_for$estimate - 1.96*toc_for$std.err
toc_for$upper=toc_for$estimate + 1.96*toc_for$std.err


toc_tight$lower=toc_tight$estimate - 1.96*toc_tight$std.err
toc_tight$upper=toc_tight$estimate + 1.96*toc_tight$std.err





to_plot_toc=ggplot()+geom_line(data=toc_age, aes(x=q,y=estimate,color="Age")) +geom_line(data=toc_age, aes(x=q,y=lower,color="Age"),linetype="dotted")+
  geom_line(data=toc_age, aes(x=q,y=upper,color="Age"),linetype="dotted")+
  geom_line(data=toc_for, aes(x=q,y=estimate,color="Level of Education")) +geom_line(data=toc_for, aes(x=q,y=lower,color="Level of Education"),linetype="dotted")+
  geom_line(data=toc_for, aes(x=q,y=upper,color="Level of Education"),linetype="dotted")+
  geom_line(data=toc_tight, aes(x=q,y=estimate,color="Tightness")) +geom_line(data=toc_tight, aes(x=q,y=lower,color="Tightness"),linetype="dotted")+
  geom_line(data=toc_tight, aes(x=q,y=upper,color="Tightness"),linetype="dotted")+
  geom_hline(yintercept = 0,color="black")+scale_colour_manual(values=c("Age"="blue","Level of Education"="red","Tightness"="green"))+ylab("T.O.C.")



ggsave("C:/Users/Public/MonjourYoucefi/short_train.png")





############length of formation >60 days

df=read_dta('C:/Users/ENSAE05_D_YOUCEFI/Desktop/long_spread.dta')



cs=readRDS("C:/Users/Public/MonjourYoucefi/models/long_spread.Rds")
proj_longspread=best_linear_projection(cs,cs$X.orig[,c("tightness","AGE","SEXE", "NENF","NATION","MOBDIST","NIVFOR","CONTRAT2","CONTRAT3")])
ate_longspread=average_treatment_effect(cs)





toc_age=rank_average_treatment_effect(cs, cs$X.orig[,"AGE"])$TOC
toc_for=rank_average_treatment_effect(cs, cs$X.orig[,"NIVFOR"])$TOC
toc_tight=rank_average_treatment_effect(cs, cs$X.orig[,"tightness"])$TOC


toc_age$lower=toc_age$estimate - 1.96*toc_age$std.err
toc_age$upper=toc_age$estimate + 1.96*toc_age$std.err


toc_for$lower=toc_for$estimate - 1.96*toc_for$std.err
toc_for$upper=toc_for$estimate + 1.96*toc_for$std.err


toc_tight$lower=toc_tight$estimate - 1.96*toc_tight$std.err
toc_tight$upper=toc_tight$estimate + 1.96*toc_tight$std.err





to_plot_toc=ggplot()+geom_line(data=toc_age, aes(x=q,y=estimate,color="Age")) +geom_line(data=toc_age, aes(x=q,y=lower,color="Age"),linetype="dotted")+
  geom_line(data=toc_age, aes(x=q,y=upper,color="Age"),linetype="dotted")+
  geom_line(data=toc_for, aes(x=q,y=estimate,color="Level of Education")) +geom_line(data=toc_for, aes(x=q,y=lower,color="Level of Education"),linetype="dotted")+
  geom_line(data=toc_for, aes(x=q,y=upper,color="Level of Education"),linetype="dotted")+
  geom_line(data=toc_tight, aes(x=q,y=estimate,color="Tightness")) +geom_line(data=toc_tight, aes(x=q,y=lower,color="Tightness"),linetype="dotted")+
  geom_line(data=toc_tight, aes(x=q,y=upper,color="Tightness"),linetype="dotted")+
  geom_hline(yintercept = 0,color="black")+scale_colour_manual(values=c("Age"="blue","Level of Education"="red","Tightness"="green"))+ylab("T.O.C.")



ggsave("C:/Users/Public/MonjourYoucefi/toc_long_spread.png")


########## length of formation < 60 ######## days


df=read_dta('C:/Users/ENSAE05_D_YOUCEFI/Desktop/short_spread.dta')




cs=readRDS("C:/Users/Public/MonjourYoucefi/models/short_spread.Rds")
proj_shortspread=best_linear_projection(cs,cs$X.orig[,c("tightness","AGE","SEXE", "NENF","NATION","MOBDIST","NIVFOR","CONTRAT2","CONTRAT3")])
ate_shortspread=average_treatment_effect(cs)


toc_age=rank_average_treatment_effect(cs, cs$X.orig[,"AGE"])$TOC
toc_for=rank_average_treatment_effect(cs, cs$X.orig[,"NIVFOR"])$TOC
toc_tight=rank_average_treatment_effect(cs, cs$X.orig[,"tightness"])$TOC


toc_age$lower=toc_age$estimate - 1.96*toc_age$std.err
toc_age$upper=toc_age$estimate + 1.96*toc_age$std.err


toc_for$lower=toc_for$estimate - 1.96*toc_for$std.err
toc_for$upper=toc_for$estimate + 1.96*toc_for$std.err


toc_tight$lower=toc_tight$estimate - 1.96*toc_tight$std.err
toc_tight$upper=toc_tight$estimate + 1.96*toc_tight$std.err





to_plot_toc=ggplot()+geom_line(data=toc_age, aes(x=q,y=estimate,color="Age")) +geom_line(data=toc_age, aes(x=q,y=lower,color="Age"),linetype="dotted")+
  geom_line(data=toc_age, aes(x=q,y=upper,color="Age"),linetype="dotted")+
  geom_line(data=toc_for, aes(x=q,y=estimate,color="Level of Education")) +geom_line(data=toc_for, aes(x=q,y=lower,color="Level of Education"),linetype="dotted")+
  geom_line(data=toc_for, aes(x=q,y=upper,color="Level of Education"),linetype="dotted")+
  geom_line(data=toc_tight, aes(x=q,y=estimate,color="Tightness")) +geom_line(data=toc_tight, aes(x=q,y=lower,color="Tightness"),linetype="dotted")+
  geom_line(data=toc_tight, aes(x=q,y=upper,color="Tightness"),linetype="dotted")+
  geom_hline(yintercept = 0,color="black")+scale_colour_manual(values=c("Age"="blue","Level of Education"="red","Tightness"="green"))+ylab("T.O.C.")



ggsave("C:/Users/Public/MonjourYoucefi/short_spread.png")


##### censoring



cs=readRDS("C:/Users/Public/MonjourYoucefi/models/censor.Rds")
proj_censor=best_linear_projection(cs,cs$X.orig[,c("tightness","AGE","SEXE", "NENF","NATION","MOBDIST","NIVFOR","CONTRAT2","CONTRAT3")])
ate_censor=average_treatment_effect(cs)


toc_age=rank_average_treatment_effect(cs, cs$X.orig[,"AGE"])$TOC
toc_for=rank_average_treatment_effect(cs, cs$X.orig[,"NIVFOR"])$TOC
toc_tight=rank_average_treatment_effect(cs, cs$X.orig[,"tightness"])$TOC


toc_age$lower=toc_age$estimate - 1.96*toc_age$std.err
toc_age$upper=toc_age$estimate + 1.96*toc_age$std.err


toc_for$lower=toc_for$estimate - 1.96*toc_for$std.err
toc_for$upper=toc_for$estimate + 1.96*toc_for$std.err


toc_tight$lower=toc_tight$estimate - 1.96*toc_tight$std.err
toc_tight$upper=toc_tight$estimate + 1.96*toc_tight$std.err





to_plot_toc=ggplot()+geom_line(data=toc_age, aes(x=q,y=estimate,color="Age")) +geom_line(data=toc_age, aes(x=q,y=lower,color="Age"),linetype="dotted")+
  geom_line(data=toc_age, aes(x=q,y=upper,color="Age"),linetype="dotted")+
  geom_line(data=toc_for, aes(x=q,y=estimate,color="Level of Education")) +geom_line(data=toc_for, aes(x=q,y=lower,color="Level of Education"),linetype="dotted")+
  geom_line(data=toc_for, aes(x=q,y=upper,color="Level of Education"),linetype="dotted")+
  geom_line(data=toc_tight, aes(x=q,y=estimate,color="Tightness")) +geom_line(data=toc_tight, aes(x=q,y=lower,color="Tightness"),linetype="dotted")+
  geom_line(data=toc_tight, aes(x=q,y=upper,color="Tightness"),linetype="dotted")+
  geom_hline(yintercept = 0,color="black")+scale_colour_manual(values=c("Age"="blue","Level of Education"="red","Tightness"="green"))+ylab("T.O.C.")



ggsave("C:/Users/Public/MonjourYoucefi/censor.png")

#############removing training days for short and long training

#######training >300 hours



cs=readRS(cs,file="C:/Users/Public/MonjourYoucefi/models/greater300length_hours_unlock.Rds")



proj_long_train_unlock=best_linear_projection(cs,cs$X.orig[,c("tightness","AGE","SEXE", "NENF","NATION","MOBDIST","NIVFOR","CONTRAT2","CONTRAT3")])
ate_long_train_unlock=average_treatment_effect(cs)


toc_age=rank_average_treatment_effect(cs, cs$X.orig[,"AGE"])$TOC
toc_for=rank_average_treatment_effect(cs, cs$X.orig[,"NIVFOR"])$TOC
toc_tight=rank_average_treatment_effect(cs, cs$X.orig[,"tightness"])$TOC


toc_age$lower=toc_age$estimate - 1.96*toc_age$std.err
toc_age$upper=toc_age$estimate + 1.96*toc_age$std.err


toc_for$lower=toc_for$estimate - 1.96*toc_for$std.err
toc_for$upper=toc_for$estimate + 1.96*toc_for$std.err


toc_tight$lower=toc_tight$estimate - 1.96*toc_tight$std.err
toc_tight$upper=toc_tight$estimate + 1.96*toc_tight$std.err





to_plot_toc=ggplot()+geom_line(data=toc_age, aes(x=q,y=estimate,color="Age")) +geom_line(data=toc_age, aes(x=q,y=lower,color="Age"),linetype="dotted")+
  geom_line(data=toc_age, aes(x=q,y=upper,color="Age"),linetype="dotted")+
  geom_line(data=toc_for, aes(x=q,y=estimate,color="Level of Education")) +geom_line(data=toc_for, aes(x=q,y=lower,color="Level of Education"),linetype="dotted")+
  geom_line(data=toc_for, aes(x=q,y=upper,color="Level of Education"),linetype="dotted")+
  geom_line(data=toc_tight, aes(x=q,y=estimate,color="Tightness")) +geom_line(data=toc_tight, aes(x=q,y=lower,color="Tightness"),linetype="dotted")+
  geom_line(data=toc_tight, aes(x=q,y=upper,color="Tightness"),linetype="dotted")+
  geom_hline(yintercept = 0,color="black")+scale_colour_manual(values=c("Age"="blue","Level of Education"="red","Tightness"="green"))+ylab("T.O.C.")



ggsave("C:/Users/Public/MonjourYoucefi/long_train_unlock.png")




#####training <300 hours


cs=readRS(cs,file="C:/Users/Public/MonjourYoucefi/models/less300length_hours_unlock.Rds")









proj_short_train_unlock=best_linear_projection(cs,cs$X.orig[,c("tightness","AGE","SEXE", "NENF","NATION","MOBDIST","NIVFOR","CONTRAT2","CONTRAT3")])
ate_short_train_unlock=average_treatment_effect(cs)


toc_age=rank_average_treatment_effect(cs, cs$X.orig[,"AGE"])$TOC
toc_for=rank_average_treatment_effect(cs, cs$X.orig[,"NIVFOR"])$TOC
toc_tight=rank_average_treatment_effect(cs, cs$X.orig[,"tightness"])$TOC


toc_age$lower=toc_age$estimate - 1.96*toc_age$std.err
toc_age$upper=toc_age$estimate + 1.96*toc_age$std.err


toc_for$lower=toc_for$estimate - 1.96*toc_for$std.err
toc_for$upper=toc_for$estimate + 1.96*toc_for$std.err


toc_tight$lower=toc_tight$estimate - 1.96*toc_tight$std.err
toc_tight$upper=toc_tight$estimate + 1.96*toc_tight$std.err





to_plot_toc=ggplot()+geom_line(data=toc_age, aes(x=q,y=estimate,color="Age")) +geom_line(data=toc_age, aes(x=q,y=lower,color="Age"),linetype="dotted")+
  geom_line(data=toc_age, aes(x=q,y=upper,color="Age"),linetype="dotted")+
  geom_line(data=toc_for, aes(x=q,y=estimate,color="Level of Education")) +geom_line(data=toc_for, aes(x=q,y=lower,color="Level of Education"),linetype="dotted")+
  geom_line(data=toc_for, aes(x=q,y=upper,color="Level of Education"),linetype="dotted")+
  geom_line(data=toc_tight, aes(x=q,y=estimate,color="Tightness")) +geom_line(data=toc_tight, aes(x=q,y=lower,color="Tightness"),linetype="dotted")+
  geom_line(data=toc_tight, aes(x=q,y=upper,color="Tightness"),linetype="dotted")+
  geom_hline(yintercept = 0,color="black")+scale_colour_manual(values=c("Age"="blue","Level of Education"="red","Tightness"="green"))+ylab("T.O.C.")



ggsave("C:/Users/Public/MonjourYoucefi/short_train_unlock.png")