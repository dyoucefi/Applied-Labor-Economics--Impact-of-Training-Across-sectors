library(haven)
library(ggplot2)
library(dplyr)
library(grf)
library(gridExtra)
library(stargazer)

df= read_dta('C:/Users/ENSAE05_D_YOUCEFI/Desktop/final_tight_des.dta')
#df=read_dta('C:/Users/ENSAE05_M_MONJOUR/Desktop/Project3/final_tight_des.dta')



###### put in a date format

df$DATE_ENTREE=as.Date(df$DATE_ENTREE,"%Y/%m/%d")
df$DATE_FIN=as.Date(df$DATE_FIN,"%Y/%m/%d")
df$DATINS=as.Date(df$DATINS,"%Y/%m/%d")
df$DATANN=as.Date(df$DATANN,"%Y/%m/%d")
df$debut_contrat=as.Date(df$debut_contrat,"%Y/%m/%d")



## put formation into levels

df[df$NIVFOR=="NV4",]$NIVFOR="2"
df[df$NIVFOR=="NV2",]$NIVFOR="4"
df[df$NIVFOR=="NV1",]$NIVFOR="5"
df[df$NIVFOR=="NV3",]$NIVFOR="3"
df[df$NIVFOR=="NV5",]$NIVFOR="1"
df[df$NIVFOR%in%c("C12","AFS","CFG","CP4","C3A"),]$NIVFOR="0"
df$NIVFOR=as.numeric(df$NIVFOR)

df$NATION=as.numeric(df$NATION)
df[is.na(df$NATION),]$NATION=0
df[df$NATION !=1,]$NATION=0





df$T_emploi=df$debut_contrat-df$DATINS


df$T_emploi=as.numeric(df$T_emploi)
df$SITMAT=as.factor(df$SITMAT)


df=df[df$MOBUNIT=="KM",]

df$ROME=substring( df$ROME,1,1)
### create a dummy for gender
df$SEXE=as.numeric( df$SEXE)-1

df$NENF=as.numeric( df$NENF)

df$ROME=factor( df$ROME)

df$AGE=as.numeric( df$AGE)

df$MOBDIST=as.numeric( df$MOBDIST)
df$MOTINS=factor(df$MOTINS)

df$CONTRAT=factor(df$CONTRAT)

df$tightness=as.numeric(df$tightness)
#df_est=subset(df_est,select=-c(MOBUNIT , DATINS , DATANN , MOBUNIT , debut_contrat , Nature , salaire_base_mois_complet , CP , SIRET ,ROME,A10,A21,A10_ET,A21,ET,ACTIF_NAT_ET,APEN,APET,NAF_des, Code_ROME, Code_NAF))




########## regular estimation


#####no censoring and subsetting for the covariates chosen
df_est=subset(df, select= c(Region,SEXE, NENF,T_emploi,NATION, SITMAT,MOTINS,formation,AGE,tightness,MOBDIST,NIVFOR, CONTRAT))
df_est=na.omit(df_est)
df_est$censor=1

X=subset(df_est,select=-c(censor,T_emploi,formation))


X=model.matrix(~ 0 +.,X)
Y=as.vector(df_est$T_emploi)
W=as.vector(df_est$formation)
D=as.vector(df_est$censor)


cs=causal_survival_forest(X,Y,W,D,horizon=1000)

#saveRDS(cs,file="C:/Users/ENSAE05_D_YOUCEFI/Desktop/models/baseline.Rds")
saveRDS(cs,file="C:/Users/Public/MonjourYoucefi/models/baseline.Rds")
average_treatment_effect(cs)





###################Robustness check 1- remove de training duration


df= read_dta('C:/Users/ENSAE05_D_YOUCEFI/Desktop/final_tight_des.dta')

df$DATE_ENTREE=as.Date(df$DATE_ENTREE,"%Y/%m/%d")
df$DATE_FIN=as.Date(df$DATE_FIN,"%Y/%m/%d")
df$DATINS=as.Date(df$DATINS,"%Y/%m/%d")
df$DATANN=as.Date(df$DATANN,"%Y/%m/%d")
df$debut_contrat=as.Date(df$debut_contrat,"%Y/%m/%d")





df[df$NIVFOR=="NV4",]$NIVFOR="2"
df[df$NIVFOR=="NV2",]$NIVFOR="4"
df[df$NIVFOR=="NV1",]$NIVFOR="5"
df[df$NIVFOR=="NV3",]$NIVFOR="3"
df[df$NIVFOR=="NV5",]$NIVFOR="1"
df[df$NIVFOR%in%c("C12","AFS","CFG","CP4","C3A"),]$NIVFOR="0"
df$NIVFOR=as.numeric(df$NIVFOR)

df$NATION=as.numeric(df$NATION)
df[is.na(df$NATION),]$NATION=0
df[df$NATION !=1,]$NATION=0





df$T_emploi=df$debut_contrat-df$DATINS


df$T_emploi=as.numeric(df$T_emploi)
df$SITMAT=as.factor(df$SITMAT)



df=df[df$MOBUNIT=="KM",]

df$ROME=substring( df$ROME,1,1)

df$SEXE=as.numeric( df$SEXE)-1

df$NENF=as.numeric( df$NENF)

df$ROME=factor( df$ROME)

df$AGE=as.numeric( df$AGE)

df$MOBDIST=as.numeric( df$MOBDIST)
df$MOTINS=factor(df$MOTINS)

df$CONTRAT=factor(df$CONTRAT)

df$tightness=as.numeric(df$tightness)

df_unlock=data.frame(df)

df_unlock$duree_form=df_unlock$DATE_FIN-df_unlock$DATE_ENTREE
df_unlock$duree_form=as.numeric(df_unlock$duree_form)
df_unlock[is.na(df_unlock$duree_form),]$duree_form=0

df_unlock$duree_rech=df_unlock$T_emploi-df_unlock$duree_form



df_est=subset(df_unlock, select= c(Region,SEXE, NENF,NATION,SITMAT, duree_rech,formation,AGE,tightness,MOBDIST,NIVFOR, CONTRAT))
df_est=na.omit(df_est)
df_est$censor=1

df_est=df_est[df_est$duree_rech>0,]

X=subset(df_est,select=-c(censor,duree_rech,formation))

X=model.matrix(~ 0 +.,X)
Y=as.vector(df_est$duree_rech)
W=as.vector(df_est$formation)
D=as.vector(df_est$censor)


cs_unlock=causal_survival_forest(X,Y,W,D,horizon=1000)

average_treatment_effect(cs_unlock)
best_linear_projection(cs_unlock,X)

best_linear_projection(cs_unlock,X[,"tightness"])
plot(rank_average_treatment_effect(cs_unlock, X[,"tightness"]))
#saveRDS(cs_unlock,file="C:/Users/ENSAE05_D_YOUCEFI/Desktop/models/unlock.Rds")
saveRDS(cs_unlock,file="C:/Users/Public/MonjourYoucefi/models/unlock.Rds")
############### Robustness check 2- Censoring Data

##### censoring time
T_censor=200



df= read_dta('C:/Users/ENSAE05_D_YOUCEFI/Desktop/final_tight_des.dta')

df$DATE_ENTREE=as.Date(df$DATE_ENTREE,"%Y/%m/%d")
df$DATE_FIN=as.Date(df$DATE_FIN,"%Y/%m/%d")
df$DATINS=as.Date(df$DATINS,"%Y/%m/%d")
df$DATANN=as.Date(df$DATANN,"%Y/%m/%d")
df$debut_contrat=as.Date(df$debut_contrat,"%Y/%m/%d")





df[df$NIVFOR=="NV4",]$NIVFOR="2"
df[df$NIVFOR=="NV2",]$NIVFOR="4"
df[df$NIVFOR=="NV1",]$NIVFOR="5"
df[df$NIVFOR=="NV3",]$NIVFOR="3"
df[df$NIVFOR=="NV5",]$NIVFOR="1"
df[df$NIVFOR%in%c("C12","AFS","CFG","CP4","C3A"),]$NIVFOR="0"
df$NIVFOR=as.numeric(df$NIVFOR)

df$NATION=as.numeric(df$NATION)
df[is.na(df$NATION),]$NATION=0
df[df$NATION !=1,]$NATION=0





df$T_emploi=df$debut_contrat-df$DATINS


df$T_emploi=as.numeric(df$T_emploi)
df$SITMAT=as.factor(df$SITMAT)



df=df[df$MOBUNIT=="KM",]

df$ROME=substring( df$ROME,1,1)

df$SEXE=as.numeric( df$SEXE)-1

df$NENF=as.numeric( df$NENF)

df$ROME=factor( df$ROME)

df$AGE=as.numeric( df$AGE)

df$MOBDIST=as.numeric( df$MOBDIST)
df$MOTINS=factor(df$MOTINS)

df$CONTRAT=factor(df$CONTRAT)

df$tightness=as.numeric(df$tightness)


df_est=subset(df, select= c(Region,SEXE, NENF,T_emploi,NATION, SITMAT,MOTINS,formation,AGE,tightness,MOBDIST,NIVFOR, CONTRAT))
df_est=na.omit(df_est)
df_est$censor=1

df_est[df_est$T_emploi>=T_censor,]$censor=0

X=subset(df_est,select=-c(censor,T_emploi,formation))

#X=subset(df_est,select=-c(censor,T_emploi,formation,MOTINS,CONTRAT))

X=model.matrix(~ 0 +.,X)
Y=as.vector(df_est$T_emploi)
W=as.vector(df_est$formation)
D=as.vector(df_est$censor)


cs_censor=causal_survival_forest(X,Y,W,D,horizon=1000)

#saveRDS(cs_censor,file="C:/Users/ENSAE05_D_YOUCEFI/Desktop/models/censor.Rds")
saveRDS(cs_censor,file="C:/Users/Public/MonjourYoucefi/models/censor.Rds")

######################## Robustness 3 -restricting at different type of formation



########## length of formation > 60 days ######## 

df=read_dta('C:/Users/ENSAE05_D_YOUCEFI/Desktop/long_spread.dta')



df$DATE_ENTREE=as.Date(df$DATE_ENTREE,"%Y/%m/%d")
df$DATE_FIN=as.Date(df$DATE_FIN,"%Y/%m/%d")
df$DATINS=as.Date(df$DATINS,"%Y/%m/%d")
df$DATANN=as.Date(df$DATANN,"%Y/%m/%d")
df$debut_contrat=as.Date(df$debut_contrat,"%Y/%m/%d")





df[df$NIVFOR=="NV4",]$NIVFOR="2"
df[df$NIVFOR=="NV2",]$NIVFOR="4"
df[df$NIVFOR=="NV1",]$NIVFOR="5"
df[df$NIVFOR=="NV3",]$NIVFOR="3"
df[df$NIVFOR=="NV5",]$NIVFOR="1"
df[df$NIVFOR%in%c("C12","AFS","CFG","CP4","C3A"),]$NIVFOR="0"
df$NIVFOR=as.numeric(df$NIVFOR)

df$NATION=as.numeric(df$NATION)
df[is.na(df$NATION),]$NATION=0
df[df$NATION !=1,]$NATION=0





df$T_emploi=df$debut_contrat-df$DATINS


df$T_emploi=as.numeric(df$T_emploi)
df$SITMAT=as.factor(df$SITMAT)



df=df[df$MOBUNIT=="KM",]

df$ROME=substring( df$ROME,1,1)

df$SEXE=as.numeric( df$SEXE)-1

df$NENF=as.numeric( df$NENF)

df$ROME=factor( df$ROME)

df$AGE=as.numeric( df$AGE)

df$MOBDIST=as.numeric( df$MOBDIST)
df$MOTINS=factor(df$MOTINS)

df$CONTRAT=factor(df$CONTRAT)

df$tightness=as.numeric(df$tightness)





df_est=subset(df, select= c(Region,SEXE, NENF,T_emploi,NATION, SITMAT,MOTINS,formation,AGE,tightness,MOBDIST,NIVFOR, CONTRAT))
df_est=na.omit(df_est)
df_est$censor=1

X=subset(df_est,select=-c(censor,T_emploi,formation))


X=model.matrix(~ 0 +.,X)
Y=as.vector(df_est$T_emploi)
W=as.vector(df_est$formation)
D=as.vector(df_est$censor)


cs_plus300days=causal_survival_forest(X,Y,W,D,horizon=1000)

#saveRDS(cs_plus300days,file="C:/Users/ENSAE05_D_YOUCEFI/Desktop/models/greater300length_days.Rds")
saveRDS(cs_plus300days,file="C:/Users/Public/MonjourYoucefi/models/long_spread.Rds")
average_treatment_effect(cs_plus300days)



########## length of formation < 60 ######## days

df=read_dta('C:/Users/ENSAE05_D_YOUCEFI/Desktop/short_spread.dta')



df$DATE_ENTREE=as.Date(df$DATE_ENTREE,"%Y/%m/%d")
df$DATE_FIN=as.Date(df$DATE_FIN,"%Y/%m/%d")
df$DATINS=as.Date(df$DATINS,"%Y/%m/%d")
df$DATANN=as.Date(df$DATANN,"%Y/%m/%d")
df$debut_contrat=as.Date(df$debut_contrat,"%Y/%m/%d")





df[df$NIVFOR=="NV4",]$NIVFOR="2"
df[df$NIVFOR=="NV2",]$NIVFOR="4"
df[df$NIVFOR=="NV1",]$NIVFOR="5"
df[df$NIVFOR=="NV3",]$NIVFOR="3"
df[df$NIVFOR=="NV5",]$NIVFOR="1"
df[df$NIVFOR%in%c("C12","AFS","CFG","CP4","C3A"),]$NIVFOR="0"
df$NIVFOR=as.numeric(df$NIVFOR)

df$NATION=as.numeric(df$NATION)
df[is.na(df$NATION),]$NATION=0
df[df$NATION !=1,]$NATION=0





df$T_emploi=df$debut_contrat-df$DATINS


df$T_emploi=as.numeric(df$T_emploi)
df$SITMAT=as.factor(df$SITMAT)



df=df[df$MOBUNIT=="KM",]

df$ROME=substring( df$ROME,1,1)

df$SEXE=as.numeric( df$SEXE)-1

df$NENF=as.numeric( df$NENF)

df$ROME=factor( df$ROME)

df$AGE=as.numeric( df$AGE)

df$MOBDIST=as.numeric( df$MOBDIST)
df$MOTINS=factor(df$MOTINS)

df$CONTRAT=factor(df$CONTRAT)

df$tightness=as.numeric(df$tightness)







df_est=subset(df, select= c(Region,SEXE, NENF,T_emploi,NATION, SITMAT,MOTINS,formation,AGE,tightness,MOBDIST,NIVFOR, CONTRAT))
df_est=na.omit(df_est)
df_est$censor=1

X=subset(df_est,select=-c(censor,T_emploi,formation))


X=model.matrix(~ 0 +.,X)
Y=as.vector(df_est$T_emploi)
W=as.vector(df_est$formation)
D=as.vector(df_est$censor)


cs_less300=causal_survival_forest(X,Y,W,D,horizon=1000)

#saveRDS(cs_less300,file="C:/Users/ENSAE05_D_YOUCEFI/Desktop/models/less300length_days.Rds")
saveRDS(cs_less300,file="C:/Users/Public/MonjourYoucefi/models/short_spread.Rds")
average_treatment_effect(cs_less300)



















########## length of formation > 300 ######## hours


df=read_dta('C:/Users/ENSAE05_D_YOUCEFI/Desktop/long_training.dta')



df$DATE_ENTREE=as.Date(df$DATE_ENTREE,"%Y/%m/%d")
df$DATE_FIN=as.Date(df$DATE_FIN,"%Y/%m/%d")
df$DATINS=as.Date(df$DATINS,"%Y/%m/%d")
df$DATANN=as.Date(df$DATANN,"%Y/%m/%d")
df$debut_contrat=as.Date(df$debut_contrat,"%Y/%m/%d")





df[df$NIVFOR=="NV4",]$NIVFOR="2"
df[df$NIVFOR=="NV2",]$NIVFOR="4"
df[df$NIVFOR=="NV1",]$NIVFOR="5"
df[df$NIVFOR=="NV3",]$NIVFOR="3"
df[df$NIVFOR=="NV5",]$NIVFOR="1"
df[df$NIVFOR%in%c("C12","AFS","CFG","CP4","C3A"),]$NIVFOR="0"
df$NIVFOR=as.numeric(df$NIVFOR)

df$NATION=as.numeric(df$NATION)
df[is.na(df$NATION),]$NATION=0
df[df$NATION !=1,]$NATION=0





df$T_emploi=df$debut_contrat-df$DATINS


df$T_emploi=as.numeric(df$T_emploi)
df$SITMAT=as.factor(df$SITMAT)



df=df[df$MOBUNIT=="KM",]

df$ROME=substring( df$ROME,1,1)

df$SEXE=as.numeric( df$SEXE)-1

df$NENF=as.numeric( df$NENF)

df$ROME=factor( df$ROME)

df$AGE=as.numeric( df$AGE)

df$MOBDIST=as.numeric( df$MOBDIST)
df$MOTINS=factor(df$MOTINS)

df$CONTRAT=factor(df$CONTRAT)

df$tightness=as.numeric(df$tightness)










df_est=subset(df, select= c(Region,SEXE, NENF,T_emploi,NATION, SITMAT,MOTINS,formation,AGE,tightness,MOBDIST,NIVFOR, CONTRAT))
df_est=na.omit(df_est)
df_est$censor=1

X=subset(df_est,select=-c(censor,T_emploi,formation))


X=model.matrix(~ 0 +.,X)
Y=as.vector(df_est$T_emploi)
W=as.vector(df_est$formation)
D=as.vector(df_est$censor)


cs=causal_survival_forest(X,Y,W,D,horizon=1000)

#saveRDS(cs_plus300hours,file="C:/Users/ENSAE05_D_YOUCEFI/Desktop/models/greater300length_hours.Rds")
saveRDS(cs,file="C:/Users/Public/MonjourYoucefi/models/greater300length_hours.Rds")
average_treatment_effect(cs)



########## length of formation < 300 ######## hours


df=read_dta('C:/Users/ENSAE05_D_YOUCEFI/Desktop/short_training.dta')




df$DATE_ENTREE=as.Date(df$DATE_ENTREE,"%Y/%m/%d")
df$DATE_FIN=as.Date(df$DATE_FIN,"%Y/%m/%d")
df$DATINS=as.Date(df$DATINS,"%Y/%m/%d")
df$DATANN=as.Date(df$DATANN,"%Y/%m/%d")
df$debut_contrat=as.Date(df$debut_contrat,"%Y/%m/%d")





df[df$NIVFOR=="NV4",]$NIVFOR="2"
df[df$NIVFOR=="NV2",]$NIVFOR="4"
df[df$NIVFOR=="NV1",]$NIVFOR="5"
df[df$NIVFOR=="NV3",]$NIVFOR="3"
df[df$NIVFOR=="NV5",]$NIVFOR="1"
df[df$NIVFOR%in%c("C12","AFS","CFG","CP4","C3A"),]$NIVFOR="0"
df$NIVFOR=as.numeric(df$NIVFOR)

df$NATION=as.numeric(df$NATION)
df[is.na(df$NATION),]$NATION=0
df[df$NATION !=1,]$NATION=0





df$T_emploi=df$debut_contrat-df$DATINS


df$T_emploi=as.numeric(df$T_emploi)
df$SITMAT=as.factor(df$SITMAT)



df=df[df$MOBUNIT=="KM",]

df$ROME=substring( df$ROME,1,1)

df$SEXE=as.numeric( df$SEXE)-1

df$NENF=as.numeric( df$NENF)

df$ROME=factor( df$ROME)

df$AGE=as.numeric( df$AGE)

df$MOBDIST=as.numeric( df$MOBDIST)
df$MOTINS=factor(df$MOTINS)

df$CONTRAT=factor(df$CONTRAT)

df$tightness=as.numeric(df$tightness)


df_est=subset(df, select= c(Region,SEXE, NENF,T_emploi,NATION, SITMAT,MOTINS,formation,AGE,tightness,MOBDIST,NIVFOR, CONTRAT))
df_est=na.omit(df_est)
df_est$censor=1

X=subset(df_est,select=-c(censor,T_emploi,formation))


X=model.matrix(~ 0 +.,X)
Y=as.vector(df_est$T_emploi)
W=as.vector(df_est$formation)
D=as.vector(df_est$censor)


cs=causal_survival_forest(X,Y,W,D,horizon=1000)

#saveRDS(cs_less300,file="C:/Users/ENSAE05_D_YOUCEFI/Desktop/models/less300length_hours.Rds")
saveRDS(cs,file="C:/Users/Public/MonjourYoucefi/models/less300length_hours.Rds")
average_treatment_effect(cs)








































 ##################### We unlock











########## length of formation > 300 ######## hours


df=read_dta('C:/Users/ENSAE05_D_YOUCEFI/Desktop/long_training.dta')



df$DATE_ENTREE=as.Date(df$DATE_ENTREE,"%Y/%m/%d")
df$DATE_FIN=as.Date(df$DATE_FIN,"%Y/%m/%d")
df$DATINS=as.Date(df$DATINS,"%Y/%m/%d")
df$DATANN=as.Date(df$DATANN,"%Y/%m/%d")
df$debut_contrat=as.Date(df$debut_contrat,"%Y/%m/%d")





df[df$NIVFOR=="NV4",]$NIVFOR="2"
df[df$NIVFOR=="NV2",]$NIVFOR="4"
df[df$NIVFOR=="NV1",]$NIVFOR="5"
df[df$NIVFOR=="NV3",]$NIVFOR="3"
df[df$NIVFOR=="NV5",]$NIVFOR="1"
df[df$NIVFOR%in%c("C12","AFS","CFG","CP4","C3A"),]$NIVFOR="0"
df$NIVFOR=as.numeric(df$NIVFOR)

df$NATION=as.numeric(df$NATION)
df[is.na(df$NATION),]$NATION=0
df[df$NATION !=1,]$NATION=0





df$T_emploi=df$debut_contrat-df$DATINS


df$T_emploi=as.numeric(df$T_emploi)
df$SITMAT=as.factor(df$SITMAT)



df=df[df$MOBUNIT=="KM",]

df$ROME=substring( df$ROME,1,1)

df$SEXE=as.numeric( df$SEXE)-1

df$NENF=as.numeric( df$NENF)

df$ROME=factor( df$ROME)

df$AGE=as.numeric( df$AGE)

df$MOBDIST=as.numeric( df$MOBDIST)
df$MOTINS=factor(df$MOTINS)

df$CONTRAT=factor(df$CONTRAT)

df$tightness=as.numeric(df$tightness)

df_unlock=data.frame(df)

df_unlock$duree_form=df_unlock$DATE_FIN-df_unlock$DATE_ENTREE
df_unlock$duree_form=as.numeric(df_unlock$duree_form)
df_unlock[is.na(df_unlock$duree_form),]$duree_form=0

df_unlock$duree_rech=df_unlock$T_emploi-df_unlock$duree_form



df_est=subset(df_unlock, select= c(Region,SEXE, NENF,SITMAT,NATION, duree_rech,formation,AGE,tightness,MOBDIST,NIVFOR, CONTRAT))
df_est=na.omit(df_est)
df_est$censor=1

df_est=df_est[df_est$duree_rech>0,]

X=subset(df_est,select=-c(censor,duree_rech,formation))

X=model.matrix(~ 0 +.,X)
Y=as.vector(df_est$duree_rech)
W=as.vector(df_est$formation)
D=as.vector(df_est$censor)



cs=causal_survival_forest(X,Y,W,D,horizon=1000)

#saveRDS(cs_plus300hours,file="C:/Users/ENSAE05_D_YOUCEFI/Desktop/models/greater300length_hours.Rds")
saveRDS(cs,file="C:/Users/Public/MonjourYoucefi/models/greater300length_hours_unlock.Rds")
average_treatment_effect(cs)



########## length of formation < 300 ######## hours


df=read_dta('C:/Users/ENSAE05_D_YOUCEFI/Desktop/short_training.dta')



df$DATE_ENTREE=as.Date(df$DATE_ENTREE,"%Y/%m/%d")
df$DATE_FIN=as.Date(df$DATE_FIN,"%Y/%m/%d")
df$DATINS=as.Date(df$DATINS,"%Y/%m/%d")
df$DATANN=as.Date(df$DATANN,"%Y/%m/%d")
df$debut_contrat=as.Date(df$debut_contrat,"%Y/%m/%d")





df[df$NIVFOR=="NV4",]$NIVFOR="2"
df[df$NIVFOR=="NV2",]$NIVFOR="4"
df[df$NIVFOR=="NV1",]$NIVFOR="5"
df[df$NIVFOR=="NV3",]$NIVFOR="3"
df[df$NIVFOR=="NV5",]$NIVFOR="1"
df[df$NIVFOR%in%c("C12","AFS","CFG","CP4","C3A"),]$NIVFOR="0"
df$NIVFOR=as.numeric(df$NIVFOR)

df$NATION=as.numeric(df$NATION)
df[is.na(df$NATION),]$NATION=0
df[df$NATION !=1,]$NATION=0





df$T_emploi=df$debut_contrat-df$DATINS


df$T_emploi=as.numeric(df$T_emploi)
df$SITMAT=as.factor(df$SITMAT)



df=df[df$MOBUNIT=="KM",]

df$ROME=substring( df$ROME,1,1)

df$SEXE=as.numeric( df$SEXE)-1

df$NENF=as.numeric( df$NENF)

df$ROME=factor( df$ROME)

df$AGE=as.numeric( df$AGE)

df$MOBDIST=as.numeric( df$MOBDIST)
df$MOTINS=factor(df$MOTINS)

df$CONTRAT=factor(df$CONTRAT)

df$tightness=as.numeric(df$tightness)

df_unlock=data.frame(df)

df_unlock$duree_form=df_unlock$DATE_FIN-df_unlock$DATE_ENTREE
df_unlock$duree_form=as.numeric(df_unlock$duree_form)
df_unlock[is.na(df_unlock$duree_form),]$duree_form=0

df_unlock$duree_rech=df_unlock$T_emploi-df_unlock$duree_form



df_est=subset(df_unlock, select= c(Region,SEXE, NENF,NATION,SITMAT,duree_rech,formation,AGE,tightness,MOBDIST,NIVFOR, CONTRAT))
df_est=na.omit(df_est)
df_est$censor=1

df_est=df_est[df_est$duree_rech>0,]

X=subset(df_est,select=-c(censor,duree_rech,formation))

X=model.matrix(~ 0 +.,X)
Y=as.vector(df_est$duree_rech)
W=as.vector(df_est$formation)
D=as.vector(df_est$censor)


cs=causal_survival_forest(X,Y,W,D,horizon=1000)

#saveRDS(cs_less300,file="C:/Users/ENSAE05_D_YOUCEFI/Desktop/models/less300length_hours.Rds")
saveRDS(cs,file="C:/Users/Public/MonjourYoucefi/models/less300length_hours_unlock.Rds")
average_treatment_effect(cs)



















