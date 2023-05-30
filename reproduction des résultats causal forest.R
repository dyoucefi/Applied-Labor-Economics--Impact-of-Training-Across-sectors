library(haven)
library(ggplot2)
library(dplyr)
library(grf)
library(gridExtra)
library(arrow)
library(dplyr)
library(readr)
library(haven)
library(lubridate)
library(tictoc)
library(tidyr)

df= open_dataset("C:/Users/dalil.youcefi/Documents/Formation et retour à l'emploi/Données/test.parquet")
df=df%>%collect()

df=df[df$emploi_creation_entreprise_t_plus_1 !="1"&
        df$emploi_creation_entreprise_t_plus_3 !="1"&
        df$emploi_creation_entreprise_t_plus_6 !="1"&
        df$emploi_creation_entreprise_t_plus_9 !="1"&
        df$emploi_creation_entreprise_t_plus_12 !="1"&
        df$emploi_creation_entreprise_t_plus_18 !="1"&
        df$emploi_creation_entreprise_t_plus_24 !="1"&
        df$emploi_creation_entreprise_t_plus_30 !="1"&
        df$emploi_creation_entreprise_t_plus_36 !="1",]

df=df[df$emploi_durable_t_plus_1 =="1"|
     df$emploi_durable_t_plus_3 =="1"|
     df$emploi_durable_t_plus_6 =="1"|
     df$emploi_durable_t_plus_9 =="1"|
     df$emploi_durable_t_plus_12 =="1"|
     df$emploi_durable_t_plus_18 =="1"|
     df$emploi_durable_t_plus_24 =="1"|
     df$emploi_durable_t_plus_30 =="1"|
     df$emploi_durable_t_plus_36 =="1",]




###### put in a date format

df$date_entree_eff=as.Date(df$date_entree_eff,"%Y/%m/%d")
df$date_fin_eff=as.Date(df$date_fin_eff,"%Y/%m/%d")



controls <- c('anciennete',
              'nb_demandes',
              'age',
              
              'SEXE',
              'NENF',
              'NATION',
              'NIVFOR',
              'MOTINS',
              
              'SITMAT',
              'CONTRAT',
              
              'QUALIF',
              'ROME',
              'EXPER',
              'handicap',
              'zone_urbaine',
              'zone_rurale',
              'longue_distance',
              'temps_maladie_spell',
              'temps_maladie_avant',
              'absence',
              'temps_chomage_avant',
              'nb_heures','T_emploi',"censor","entree_formation")


####create the length of unemplyment spell variable


df[df$NIVFOR=="BEPC/CAP/pr?-bac",]$NIVFOR="0"
df[df$NIVFOR=="Bac",]$NIVFOR="1"
df[df$NIVFOR=="Bac+2",]$NIVFOR="2"
df[df$NIVFOR=="Licence",]$NIVFOR="3"
df[df$NIVFOR=="Bac +5",]$NIVFOR="4"

df$NIVFOR=as.numeric(df$NIVFOR)

df$NATION=as.factor(df$NATION)
df$T_emploi=df$date_fin_eff-df$date_entree_eff
df$censor=df$emploi_durable_t_plus_12

df$T_emploi=as.numeric(df$T_emploi)

### create factor for categorical variables
df$SITMAT=as.factor(df$SITMAT)




df$ROME=substring( df$ROME,1,1)
### create a dummy for gender

df$SEXE=as.numeric( df$SEXE)

df$NENF=as.numeric( df$NENF)

df$zone_urbaine=as.numeric(df$zone_urbaine)
df$zone_rurale=as.numeric(df$zone_rurale)

df$handicap=as.numeric(df$handicap)

df$age2=as.factor(df$age2)

df$age=as.numeric( df$age)

df$MOTINS=factor(df$MOTINS)

df$CONTRAT=factor(df$CONTRAT)


df$QUALIF=as.factor(df$QUALIF)
df$nb_demandes=as.numeric(df$nb_demandes)
df$anciennete=as.numeric(df$anciennete)
df$longue_distance=as.numeric(df$longue_distance)
df$temps_maladie_avant=as.numeric(df$temps_maladie_avant)
df$absence=as.numeric(df$absence)
df$temps_chomage_avant=as.numeric(df$temps_chomage_avant)
df$temps_maladie_spell=as.numeric(df$temps_maladie_spell)
########## regular estimation


####subsetting for the covariates chosen
df_est=subset(df, select= controls)
df_est=na.omit(df_est)
df_est$censor=1
#no censoring and 

treatment=df_est[df_est$entree_formation==1,]
control=df_est[df_est$entree_formation==0,]
control=control[sample(nrow(control),nrow(treatment)),]

df_est=rbind(treatment,control)

X=subset(df_est,select=-c(censor,T_emploi,entree_formation))

#### encode for the grf model
X=model.matrix(~ 0 +.,X)
Y=as.vector(df_est$T_emploi)
W=as.vector(df_est$entree_formation)
D=as.vector(df_est$censor)

### set a long horizon 
cs=causal_survival_forest(X,Y,W,D)


### save the model






saveRDS(cs,file="C:/Users/dalil.youcefi/Documents/Formation et retour à l'emploi/test_causal_forest.Rds")



best_linear_projection(cs,X)















