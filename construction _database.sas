
LIBNAME lib_bre "\\casd.fr\casdfs\Projets\ENSAE05\Data\FORCE_FORCE_2022T4\BREST";
LIBNAME lib_fh "\\casd.fr\casdfs\Projets\ENSAE05\Data\FORCE_FORCE_2022T4\FH";
LIBNAME lib_mmo "\\casd.fr\casdfs\Projets\ENSAE05\Data\FORCE_FORCE_2022T4\MMO";
LIBNAME lib_acem "\\casd.fr\casdfs\Projets\ENSAE05\Data\ACEMO_ACEMO-Trimestrielle_2021" ;
LIBNAME lib_ree "\\casd.fr\casdfs\Projets\ENSAE05\Data\REE_Stocks Etablissements_2021";


/* open the database and concatenate them*/
DATA brest;
	SET lib_bre.brest2_22t2_force_vf;
	if cmiss(of _all_) then delete;
RUN;
PROC CONTENTS data=brest;


DATA brest2;
	SET lib_bre.brest2_1721_majv10;
	if cmiss(of _all_) then delete;
RUN;

PROC APPEND
	base=brest
	data=brest2;
RUN;

/* we keep the variables we want and format date */
DATA brest;
	set brest (keep = id_force VB_IDENT DISPOSITIF DATE_ENTREE DATE_FIN COMMANDITAIRE OBJECTIF_STAGE DOMAINE_FORMATION DUREE_FORMATION_HEURES_REDRESSEE NIV_DIPLOME AGE_ENTREE_STAGE COMMUNE DEPARTEMENT_HABITATION REGION_HABITATION);
	WHERE year(DATE_ENTREE)>2020;

	if cmiss(of _all_) then delete;
	
	format DATE_ENTREE DATE_FIN yymmdd10.;
RUN;

DATA de_fh;
	SET lib_fh.de_tempo (keep= id_force Region NDEM SEXE NENF NATION NIVFOR ROME DIPLOME SITMAT DATINS MOTANN DATANN AGE MOTINS CONTRAT MOBDIST MOBUNIT);
	if cmiss(of _all_) then delete;

RUN;


DATA de_fh;
	SET de_fh;
	WHERE year(DATANN) >2020;
RUN;




/* We keep people that found a job*/	

DATA de_fh;
	SET de_fh;
	
	WHERE MOTANN = "11"|MOTANN = "12"|MOTANN = "13"|MOTANN = "14"|MOTANN = "15"|MOTANN = "21"|MOTANN = "22"|MOTANN = "23"|MOTANN = "24"|MOTANN = "25";
RUN;
	


DATA mmo;
	SET lib_mmo.MMO_2_T22022_F10 (keep= id_force DebutCTT Nature salaire_base_mois_complet CP siret_AF);
	if cmiss(of _all_) then delete;
	debut_contrat=input(DebutCTT, yymmdd10.);

	format debut_contrat yymmdd10.;
RUN;


DATA mmo_2021;
	SET lib_mmo.MMO_2_2021_F10 (keep= id_force DebutCTT Nature salaire_base_mois_complet CP siret_AF);

	if cmiss(of _all_) then delete;
	debut_contrat=input(DebutCTT, yymmdd10.);
	
	format debut_contrat yymmdd10.;

RUN;

PROC SQL;
SELECT min(DATANN) from fh_non_form1;
RUN;


DATA mmo_2021;

	SET mmo_2021;
	where year (debut_contrat)>2020;
RUN;

DATA mmo;

	SET mmo;
	where year (debut_contrat)>2020;
RUN;


PROC APPEND
	base=mmo
	data=mmo_2021;
RUN;

/* join FH and BREST on FORCE key*/

PROC SQL;
CREATE TABLE brest_fh as 
SELECT *
FROM brest as x
LEFT JOIN  de_fh as y 
on x.id_force=y.id_force;
QUIT;

DATA brest_fh;
	SET brest_fh;
	WHERE DATE_ENTREE>DATINS & DATE_FIN<DATANN;
RUN;


/*We keep person which is their first training*/
PROC SQL;
create table brest_fh_merged as
SELECT *
from brest_fh
Group by id_force
having count(DATE_ENTREE) =1
;
QUIT;


/*Set treatment dummy*/


DATA brest_fh_merged;
	SET brest_fh_merged;
	formation = 1;
RUN;



/*Join with MMO*/
PROC SQL;
CREATE TABLE brest4 as 
SELECT *
FROM  brest_fh_merged as x
LEFT JOIN  mmo as y 
on x.id_force=y.id_force;
QUIT;

/*set contract that begins after inscription to pole emploi and after training*/
DATA brest4;
	SET brest4;
	if cmiss( of _all_) then delete;
	where debut_contrat>=DATINS & debut_contrat>DATE_ENTREE;
RUN;

/* create the difference of days between leaving pole emploi and beginning of working contrat*/

DATA control;
	SET brest4;
	diff= intck('day', DATE_ENTREE, debut_contrat);
RUN;

PROC SQL;
	select mean(diff) from control;
RUN;



DATA brest4;
	SET brest4;
	diff_jours = intck('day', DATANN, debut_contrat);
	diff_jours2= (diff_jours)**2 ;
RUN;

/*we select the contract that it is the closest from leaving pole emploi*/

PROC SQL;
create table formation as
SELECT *
from brest4
group by id_force
having diff_jours2=min(diff_jours2);
QUIT;

/*remove duplicates*/
PROC SORT data=formation out=formation nodupkey;
	by id_force;
RUN;

/*create control group-routine is the same*/

DATA control;
	SET formation;
	diff= intck('day', DATINS, debut_contrat);
RUN;

PROC SQL;
	select min(diff) from control;
RUN;
	



PROC SQL;
SELECT count(id_force) as N from formation;



PROC SQL;
create table fh_non_form as
SELECT *
from de_fh
WHERE id_force NOT IN (SELECT id_force from brest);
QUIT;





DATA fh_non_form;
	SET fh_non_form;
	formation = 0;
RUN;


PROC SQL;
CREATE TABLE fh_non_form1 as 
SELECT *
FROM  fh_non_form as x
LEFT JOIN  mmo as y 
on x.id_force=y.id_force;
QUIT;

DATA fh_non_form1;
	SET fh_non_form1;
	if cmiss( of _all_) then delete;
	where debut_contrat>DATINS;
RUN;






DATA fh_non_form1;
	SET fh_non_form1;
	diff_jours = intck('day', DATANN, debut_contrat);
	diff_jours2= (diff_jours)**2 ;
RUN;



PROC SQL;
create table fh_non_form2 as
SELECT *
from fh_non_form1
group by id_force
having diff_jours2=min(diff_jours2);
QUIT;








PROC SORT data=fh_non_form2 out=fh_non_form2 nodupkey;
	by id_force;
RUN;

PROC SURVEYSELECT data= fh_non_form2
	out= fh_non_form2
	sampsize=16150
	seed=111;
RUN;


DATA formation;
	SET formation;
	formation=1;
RUN;


DATA treatment;


/*append the two database we need to create empty variables for control*/

set fh_non_form2;
VB_IDENT=' ';
DISPOSITIF=' ';
DATE_ENTREE=.;
DATE_FIN=.;
COMMANDITAIRE=' ';
OBJECTIF_STAGE=' ' ;
DOMAINE_FORMATION=' ' ;
DUREE_FORMATION_HEURES_REDRESSEE=.;
NIV_DIPLOME=' ';
AGE_ENTREE_STAGE=.;
COMMUNE =' ';
DEPARTEMENT_HABITATION=' ';
REGION_HABITATION=' ';
RUN;

PROC SQL;

create table database_final as

SELECT id_force ,VB_IDENT, DISPOSITIF, DATE_ENTREE, DATE_FIN ,COMMANDITAIRE, OBJECTIF_STAGE, DOMAINE_FORMATION ,DUREE_FORMATION_HEURES_REDRESSEE, NIV_DIPLOME, AGE_ENTREE_STAGE, COMMUNE, DEPARTEMENT_HABITATION, REGION_HABITATION
		,Region, NDEM, SEXE, NENF, NATION, NIVFOR, ROME, DIPLOME, SITMAT, DATINS, MOTANN, DATANN, AGE, MOTINS, CONTRAT, MOBDIST, MOBUNIT
		, debut_contrat, Nature, salaire_base_mois_complet ,CP, siret_AF, formation from formation
UNION



SELECT id_force ,VB_IDENT, DISPOSITIF, DATE_ENTREE, DATE_FIN ,COMMANDITAIRE, OBJECTIF_STAGE, DOMAINE_FORMATION ,DUREE_FORMATION_HEURES_REDRESSEE, NIV_DIPLOME, AGE_ENTREE_STAGE, COMMUNE, DEPARTEMENT_HABITATION, REGION_HABITATION
		,Region, NDEM, SEXE, NENF, NATION, NIVFOR, ROME, DIPLOME, SITMAT, DATINS, MOTANN, DATANN, AGE, MOTINS, CONTRAT, MOBDIST, MOBUNIT
		, debut_contrat, Nature, salaire_base_mois_complet ,CP, siret_AF, formation from treatment;

QUIT;





/*We join REE through SIRET number of companies*/

LIBNAME project "C:\Users\ENSAE05_D_YOUCEFI\Desktop";

DATA project.database_final;
	SET database_final;
RUN;



/* REE DONNEES STOCK ETABLISSEMENT */


DATA ree;
	SET lib_ree.stetab_31122020 (keep= SIRET A10 A21 A10_ET A21_ET ACTIV_NAT_ET APEN APET) ;
RUN;


data database_final ;
set project.database_final (rename=(siret_AF=SIRET)) ;
run ;


PROC SQL;
CREATE TABLE database_final_ree as 
SELECT *
FROM  database_final as x
LEFT JOIN  ree as y 
on x.SIRET=y.SIRET;
QUIT;


DATA project.database_final_ree;
	SET database_final_ree;
RUN;




/* exportation to format dta */

LIBNAME project "C:\Users\ENSAE05_M_MONJOUR\Desktop\Project2" ;


proc contents data= "C:\Users\ENSAE05_M_MONJOUR\Desktop\Project2\database_final_ree";
run ;

DATA temp;
set "C:\Users\ENSAE05_M_MONJOUR\Desktop\Project2\database_final_ree" ;
run ; 

proc export
data=temp
outfile="C:\Users\ENSAE05_M_MONJOUR\Desktop\Project2\database_final_ree.dta"
dbms=dta
replace;
RUN;


DATA acemo_2021;
SET lib_acemo_2021.pro20211_resultats (keep =  s_et empvac nbvac sectu r_annenq r_trienq pond)

	if cmiss(of _all_) then delete;
RUN;

LIBNAME lib_fh "\\casd.fr\casdfs\Projets\ENSAE05\Data\FORCE_FORCE_2022T4\FH";

proc contents data= "\\casd.fr\casdfs\Projets\ENSAE05\Data\FORCE_FORCE_2022T4\FH\de_tempo";
run ;


&let keepvars = id_force Region  ;


DATA temp;
set "\\casd.fr\casdfs\Projets\ENSAE05\Data\FORCE_FORCE_2022T4\FH\de_tempo" ;

keep &keepvars ; 
run ;


proc export
data=temp
outfile="C:\Users\ENSAE05_M_MONJOUR\Desktop\Project\de_tempo.dta"
dbms=dta
replace;
RUN;	



LIBNAME lib_mmo "\\casd.fr\casdfs\Projets\ENSAE05\Data\FORCE_FORCE_2022T4\MMO";

proc contents data= "\\casd.fr\casdfs\Projets\ENSAE05\Data\FORCE_FORCE_2022T4\MMO\MMO_2_T22022_F10";
run ;

DATA tempp;
set "\\casd.fr\casdfs\Projets\ENSAE05\Data\FORCE_FORCE_2022T4\MMO\MMO_2_T22022_F10" ;
run ; 
proc export
data=tempp
outfile="C:\Users\ENSAE05_M_MONJOUR\Desktop\Project\MMO.dta"
dbms=dta
replace;
RUN;	


proc contents data= "C:\Users\ENSAE05_M_MONJOUR\Desktop\Project2\MMO";
run ;

DATA temp2;
set "C:\Users\ENSAE05_M_MONJOUR\Desktop\Project2\MMO" ;
run ; 

proc export
data=temp2
outfile="C:\Users\ENSAE05_M_MONJOUR\Desktop\Project2\MMO.dta"
dbms=dta
replace;
RUN;
