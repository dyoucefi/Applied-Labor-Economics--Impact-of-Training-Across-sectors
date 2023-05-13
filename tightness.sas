/*we load fh and keep the unemployed during 2020 and 2021*/

LIBNAME lib_fh "\\casd.fr\casdfs\Projets\ENSAE05\Data\FORCE_FORCE_2022T4\FH";
DATA de_fh;
	SET lib_fh.de_tempo (keep= id_force Region ROME DATINS DATANN );
	if cmiss(ROME) then delete ;

RUN;

DATA de_fh;
	SET de_fh;
	WHERE year(DATANN) >2020 | DATANN=.;
RUN;
/*we match Rome code to NAF code with this table of conversion*/
PROC IMPORT datafile="C:\Users\ENSAE05_D_YOUCEFI\Desktop\naf_rome.csv"
out=naf_rome
dbms=csv
replace;
delimiter=";"
getnames=yes;
run;

PROC SQL;
CREATE TABLE fh_naf as 
SELECT *
FROM de_fh as x
LEFT JOIN  naf_rome as y 
on x.ROME=y.Code_ROME;
QUIT;

/* we then have the number of unemployed searching for each nag sector*/
PROC SQL;
CREATE TABLE unempl_naf as 
SELECT Code_NAF, COUNT(*)as unempl
FROM fh_naf
GROUP BY Code_NAF;
QUIT;



/* we do the same routine with acme but for vacancies*/
LIBNAME lib_acem "\\casd.fr\casdfs\Projets\ENSAE05\Data\ACEMO_ACEMO-Trimestrielle_2021" ;

DATA acemo;
	SET lib_acem.pro20211_resultats (keep= s_et nbvac empvac sectu);
	SET lib_acem.pro20212_resultats (keep= s_et nbvac empvac sectu);
	SET lib_acem.pro20213_resultats (keep= s_et nbvac empvac sectu);
	SET lib_acem.pro20214_resultats (keep= s_et nbvac empvac sectu);
	WHERE empvac="O";
RUN;

PROC SQL;

create table mean_acemo as

SELECT s_et ,sectu, mean(nbvac) as mean_vac from acemo
group by  s_et;
QUIT;

PROC SQL;
create table vacancies as
SELECT sectu, sum(mean_vac) as vac from mean_acemo
group by sectu;
QUIT;


PROC SQL; 

create table tightness as
SELECT * FROM unempl_naf as x
LEFT JOIN vacancies as y
on x.Code_NAF=y.sectu;
QUIT;


DATA tightness;
set tightness;
if cmiss( of _all_) then delete;
tightness=vac/unempl;
RUN;

/*we load the unemployed database and we merge the tightness indicator for desired sector*/
LIBNAME lib_dat "C:\Users\ENSAE05_D_YOUCEFI\Desktop";

DATA database_final_ree;
SET lib_dat.database_final_ree;
RUN;
DATA naf_rome;
SET naf_rome (keep = Code_ROME Code_NAF);
RUN;
/*get NAF code as in FH*/
PROC SQL;
CREATE TABLE final_naf as 
SELECT *
FROM database_final_ree as x
LEFT JOIN  naf_rome as y 
on x.ROME=y.Code_ROME;
QUIT;


DATA final_naf;
SET final_naf (rename=(Code_NAF=NAF_des));
RUN;


proc sql; 
select distinct NAF_des from final_naf;
QUIT;


PROC SQL;
CREATE TABLE final_tight as 
SELECT *
FROM final_naf as x
LEFT JOIN  tightness as y 
on x.NAF_des=y.Code_NAF;
QUIT;


DATA lib_dat.database_final_tight_des;
	SET final_tight;
RUN;
