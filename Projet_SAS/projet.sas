libname mabiblio '/home/u53870402/sasuser.v94';


/* importer les données*/
PROC IMPORT datafile='/home/u53870402/sasuser.v94/insurance.csv' 
out=MABIBLIO.data REPLACE;
RUN;


/* .............statistiques descriptives............*/

Title 'Procedures Moyenne';
PROC MEANS data=MABIBLIO.data;
RUN;

Title 'Procedure Résumé';
PROC SUMMARY data=MABIBLIO.data print;
RUN;

/*macrovariable*/

%let table = MABIBLIO.data;
%let variables = charges;
PROC UNIVARIATE DATA = &table.;
VAR &variables.;
RUN;

/*Les corrélations*/
PROC CORR DATA=MABIBLIO.data;
	var age bmi children charges;
run;

/*Correlogram*/

proc corr data=MABIBLIO.data plots=matrix(histogram);
run;


proc sgscatter data=MABIBLIO.data;
matrix age bmi children charges / group=sex diagonal=(histogram kernel);
 run;
 
/*Répresentation pour toutes les entités catégorielles individuellement*/


proc sgplot data=MABIBLIO.DATA;
	vbox charges / category=sex;
	yaxis grid;
run;

/*........*/

PROC SGPANEL  DATA=MABIBLIO.DATA;
PANELBY sex;
  VBOX charges / category = smoker;
RUN;

/*........*/
proc sgplot data=MABIBLIO.DATA;
	bubble x=region y=charges size=age/ group=sex bradiusmin=7 bradiusmax=14;
	xaxis grid;
	yaxis grid;
run;

/*........*/

proc sgplot data=MABIBLIO.DATA;
	bubble x=smoker y=charges size=age/ group=sex bradiusmin=7 bradiusmax=14;
	xaxis grid;
	yaxis grid;
run;

/*........*/

PROC SGPLOT DATA = MABIBLIO.DATA;
 VBAR smoker / GROUP = sex GROUPDISPLAY = CLUSTER;
TITLE 'Olympic Countries by Region and Population Group';
RUN;

/*...................................................*/


/* conversion des variables sex,smoker et region en variables quantitatives*/

data MABIBLIO.DATA;
set MABIBLIO.DATA;
/* use IF-THEN logic to recode gender */
length sex_Recode $6;
if      sex="female" then sex_Recode = 0;
else if sex="male" then sex_Recode = 1;
else sex_Recode = " ";
run;

/*........*/

data MABIBLIO.DATA;
set MABIBLIO.DATA;
/* use IF-THEN logic to recode gender */
length smoker_Recode $6;
if      smoker="no" then smoker_Recode = 0;
else if smoker="yes" then smoker_Recode = 1;
else smoker_Recode = " ";
run;

/*........*/

data MABIBLIO.DATA;
set MABIBLIO.DATA;
/* use IF-THEN logic to recode gender */
length region_Recode $6;
if      region="southwest" then region_Recode = 0;
else if region="southeast" then region_Recode = 1;
else if region="northwest" then region_Recode = 2;
else if region="northeast" then region_Recode = 3;
else region_Recode = " ";
run;


/* Selection de variables suivit de regression lineaire multiple*/

/*Selection de variables*/
proc reg data=MABIBLIO.DATA;
model charges = age bmi children / selection=rsquare cp rsquarebic best=1;
run;


/*regression lineaire multiple*/
proc reg data=MABIBLIO.DATA all;
model charges = age bmi children/dw covb Influence cli clm tol vif collin R P;
output out=resout h=lev p=pred r=res student=resstu;
run;



/*diviser les données en plusieurs sous-ensembles aléatoires avec une proportion donnée*/

proc surveyselect data=MABIBLIO.DATA out=BIBLIO method=srs samprate=.7 outall noprint;
run; 
/*cette étape crée une nouvelle variable nommée "Sélectionné" qui peut être référencée plus tard*/
data train;
set BIBLIO;
if selected = 1;
drop selected;
run;

data valid;
set BIBLIO;
if selected = 0;
drop selected;
run;

proc print data=train.set;
run;

proc print data=test.set;
run;