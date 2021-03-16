
/* importer les données*/
PROC IMPORT datafile='/home/u53870302/sasuser.v94/insurance.csv' 
out=MABIBLIO.data REPLACE;
RUN;

/*........ definition des macros...... */


/* macro pour la statistique univariée */

%MACRO univariate(table=,variables=);
PROC UNIVARIATE DATA = &table.;
VAR &variables.;
RUN;
%MEND univariate;

/* macro pour la correlation */
%MACRO correlation(table=,variables=);
PROC CORR DATA=&table.;
	var &variables.;
RUN;
%MEND correlation;

/* macro pour la regression */
%MACRO regressionL(table=,variables=);
proc reg data=&table alpha=0.05 plots(only)=(diagnostics residuals 
		observedbypredicted);
	    model charges=&variables./;
RUN;
%MEND regressionL;


/* .............statistiques descriptives............*/
Title 'Informations sur le jeux de données';
PROC CONTENTS DATA=mabiblio.data;
RUN;

Title 'Procedures Moyenne';
PROC MEANS data=MABIBLIO.data;
RUN;

Title 'Procedure Résumé';
PROC SUMMARY data=MABIBLIO.data print;
RUN;

/*macroprogramme*/

%univariate(table=MABIBLIO.data,variables=charges);
%correlation(table=MABIBLIO.data,variables=age bmi children charges);



proc sgscatter data=MABIBLIO.data;
matrix age bmi children charges / group=sex diagonal=(histogram kernel);
TITLE "Matrice de scatterplots";
 run;
 
/*Répresentation pour toutes les entités catégorielles individuellement*/


proc sgplot data=MABIBLIO.DATA;
	vbox charges / category=sex;
	yaxis grid;
	TITLE 'Boite a moustache des charges en fonction du sexe';
run;

/*........*/

PROC SGPANEL  DATA=MABIBLIO.DATA;
PANELBY sex;
  VBOX charges / category = smoker;
  TITLE 'Boite a moustache des charges en fonction du statut fumeur';
RUN;

/*........*/
proc sgplot data=MABIBLIO.DATA;
	bubble x=region y=charges size=age/ group=sex bradiusmin=7 bradiusmax=14;
	xaxis grid;
	yaxis grid;
	TITLE 'Graphe des charges en fonction des regions';
run;

/*........*/

proc sgplot data=MABIBLIO.DATA;
	bubble x=smoker y=charges size=age/ group=sex bradiusmin=7 bradiusmax=14;
	xaxis grid;
	yaxis grid;
	TITLE 'Graphe des charges en fonction du statut fumeur';
run;

/*........*/

PROC SGPLOT DATA = MABIBLIO.DATA;
 VBAR smoker / GROUP = sex GROUPDISPLAY = CLUSTER;
TITLE 'Proportion des fumeurs en fonction du sexe';
RUN;

/*.......Pré-Traitement............................................*/


/* conversion des variables sex,smoker et region en variables quantitatives*/
data MABIBLIO.DATA;
set MABIBLIO.DATA;
/* use IF-THEN logic to recode gender */
length sex_Recode  8;
format sex_Recode BEST12.;
informat sex_Recode BEST32.;
if      sex="female" then sex_Recode = 0;
else if sex="male" then sex_Recode = 1;
else sex_Recode = ".";
run;

/*........*/


data MABIBLIO.DATA;
set MABIBLIO.DATA;
/* use IF-THEN logic to recode smoker */
length smoker_Recode  8;
format smoker_Recode BEST12.;
informat smoker_Recode BEST32.;
if      smoker="no" then smoker_Recode = 0;
else if smoker="yes" then smoker_Recode = 1;
else smoker_Recode = ".";
run;
/*........*/

data MABIBLIO.DATA;
set MABIBLIO.DATA;
/* use IF-THEN logic to recode region */
length region_Recode  8;
format region_Recode BEST12.;
informat region_Recode BEST32.;
if      region="southwest" then region_Recode = 0;
else if region="southeast" then region_Recode = 1;
else if region="northwest" then region_Recode = 2;
else if region="northeast" then region_Recode = 3;
else region_Recode = ".";
run;

/*Type*/

proc contents data=MABIBLIO.DATA
             out=mabiblio.contenu;
RUN ;

/*Correlogram*/

proc corr data=MABIBLIO.data plots=matrix(histogram);
run;

/*On fixe un generateur aleatoire */


/*diviser les données en plusieurs sous-ensembles aléatoires avec une proportion donnée*/

proc surveyselect data=MABIBLIO.DATA out=BIBLIO method=srs seed=1953 samprate=.7 outall noprint;
run; 

/*cette étape crée une nouvelle variable nommée "Sélectionné" qui peut être référencée plus tard*/
data MABIBLIO.train;
set BIBLIO;
if selected = 1;
drop selected;
run;

data MABIBLIO.valid;
set BIBLIO;
if selected = 0;
drop selected;
run;


/*Regression lineaire multiple suivit d'une selection de variables*/

/*Macro-programme pour la regression lineaire multiple avec toutes les variables*/

%regressionL(table=MABIBLIO.train,variables=age bmi children smoker_Recode sex_Recode region_Recode);


/*Selection de variables*/
proc glmselect data=MABIBLIO.train outdesign(addinputvars)=Work.reg_design 
		plots=(criterionpanel);
	model charges=age bmi children smoker_Recode sex_Recode region_Recode / 
		showpvalues selection=stepwise
    
   (select=aic);
run;

/*Macro-programme pour la regression lineaire multiple avec variables selectionnées*/

%regressionL(table=MABIBLIO.train,variables=age bmi children smoker_Recode region_Recode);
/*Model de regression predictif avec valid-set */

proc glmselect data=MABIBLIO.valid plots=(criterionpanel);
	class region smoker / param=glm;
	model charges=region smoker age bmi children / 
		selection=stepwise
(select=aic) hierarchy=single;
	score out=MABIBLIO.scorev predicted residual;
run;

/* Recuperation de la table de prediction */

data MABIBLIO.prediction;
set mabiblio.scorev (keep=region smoker age bmi p_charges );
run;

proc sql;
select sqrt(mean((charges-p_charges)**2)) as RMSE from mabiblio.score;
quit;

/* Affichage de la table prediction */
proc print data=mabiblio.prediction;
run;
/* acp avec princomp pour afficher le graphe des individus */
proc princomp data=MABIBLIO.DATA plots(only ncomp=2)=(scree score) 
		out=MABIBLIO.PRINCOMP_SCORES outstat=MABIBLIO.PRINCOMP_STATS;
	var age bmi children charges sex_Recode smoker_Recode region_Recode;
	id region charges;
run;

