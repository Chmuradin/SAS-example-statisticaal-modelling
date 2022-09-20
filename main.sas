 /* The Boston house-price data of Harrison, D. and Rubinfeld, D.L. 'Hedonic
 prices and the demand for clean air', J. Environ. Economics & Management,
 vol.5, 81-102, 1978.   Used in Belsley, Kuh & Welsch, 'Regression diagnostics
 ...', Wiley, 1980.   N.B. Various transformations are used in the table on
 pages 244-261 of the latter.

 Variables in order:
 CRIM     per capita crime rate by town
 ZN       proportion of residential land zoned for lots over 25,000 sq.ft.
 INDUS    proportion of non-retail business acres per town
 CHAS     Charles River dummy variable (= 1 if tract bounds river; 0 otherwise)
 NOX      nitric oxides concentration (parts per 10 million)
 RM       average number of rooms per dwelling
 AGE      proportion of owner-occupied units built prior to 1940
 DIS      weighted distances to five Boston employment centres
 RAD      index of accessibility to radial highways
 TAX      full-value property-tax rate per $10,000
 PTRATIO  pupil-teacher ratio by town
 B        1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town
 LSTAT    % lower status of the population
 MEDV     Median value of owner-occupied homes in $1000's */
proc import datafile="C:\Users\micha\OneDrive\Pulpit\boston.csv"
     out=dane
     dbms=csv
     replace;
     getnames=yes;
run;
proc print data=dane (obs=15);
run;
proc sql;
alter table dane
drop VAR1, rad;
run;
quit; 
proc print data=dane (obs=15);
run;
proc contents data=dane;
run;
proc means data=dane;
run;
proc univariate data = dane;
var crim zn indus nox rm age dis tax ptratio black lstat medv;
histogram crim zn indus nox rm age dis tax ptratio black lstat medv;
run;
proc freq data = dane;
	table nox;
/* zmienne black i medv mog¹ byæ cenzurowane - w dokumentacji nie ma wzmianki, ale tak wygl¹daj¹ */
proc sort data=dane;
by medv;
run;
proc sgplot data=dane;
scatter y=nox x=medv;
run;
proc sgplot data=dane;
scatter y=black x=medv;
run;
/* objasniamy nox, bez efektów losowych*/
proc glm data=dane;
class chas;
model nox = crim  zn indus rm age dis tax ptratio black lstat medv;
run;
/* selekcja zmiennych */ 
proc reg data=dane plots=(criteria(label) cp);    
    model nox = crim zn indus chas tax black lstat rm age dis ptratio medv/ 
       selection=rsquare cp;   
	plot cp.*np. / cmallows=black; 
	title 'All models';
run;
/*najlepszy z 5 zmiennymi: indus tax age dis ptratio */
/*najlepszy z 6 zmiennymi: indus tax age dis ptratio medv */
/* selekcja metod¹ krokow¹ */
proc reg data=dane;
model nox = crim zn indus chas tax black lstat rm age dis ptratio medv /
selection=stepwise sle=.05 sls=.05 details=summary;
run;
/*najlepszy z 5 zmiennymi: indus tax age dis ptratio */
/*najlepszy z 6 zmiennymi: indus tax age dis ptratio medv */


proc reg data=dane plots=(criteria(label) cp);                        
    model medv = crim zn indus chas nox tax black lstat rm age dis ptratio/ 
       selection=rsquare cp;   
	plot cp.*np. / cmallows=black; 
	title 'All models';
run;
/*najlepszy z 5 zmiennymi: nox lstat rm dis ptratio */
/*najlepszy z 5 zmiennymi: chas nox lstat rm dis ptratio */
/* selekcja metod¹ krokow¹ */
proc reg data=dane;
model medv = crim zn indus chas tax black lstat rm age dis ptratio nox /
selection=stepwise sle=.05 sls=.05 details=summary;
run;
/*najlepszy z 5 zmiennymi: nox lstat rm dis ptratio */
/*najlepszy z 5 zmiennymi: chas nox lstat rm dis ptratio */


/* lasso dla nox */ 
proc glmselect data=dane;
 model nox = crim zn indus chas tax black lstat rm age dis ptratio medv
      /selection=lasso(stop=none choose=AIC);
run;
/* wybrany model: Intercept crim indus chas tax black age dis ptratio medv */

/* teraz lasso dla medv */ 
proc glmselect data=dane;
 model medv = crim zn indus chas nox tax black lstat rm age dis ptratio
      /selection=lasso(stop=none choose=AIC);
run;
/* wybrany model: Intercept crim zn chas nox tax black lstat rm dis ptratio */

/* ridge dla nox */
proc reg data=dane outvif   /* The OUTVIF option outputs the variance inflation factors to the OUTEST= data set */
         outest=b ridge=0 to 0.2 by .002;
   model nox = crim zn indus chas tax black lstat rm age dis ptratio medv;
   plot / ridgeplot nomodel nostat;
run;   /* ie jest zbyt efektywne */
/* ridge dla medv */
proc reg data=dane outvif   /* The OUTVIF option outputs the variance inflation factors to the OUTEST= data set */
         outest=b ridge=0 to 0.2 by .002;
   model medv = Intercept crim rad zn chas nox tax black lstat rm dis ptratio;
   plot / ridgeplot nomodel nostat;
run;   /* ie jest zbyt efektywne */
/* model losowy dla nox na zmienn¹ chas*/
proc mixed data=dane covtest noclprint=3;
class chas;
model nox = tax indus age dis ptratio/s;
random int/subject=chas;
run; /* czynnik losowy bezu¿yteczny */
/* model losowy dla medv*/
proc mixed data=dane covtest noclprint=3;
class chas;
model medv = nox lstat rm dis ptratio/s;
random int/subject=chas;
run; /* troche lepiej */
/* Testowanie krzy¿owe*/ 
data tmp;
set dane;
n = ranuni(60);
run;

proc sort data=tmp;
by n;
run;
/* dla nox */
data model_data;
set tmp nobs = nobs;
if _n_ > 0.8*nobs then score_train = .;
else score_train = nox;
run;

proc print data=model_data (obs=20);
run;
/* CV dla regresji liniowej */
proc glm data=model_data;
model score_train = indus tax age dis ptratio medv;
output out=out1 p=pred;
run;
data MSE1;
set out1 nobs = nobs;
if _n_ <= nobs * 0.8 then SE_train1 = (nox - pred)**2;
else SE_train1 = .;
if _n_ > nobs * 0.8 then SE_test1 = (nox - pred)**2;
else SE_test1 = .;
run;

proc sql;
select avg(SE_train1) as train_MSE1, avg(SE_test1) as test_MSE1 from MSE1;
run;
quit; /* 0.003059 0.003069 R^2 = 0.774165  */

/* CV dla modelu losowego*/
proc mixed data=model_data covtest noclprint=3;
class chas;
model score_train = indus tax age dis ptratio medv/s outpm = out2;
random int/subject=chas;
run;
data MSE2;
set out2 nobs = nobs;
if _n_ <= nobs * 0.8 then SE_train2 = (nox - pred)**2;
else SE_train2 = .;
if _n_ > nobs * 0.8 then SE_test2 = (nox - pred)**2;
else SE_test2 = .;
run;
proc sql;
select avg(SE_train2) as train_MSE2, avg(SE_test2) as test_MSE2 from MSE2;
run;
quit; /* 0.003078 0.003021    */
/* dorzucam kwadraty zmiennych */
data model_data2;
    set model_Data;
    age2 = age**2;
	indus2 = indus**2;
	tax2 = tax**2;
	dis2 = dis**2;
	ptratio2 = ptratio**2;
run;
proc reg data=model_data2 plots=(criteria(label) cp);                        
    model nox = indus tax age dis ptratio age2 indus2 tax2 dis2 ptratio2/ 
       selection=rsquare cp;   
	plot cp.*np. / cmallows=black; 
	title 'All models';
run;
/* najlepszy dla  5 zmiennych: indus tax dis ptratio age2 */
proc glmselect data=model_data2;
 model nox = indus tax age dis ptratio age2 indus2 tax2 dis2 ptratio2
      /selection=lasso(stop=none choose=AIC);
run;
/* z lasso: dis indus age2 tax ptratio */
proc glm data=model_data2;
model score_train = indus tax dis ptratio age2;
output out=out3 p=pred;
run; 
data MSE3;
set out3 nobs = nobs;
if _n_ <= nobs * 0.8 then SE_train3 = (nox - pred)**2;
else SE_train3 = .;
if _n_ > nobs * 0.8 then SE_test3 = (nox - pred)**2;
else SE_test3 = .;
run;
proc sql;
select avg(SE_train3) as train_MSE3, avg(SE_test3) as test_MSE3 from MSE3;
run;
quit; /* 0.003272 0.002959 R^2 = 0.758442*/


/* czyli ostatecznie najlepiej pozostaæ przy regresji liniowej, ale z kwadratami
nox_i = 0.6208490655 + 0.0045695652*indus_i + 0.0001727817*tax_i + 0.0000088418*age^2_i - .0160684796*dis_i -.0095734461*ptratio_i + epsilon_i 
*/



