data mobile;
	set '/folders/myfolders/final project/mobilead.sas7bdat';
run;

proc contents data=mobile;
run;

proc sql;
select count(distinct(publisher_id_class)) as num_pub,
count(distinct(device_make_class)) as num_models,
count(distinct(device_platform_class)) as no_of_platform,
count(distinct(device_os_class)) as num_os , 
count(distinct(device_height)) as num_height , 
count(distinct(device_width)) as num_width, 
count(distinct(Resolution)) as num_resolution,
count(distinct(device_volume)) as num_vol,
count(distinct(Wifi)) as num_wifi,
count(distinct(Install)) as num_install
from mobile;

data mobile;
set mobile (rename=(device_platform_class=device_platform));
if device_platform_class = 'iOS' then device_platform = 0; else device_platform = 1;
drop device_platform_class;
run;

 

proc univariate data=mobile plots;
var Resolution device_height device_width device_volume;
*histogram resolution device_height device_width;
run;


proc corr data=mobile;
var Resolution device_height device_width;
Run;

/* generate indicator variables using glm_mod - useful to run proc reg for the categorical variables */
proc glmmod data=mobile outdesign=main_with_indicators1 noprint; 
 class wifi publisher_id_class device_platform publisher_id_class device_os_class device_make_class;
 model install =  device_volume wifi resolution device_height device_width device_make_class publisher_id_class device_platform device_os_class / noint;
run;



/* Create training and test datasets. 70% of sample in training  */
proc surveyselect data=main_with_indicators1  out=mobile_sampled outall samprate=0.7 seed=10;
run;

data mobile_training mobile_test;
 set mobile_sampled;
 if selected then output mobile_training; 
 else output mobile_test;
run;

/* Stepwise selection with AIC to select next step, and cross-validation to choose best model*/

proc glmselect data=mobile seed=2 plots=all;
class wifi(split)  publisher_id_class(split) device_os_class(split) device_make_class(split) device_platform(split);
partition fraction(validate=0.2); /* Set aside 20% of data for validation */
model install =  device_volume wifi resolution device_height device_width device_make_class publisher_id_class device_platform device_os_class
/selection=stepwise(select=aic choose=validate) hierarchy=single;
performance buildsscp=incremental;
run;



/* var selection*/
ods rtf file="/folders/myfolders/final project/stepwise selection1.rtf";
proc glmselect data=main_with_indicators1 plots=all;
 model install = Col1 Col2 Col3 Col4 Col5 Col6 Col7 Col8 Col9 Col10 Col11 Col12 Col13 Col14 Col15 Col16 Col17 Col18 Col19 Col20 Col21 Col22 Col23 Col24 Col25 Col26 Col27 Col28 Col29
  /selection=stepwise(select=sl sle=0.15) stats=all showpvalues;
run;
ods rtf close;



/* Logistic Regression selected cols*/
proc logistic data=mobile_training outmodel=Logitmodel;
 /*class wifi publisher_id_class device_platform publisher_id_class device_os_class;*/
 logit: model install (event='1') = Col3 Col5 Col8 Col12 Col13 Col18 Col19 Col22 Col23 Col24 Col25;
 
score data=mobile_test out=mobile_logit_predict outroc=roc_details; /* predictions are made only for the test dataset specified*/
run;



/* To create ROC table for logit model - we proceed in two steps.*/
/* Step 1: Estimate model and store estimated model in a dataset*/
proc logistic data=mobile_training outmodel=Logitmodel;
 logit: model install (event='1') = Col3 Col5 Col8 Col12 Col13 Col18 Col19 Col22 Col23 Col24 Col25;
run;

/* Step 2: Use estimated model to ony score test data and create a roc table */
proc logistic inmodel=Logitmodel;
 score data=mobile_test outroc=_logit_roc;
run;

proc sql;
select _PROB_, _FALPOS_, _FALNEG_, ((_FALNEG_*25) + _FALPOS_) as cost_25, ((_FALNEG_*50) + _FALPOS_) as cost_50, ((_FALNEG_*75) + _FALPOS_) as cost_75, ((_FALNEG_*100) + _FALPOS_) as cost_100
from  _logit_roc;
quit;
run;

proc sql;
title 'Min Cost for FN:FP = 50';
select _PROB_, _FALPOS_, _FALNEG_, ((_FALNEG_*50) + _FALPOS_) as cost_50 from _logit_roc  having cost_50 = min(cost_50 );
quit;
run;

proc sql;
title 'Min Cost for FN:FP = 75';
select _PROB_, _FALPOS_, _FALNEG_, ((_FALNEG_*75) + _FALPOS_) as cost_75 from _logit_roc  having cost_75 = min(cost_75 );
quit;
run;
 
proc sql;
title 'Min Cost for FN:FP = 100';
select _PROB_, _FALPOS_, _FALNEG_, ((_FALNEG_*100) + _FALPOS_) as cost_100 from _logit_roc  having cost_100 = min(cost_100 );
quit;
run;

/* Part 2 */
/* Linear probability model */
/* Make predictions for test observations */
proc reg data=mobile_sampled;
 linear: model install =  Col3 Col5 Col8 Col12 Col13 Col18 Col19 Col22 Col23 Col24 Col25;
 weight selected;
 output out=mob_lin_predict p=mob_linear_predictions; /* predictions are made for all observations - training and test */
quit;

/* To plot ROC curve based on predictions from linear Probability model */
proc logistic data=mob_lin_predict plots=roc(id=prob);
 linear: model install =  Col3 Col5 Col8 Col12 Col13 Col18 Col19 Col22 Col23 Col24 Col25 / nofit;
 roc pred=mob_linear_predictions;
 where selected=0;
run;

/*ROC curve on test data */
proc logistic data=mob_lin_predict plots=roc(id=prob);
 model install (event='1') = Col3 Col5 Col8 Col12 Col13 Col18 Col19 Col22 Col23 Col24 Col25 / nofit;
 roc pred=mob_linear_predictions;
run;

/* To create ROC table for Linear probability model - we proceed in two steps.*/
/* Step 1: Estimate model and store estimated model in a dataset*/
proc logistic data=mobile_training outmodel=mob_LP_model;
 linear: model install(event='1') = Col3 Col5 Col8 Col12 Col13 Col18 Col19 Col22 Col23 Col24 Col25;
run;

/* Step 2: Use estimated model to ony score test data and create a roc table */
proc logistic inmodel=mob_LP_model;
 score data=mobile_test outroc=lin_logit_roc;
run;


proc sql;
select _PROB_, _FALPOS_, _FALNEG_, ((_FALNEG_*25) + _FALPOS_) as cost_25, ((_FALNEG_*50) + _FALPOS_) as cost_50, ((_FALNEG_*75) + _FALPOS_) as cost_75, ((_FALNEG_*100) + _FALPOS_) as cost_100
from  lin_logit_roc;
quit;
run;

proc sql;
title 'Min Cost for FN:FP = 25';
select _PROB_, _FALPOS_, _FALNEG_, ((_FALNEG_*25) + _FALPOS_) as cost_25 from lin_logit_roc  having cost_25 = min(cost_25 );
quit;
run;

proc sql;
title 'Min Cost for FN:FP = 50';
select _PROB_, _FALPOS_, _FALNEG_, ((_FALNEG_*50) + _FALPOS_) as cost_50 from lin_logit_roc  having cost_50 = min(cost_50 );
quit;
run;

proc sql;
title 'Min Cost for FN:FP = 75';
select _PROB_, _FALPOS_, _FALNEG_, ((_FALNEG_*75) + _FALPOS_) as cost_75 from lin_logit_roc  having cost_75 = min(cost_75 );
quit;
run;
 
proc sql;
title 'Min Cost for FN:FP = 100';
select _PROB_, _FALPOS_, _FALNEG_, ((_FALNEG_*100) + _FALPOS_) as cost_100 from lin_logit_roc  having cost_100 = min(cost_100 );
quit;
run;



proc sql;
select _PROB_, _FALPOS_, _FALNEG_, ((_FALNEG_*0.001) + _FALPOS_) as A, ((_FALNEG_*0.005) + _FALPOS_) as B, ((_FALNEG_*0.010) + _FALPOS_) as C, ((_FALNEG_*0.015) + _FALPOS_) as D, 
((_FALNEG_*0.0020) + _FALPOS_) as E, ((_FALNEG_*0.025) + _FALPOS_) as F, ((_FALNEG_*0.030) + _FALPOS_) as G, ((_FALNEG_*0.035) + _FALPOS_) as H,
((_FALNEG_*0.0040) + _FALPOS_) as I, ((_FALNEG_*0.045) + _FALPOS_) as J, ((_FALNEG_*0.050) + _FALPOS_) as K
from  lin_logit_roc;
quit;
run;
 


proc sql;
title 'Min Cost for FN:FP = 0.001';
select _PROB_, _FALPOS_, _FALNEG_, ((_FALNEG_*0.001) + _FALPOS_) as A from lin_logit_roc  having A = min(A );
quit;
run;
proc sql;
title 'Min Cost for FN:FP = 0.005';
select _PROB_, _FALPOS_, _FALNEG_, ((_FALNEG_*0.005) + _FALPOS_) as B from lin_logit_roc  having B = min(B );
quit;
run;
proc sql;
title 'Min Cost for FN:FP = 0.010';
select _PROB_, _FALPOS_, _FALNEG_, ((_FALNEG_*0.010) + _FALPOS_) as C from lin_logit_roc  having C = min(C );
quit;
run;
proc sql;
title 'Min Cost for FN:FP = 0.015';
select _PROB_, _FALPOS_, _FALNEG_, ((_FALNEG_*0.015) + _FALPOS_) as D from lin_logit_roc  having D = min(D );
quit;
run;
proc sql;
title 'Min Cost for FN:FP = 0.020';
select _PROB_, _FALPOS_, _FALNEG_, ((_FALNEG_*0.020) + _FALPOS_) as E from lin_logit_roc  having E = min(E);
quit;
run;

proc sql;
title 'Min Cost for FN:FP = 0.025';
select _PROB_, _FALPOS_, _FALNEG_, ((_FALNEG_*0.025) + _FALPOS_) as F from lin_logit_roc  having F = min(F);
quit;
run;

proc sql;
title 'Min Cost for FN:FP = 0.030';
select _PROB_, _FALPOS_, _FALNEG_, ((_FALNEG_*0.030) + _FALPOS_) as G from lin_logit_roc  having G = min(G);
quit;
run;
proc sql;
title 'Min Cost for FN:FP = 0.035';
select _PROB_, _FALPOS_, _FALNEG_, ((_FALNEG_*0.035) + _FALPOS_) as H from lin_logit_roc  having H = min(H);
quit;
run;

proc sql;
title 'Min Cost for FN:FP = 0.040';
select _PROB_, _FALPOS_, _FALNEG_, ((_FALNEG_*0.040) + _FALPOS_) as I from lin_logit_roc  having I = min(I);
quit;
run;
proc sql;
title 'Min Cost for FN:FP = 0.045';
select _PROB_, _FALPOS_, _FALNEG_, ((_FALNEG_*0.045) + _FALPOS_) as J from lin_logit_roc  having J = min(J);
quit;
run;
proc sql;
title 'Min Cost for FN:FP = 0.050';
select _PROB_, _FALPOS_, _FALNEG_, ((_FALNEG_*0.050) + _FALPOS_) as K from lin_logit_roc  having K = min(K);
quit;
run;



