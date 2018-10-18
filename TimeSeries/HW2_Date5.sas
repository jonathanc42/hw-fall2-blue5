libname Time 'C:\Users\senor\Documents\Time_Series'; 
run; 

/*Subset Data for right end date */ 

data date5sub; 
	set time.date5;
	if _n_ < 14771;
run; 


/*Visualize the data */ 

proc sgplot data=date5sub;
series x=datetime y=well_ft;
series x=datetime y=rain_in;
*series x=datetime y=tide_ft;
run;
quit;


/* Step 1: Need to see if residuals are stationary */
proc arima data=date5sub;
identify var=well_ft crosscorr=(rain_in tide_ft); *add x vars rain and tide; 
estimate input=(rain_in tide_ft) p=2 method=ML; *add p=2 to help remove some of the autocorr; 
forecast out=ts_residuals; *saving residuals; 
run;
quit;

/* Look at zero mean test it is stationary */ 
proc arima data=ts_residuals;
identify var=residual stationarity=(adf=2);
run;
quit;


/*Stationary is confirmed so decide on which predictors are important then forecast */
proc arima data=date5sub out=testdate5;
identify var=well_ft  crosscorr=(rain_in) nlag=40; *look at P-values to see sig; 
estimate input=(rain_in) p=4 q=(1, 5, 23) method=ML;
forecast lead=168 back=168;
run;
quit;

data time.residualdate5;
	set testdate5; 
	if _n_ > 14602; 
run; 

/*Absolute value of the residuals divided by actual */ 
data predictedvalue; 
	set time.residualdate5; 
	yhat= Residual; 
	yhatabs= abs(yhat)/abs(well_ft);
	ysum + yhatabs;
run; 

/*Get MAPE (mean value) */ 
proc means data=predictedvalue;
	var yhatabs; 
run; 
