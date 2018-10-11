libname Time 'C:\Users\senor\Documents\Time_Series'; 
run; 

/*Visualize the data */ 

proc sgplot data=time.ts_hw2;
series x=datetime y=well_ft;
series x=datetime y=rain_in;
*series x=datetime y=tide_ft;
run;
quit;


/* Step 1: Need to see if residuals are stationary */
proc arima data=time.ts_hw2;
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


/*Stationary is confirmed so decide on which predictors are important */
proc arima data=time.ts_hw2;
identify var=well_ft(1) crosscorr=(rain_in tide_ft) nlag=40; *look at P-values to see sig; 
estimate input=(rain_in tide_ft) p=2 q=(1,2,3,4,21,22,23,24) method=ML;
*estimate input=(rain_in tide_ft) p=2 q=(12) method=ML; *look at PACF for MA terms; 
run;
quit;
