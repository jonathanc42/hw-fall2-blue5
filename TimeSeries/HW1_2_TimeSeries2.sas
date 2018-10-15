libname Time 'C:\Users\senor\Documents\Time_Series';
run;


/*Plot the TS object */ 
proc arima data=Time.timeseries plot=all;
	identify var=well_ft(1) nlag=10;
run;
quit;



/*Look for a model on main dataset DOES NOT NEED SINES AND COSINES*/ 
proc arima data=time.Ts_hw2 plot=all;
identify var=well_ft(1)  nlag=25; *inputting x vars fitting a regression; 
estimate p=5 q=(1,15)   method=ML; 
run;
quit;


/*Forecast for the last week (IF BAD forecast find another model in previous proc arima step */ 

proc arima data=time.Ts_hw2 plot=all out=testdata;
	identify var=well_ft(1); *inputting x vars fitting a regression; 
    estimate p=5 q=(1,15)  method=ML; 
	forecast lead=168 back=168;
run;
quit;


/* Calculate MAPE by creating a dataset with last week of obsv */ 
data time.residualdata;
	set testdata; 
	if _n_ > 21286; 
run; 

/*Absolute value of the residuals divided by actual */ 
data predictedvalue; 
	set time.residualdata; 
	yhat= Residual; 
	yhatabs= abs(yhat)/abs(well_ft);
	ysum + yhatabs;
run; 

/*Get MAPE (mean value) */ 
proc means data=predictedvalue;
	var yhatabs; 
run; 
