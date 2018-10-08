libname Time 'C:\Users\senor\Documents\Time_Series';
run;


/*Plot the TS object */ 
proc arima data=work.timeseries plot=all;
	identify var=_7_278 nlag=10;
run;
quit;

/*Fit sines/cosines for complex seasonality */
data ts;
set work.timeseries;
pi=constant("pi");
s1=sin(2*pi*1*_n_/(24*365.25));  *every sine need related cosine;
c1=cos(2*pi*1*_n_/(24*365.25));  *24 times 365.25 is length of the season (divide by length of season) for monthly data; 
s2=sin(2*pi*2*_n_/(24*365.25));  * 4-5 is what we normally see for the amt of sines/cosines; 
c2=cos(2*pi*2*_n_/(24*365.25));  *_n_ is observation number; 
s3=sin(2*pi*3*_n_/(24*365.25));
c3=cos(2*pi*3*_n_/(24*365.25));
s4=sin(2*pi*4*_n_/(24*365.25));
c4=cos(2*pi*4*_n_/(24*365.25));
run;


/*Look for a model on the training dataset with a difference of 1 */ 
proc arima data=ts plot=all;
identify var=_7_278(1) crosscorr=(s1 c1 s2 c2 s3 c3 s4 c4); *inputting x vars fitting a regression; 
estimate p=(1, 2, 3, 4) q=(1, 12, 24) input=(s1 c1 s2 c2 s3 c3 s4 c4) method=ML; 
run;
quit;


/*Forecasting for a week after taking care of seasonality*/ 
data alldata;
set work.alldata;
pi=constant("pi");
s1=sin(2*pi*1*_n_/(24*365.25));  *every sine need related cosine;
c1=cos(2*pi*1*_n_/(24*365.25));  *24 times 365.25 is length of the season (divide by length of season) for monthly data; 
s2=sin(2*pi*2*_n_/(24*365.25));  * 4-5 is what we normally see for the amt of sines/cosines; 
c2=cos(2*pi*2*_n_/(24*365.25));  *_n_ is observation number; 
s3=sin(2*pi*3*_n_/(24*365.25));
c3=cos(2*pi*3*_n_/(24*365.25));
s4=sin(2*pi*4*_n_/(24*365.25));
c4=cos(2*pi*4*_n_/(24*365.25));
run;


proc arima data=Alldata plot=all;
	identify var=_7_278(1) crosscorr=(s1 c1 s2 c2 s3 c3 s4 c4); *inputting x vars fitting a regression; 
    estimate p=(1, 2, 3, 4) q=(1, 12, 24) input=(s1 c1 s2 c2 s3 c3 s4 c4) method=ML; 
	forecast lead=168 back=168;
run;
quit;


data time.residualdata;
	set testdata; 
	if _n_ > 93622; 
run; 
