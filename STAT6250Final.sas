/*Taiwaree Vannasiri 
Final Exam
STAT 6250*/

/*Consider the data set from HW 3 (the one with all 6 cells, called "problem3" in the solutions). In your code, refer to this data set as "problem3".

   1. Write a macro that calls "proc compare" and allows you to compare two data sets 
(macro variables "base" and "with") for two variables (macro variables "from" and "to"),
using an "id" variable ("myid"). Note: if you find in later comparisons that 
you are getting differences ending with E-16, like in class, 
you may use "criterion=0.000001" as an option in proc compare.*/

title "Problem 1";
%macro compare(base, with, from, to, myid);

proc compare base=&base
		compare=&with
		criterion=0.000001;
		var &from;
		with &to;	
          id &myid;	

run;
%mend;

/*  2. Use the macro you wrote for HW 5 to compare the model with factors A, B, and the interaction to the model
         A. ... with just factors A and B. Give only the results of the F-test.
         B. ... with just factor A. Give only the results of the F-test. */


%macro stack(indata, outdata);
	data &outdata;
		set &indata;
	run;
%mend;

data oneI;
	a="one";
	b="I";
	input y;
	cards;
	1
	2
	3
run;
data oneII;
	a="one";
	b="II";
	input y @@;
	cards;
	5 6 7 8 9
run;
data oneIII;
	a="one";
	b="III";
	input y @@;
	cards;
	10 11
run;
data twoI;
	a="two";
	b="I";
	input y @@;
	cards;
	101 102 103 100 104
run;
data twoII;
	a="two";
	b="II";
	input y @@;
	cards;
	106 107 108
run;
data twoIII;
	a="two";
	b="III";
	input y @@;
	cards;
	90 91
run;


%macro myglm(indata, outcome, classx, modelx, myoutstat);    
				
title "&indata &modelx";
proc glm data=&indata 	outstat=&myoutstat noprint;
	class &classx; 
	model &outcome = &modelx;
	output out=myout r=res p=fitted;
    run;
%mend;

%macro problem2(indata, outcome, classx1,classx2, modelx1, modelx2, myoutstat1, myoutstat2);
%myglm(&indata, &outcome, &classx1, &modelx1, &myoutstat1);
%myglm(&indata, &outcome, &classx2, &modelx2, &myoutstat2);

data mymodel;
	set &myoutstat1 &myoutstat2;
run;
proc sort data=mymodel;
	by df;
run;

data mymodel2;
	set mymodel;
if _source_ = "ERROR";
	lagsse = lag(ss);
	lagdf = lag(df);
	f = ((ss - lagsse)/(df-lagdf))
		/ (lagsse / lagdf);
	pvalue = 1-cdf('F',f, df-lagdf, lagdf);
if f = . and pvalue = . then delete;
	run;

proc print data=mymodel2;

var f ;  		 /*give a result of f value*/

run;

%mend;
 title "Problem 2 model";

%stack(oneIII oneII oneI twoIII twoII twoI, problem2);
title "Problem 2A";
%problem2(problem2, y,a b, a b, a b a*b, a b, myoutstat1, myoutstat2);  
/*compare the model with factors A, B, and the interaction to the model  with just factors A and B.*/

title "Problem 2B";
%problem2(problem2,y, a b, a , a b a*b, a , myoutstat1, myoutstat2);

/*compare the model with factors A, B, and the interaction to the model  with just factors A*/




/* 3. Regardless of your results for problem 2, we are interested in the LSMEANS for the model 
		with only factors A and B.
         3A. Get the LSMEANS for a and b in a data set using ODS.*/
%stack(oneIII oneII oneI twoIII twoII twoI, problem2);
title "problem 3A";
ods trace on;
ods listing close;
ods output  lsmeans = lsmeansglm;

proc glm data=problem2 ;
     class a b;
	model y = a b;
	lsmeans a b;
run;
ods listing;
proc print data = lsmeansglm;
run;
ods trace off;


/*3B. Get the LSMEANS using proc sql from the fitted values in the output statement of proc glm. 
 Note, unlike HW 4, these LSMEANS should be in one data set (hint: you can stack data sets in proc sql). */
title "problem 3B";

proc glm data=problem2  ;
     class a b;
	model y = a b;
	output out=myout r=res p=fitted;
run;

proc sql;
	create table fitted as
	select distinct a, b, fitted from myout;
quit;

proc sql;
	create table lsmeansa 
as select a, mean(fitted) as lsmeansa from fitted
	group by a;
quit;
proc sql;
	create table lsmeansb
as select mean(fitted)  as lsmeansb , b from fitted
	group by b;
quit;

proc sql;
create table lsmeans1 as 
select distinct a,
lsmeansa , lsmeansb, b  
from lsmeansa a  full join lsmeansb b
on a.lsmeansa = b.lsmeansb;
quit;

proc sql;
create table lsmeanssql
as select a,
case 
when lsmeansa = . then lsmeansb 
when lsmeansb = . then lsmeansa
else lsmeansb
end as lsmeansab 
, b
from lsmeans1
order by b, a;
quit;
proc sql;
select * from lsmeanssql;
quit;

/*3C Use your proc compare macro to determine whether you got the same answers
 using proc sql as SAS had from proc means (the ID variables here are a and b). */
title2 "proc compare";

proc sort data = lsmeansglm;  /*sort id variables before comparing*/
by a b;
run;

proc sort data = lsmeanssql;
by a b;
run;   
%macro compare(base, with, from, to, myid);

proc compare base=&base
		compare=&with
        criterion=0.000001;
		
		var &from;
		with &to;	
          id &myid;	
		  
run;
%mend;

%compare(lsmeansglm, lsmeanssql, ylsmean, lsmeansab, a b);


/* 4. Consider the (corrected) total sum of squares from the model. */
   /*4A Get the SS Total from SAS in a data set using an ODS statement to 
    create the data set and either proc sql or a data step to eliminate other observations.*/
     
%stack(oneIII oneII oneI twoIII twoII twoI, problem2);

title "Problem 4A";

ods trace on;
ods listing close;
ods output overallanova = myanovatable;
proc glm data=problem2 ;
    class a b;
	model y = a b;
	
run;
ods listing ;
proc print data=myanovatable;
run;

proc sql ;
create table SSTOA
as select  ss as ssto_a
 from  myanovatable
where df = 19
;
quit;

proc sql;
select * from SSTOA;
quit;
ods trace off;


/*  4B Get the SS Total from the Proc GLM outstat data set. (Hint: Remember that this is not
given to you directly, so you will need to use proc means or proc sql to add values in the data set.)*/


title "Problem 4B";
proc glm data = problem2 outstat= myoutstat noprint;    /*Get the SS Total from the Proc GLM outstat data set.*/
class a b;
model y = a b;
run;
quit;

proc sql ;
create table glmsst
as select ss from myoutstat
where _type_ eq "ERROR" or _type_ eq "SS1" ;
quit;

proc sql;             /* using proc sql to add values in the data set*/
create table SSTOB
as select sum(ss) as SSTO_B
from glmsst;
quit;
proc sql;
select * from SSTOB;
quit;

/*4C Get the SS Total using proc iml, as in HW 5.*/
title "Problem 4C";

proc iml;
	use problem2;
	read all var{y} into y;
	print y;

	reset log print;

	SSTO_C= (t(y)*y) - ((1/20)*t(y)*j(20,20,1)*y); /*SSTO = Y'Y - (1/n)*Y'*J*Y 
													=  sum of the squares of (y - ybar)
										               obtain formula from Applied linear regression models 4th edition, Page 204
												        , Kutner Nachtsheim Netter*/
	print SSTO_C;                                    /*check SSTO from log file: SSTO =  0.2189975 */
	
	create sstoc from SSTO_C[colname="SSTO_C"];
	append from SSTO_C;
	quit;
 proc print data = sstoc;
 run;

/* 4.D Get the SS Total using the "css" command in proc sql. */
title "Problem 4D";

proc sql;
create table sstod as
	select css(y) as SSTO_D     
		from problem2;
quit;
proc sql;
select * from sstod;
quit;


/*4.E Compare the results from parts B-D (separately) to the SS Total in 
 your ODS data set from part A using your "proc compare" macro to ensure you are 
getting the right answer (there is no "id" variable for this problem). */

%macro compare(base, with, from, to, myid);
proc compare base=&base
		compare=&with
		criterion=0.000001;
		var &from;
		with &to;	
          id &myid;	
run;
%mend;

%compare(sstoa, sstob, ssto_a, ssto_b, );
%compare(sstoa, sstoc, ssto_a, ssto_c, );
%compare(sstoa, sstod, ssto_a, ssto_d, );







/*print output*/

ods pdf file = "C:\Users\Taiwaree\Desktop\Final Exam\STAT6250Final.VannasiriTaiwaree.pdf";

title "Problem2";
%problem2(problem2, y,a b, a b, a b a*b, a b, myoutstat1, myoutstat2);  
/*compare the model with factors A, B, and the interaction to the model  with just factors A and B.*/

%problem2(problem2,y, a b, a , a b a*b, a , myoutstat1, myoutstat2);

/*compare the model with factors A, B, and the interaction to the model  with just factors A*/

title "Problem 3A";
proc print data = lsmeansglm;
run;

title "Problem 3B";
proc sql;
select * from lsmeanssql;
quit;

title "Problem 3C";
%compare(lsmeansglm, lsmeanssql, ylsmean, lsmeansab, a b);

title "Problem 4A";
proc sql;
select * from SSTOA;
quit;

title "Problem 4B";
proc sql;
select * from SSTOB;
quit;

title "Problem 4C";
proc print data = sstoc;
 run;


title "Problem 4D";
proc sql;
select * from sstod;
quit;

title "Problem 4E";
%compare(sstoa, sstob, ssto_a, ssto_b, );
%compare(sstoa, sstoc, ssto_a, ssto_c, );
%compare(sstoa, sstod, ssto_a, ssto_d, );


ods pdf close;
