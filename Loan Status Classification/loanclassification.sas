proc import datafile="/home/u64316547/STAT 448/home_loan.csv"
            out=loans_raw
            dbms=csv
            replace;
    getnames=yes;
run;

data loans;
    set loans_raw;
    length Loan_Term_Group $10;

    if Loan_Amount_Term < 180 then Loan_Term_Group = "Short";
    else if 180 <= Loan_Amount_Term < 360 then Loan_Term_Group = "Medium";
    else if Loan_Amount_Term => 360 then Loan_Term_Group = "Long";
run;

/* Summary statistics and correlations for continuous variables */
proc corr data=loans;
	var ApplicantIncome CoapplicantIncome LoanAmount;
run;

/* Frequency tables for categorical variables*/
proc freq data=loans;
	tables Gender Married Dependents Education Self_Employed Loan_Amount_Term Credit_History Property_Area Loan_Status / nocum;
run;

/* Frequency table for Loan_Term_Group*/
proc freq data=loans;
	tables Loan_Term_Group / nocum;
run;



/* Histograms and normality check for continuous variables */
proc univariate data=loans normal plots;
  class Loan_Status;
  var ApplicantIncome CoapplicantIncome LoanAmount;
run;


/* 2 */

/* Cross-tabulation with Loan_Status */
proc freq data=loans;
   tables Gender*Loan_Status
          Married*Loan_Status
          Education*Loan_Status
          Self_Employed*Loan_Status
          Credit_History*Loan_Status
          Property_Area*Loan_Status
          Dependents*Loan_Status
          Loan_Term_Group*Loan_Status / chisq nocol norow expected;
run;

proc npar1way data=loans wilcoxon;
  class Loan_Status;
  var ApplicantIncome CoapplicantIncome LoanAmount;
run;

/* 3 */

proc logistic data=loans;
	class Gender Married Dependents Education Self_Employed Credit_History Property_Area Loan_Term_Group / param=ref;
	model Loan_Status(event='Y') = Gender Married Dependents Education Self_Employed Credit_History Property_Area Loan_Term_Group ApplicantIncome CoapplicantIncome LoanAmount / selection=stepwise slentry=0.05 slstay=0.05;
run;

proc logistic data=loans plots=all;
	class Married Credit_History Property_Area;
	model Loan_Status(event='Y') = Married Credit_History Property_Area / lackfit ctable pprob=0.5;
run;



