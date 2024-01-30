/******************************************************************************/

/* Details: */
/* everyone has had a procedure (hip replacement)                        */
/* some people are in worse shape afterwards (or even before) unobserved */
/* some people have access to better treatments (unobserved)             */
/* It is perhaps even possible that the above two are correlated. This   */
/* might occur, say, if someone is poor and living alone.                */

/* Outcome variable (expenses, say) */
/* The following source gives us some data on age */

clear all
set obs 10000
gen age =  rnormal(76, 5)

/* Just a guess for calibration (from this website) */
/* https://www.bcbs.com/the-health-of-america/reports/planned-knee-and-hip-replacement-surgeries-are-the-rise-the-us */

scalar age_cost = 50 
gen cost = `=age_cost'*age + exp(rnormal(10, .4))
hist cost
