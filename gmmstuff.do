/* gmm examples */

set more off

sysuse auto, clear
reg mpg weight length

capture program drop mynormal_reg
program mynormal_reg
    version 14.1
	args lnf mu sigma
	quietly replace `lnf' = ln(normalden($ML_y1 , `mu', `sigma'))
end

ml model lf mynormal_reg (mpg = weight) ( )
ml maximize

gmm ( (mpg - {b0} - {b1}*weight)/(2*{v=1}^2) ) ///
    ( (mpg - {b0} - {b1}*weight)^2 - {v}^2 ), ///
	instruments(1: weight) instruments(2: ) winit(unadjusted, independent)

