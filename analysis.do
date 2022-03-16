* ------------------------------------------------------------------------------
* read data
* ------------------------------------------------------------------------------

cd "/Users/barnejr/Desktop/"
use "data.dta", clear

label drop _all



* ------------------------------------------------------------------------------ 
* descriptives and recodes
* ------------------------------------------------------------------------------

recode impulse_control crime_hist peer_resist (-9=.)
recode recidivism_f1-recidivism_f6 sub_depend (1=0) (2=1) 
sum recidivism_f1 recidivism_f2 impulse_control crime_hist peer_resist gender sub_depend 

						 
* create risk assessment, graph, etc
factor impulse_control crime_hist peer_resist gender sub_depend 
predict riskAssess
hist riskAssess, xtitle(Risk Assessment Scores) freq
gr export "RAscores.pdf", replace
gr close
gsort -riskAssess
list riskAssess in 1/10



* ------------------------------------------------------------------------------
* elo
* ------------------------------------------------------------------------------

capture drop phi
capture drop recidivism_f1_01 
capture drop riskAssess_01


* step 1: gen phi from risk assessment score
gen phi_A = riskAssess+100
sum phi_A


* step 2: compute R
gen R_A = 10^(phi_A/400)

* bonus step: generate random matches
foreach i of varlist recidivism_f1 riskAssess phi_A R_A {
sort CASEID
gen `i'_B=`i'[_n+1] 
}
rename phi_A_B phi_B
rename R_A_B R_B

list CASEID recidivism_f1 riskAssess phi_A R_A recidivism_f1_B riskAssess_B phi_B R_B in 1/10


* step 3: compute expected probability (i.e., predicted probability) of recidivism
gen E_A = R_A/(R_A+R_B)
gen E_B = R_B/(R_A+R_B)
list CASEID recidivism_f1 riskAssess phi_A R_A E_A recidivism_f1_B riskAssess_B phi_B R_B E_B in 1/10
* we need to average the phi_new variables since each person 
* now has two competitions


* step 4: assume A and B "compete"


* step 5: assign S=1 to winner, S=0 to loser, and S=0.5 for ties
gen S_A = 0
replace S_A = 1 if   recidivism_f1>recidivism_f1_B
replace S_A = 0.5 if recidivism_f1==recidivism_f1_B
replace S_A = . if   recidivism_f1==. | recidivism_f1_B==.

gen S_B = 0
replace S_B = 1 if   recidivism_f1_B>recidivism_f1
replace S_B = 0.5 if recidivism_f1_B==recidivism_f1
replace S_B = . if   recidivism_f1_B==. | recidivism_f1==.
list CASEID recidivism_f1 recidivism_f1_B S_A S_B in 1/10


* step 6: update phi values; first we need K
gen K = 32

gen phi_A_new = R_A+K*(S_A-E_A)
gen phi_B_new = R_B+K*(S_B-E_B)
list CASEID recidivism_f1 phi_A phi_A_new recidivism_f1_B phi_B phi_B_new S_A S_B in 1/10

gen personA_id = 4
gen personB_id = 10
list personA_id phi_A recidivism_f1 phi_A_new personB_id phi_B recidivism_f1_B phi_B_new S_A S_B in 1

* average phi_new variables across the two competitions
gen phi_new_avg = (phi_A_new + phi_B_new [_n-1])/2
list CASEID recidivism_f1 phi_A phi_A_new recidivism_f1_B phi_B phi_B_new phi_new_avg in 1/10



* ------------------------------------------------------------------------------
* Results
* ------------------------------------------------------------------------------

log using "results.txt", text replace

* create Elo_rank score to be used as new predictor 
* along with baseline risk assessment
egen Elo_rank = rank(phi_new_avg)
egen miss = rowmiss(Elo_rank riskAssess)

sum Elo_rank
hist Elo_rank, xtitle(Elo Scores) freq
gr export "EloScores.pdf", replace
gr close


* table 1
sum recidivism_f1 recidivism_f2 riskAssess impulse_control crime_hist ///
	peer_resist sub_depend gender if recidivism_f2!=. & miss==0


* table 2, model 1
logit recidivism_f2 riskAssess			if miss==0
estat class
lroc
gr close


* table 2, model 2
quietly sum Elo_rank
local min = `r(min)'
local max = `r(max)'
logit recidivism_f2 Elo_rank			if miss==0
estat class
lroc
gr close


* table 2, model 3
logit recidivism_f2 Elo_rank riskAssess if miss==0
estat class
lroc
gr close

log close
