* read data
cd "/Users/j.c.barnes/Box Sync/manuscripts/Elo/data/pathways/"
use "data.dta", clear
 
* descriptives and recodes
*sum impulse_control crime_hist peer_resist gender sub_depend 
recode impulse_control crime_hist peer_resist (-9=.)
sum recidivism_f1 recidivism_f2 impulse_control crime_hist peer_resist gender sub_depend 

sum recidivism_f1-recidivism_f6
recode recidivism_f1-recidivism_f6 (1=0) (2=1) 
label drop _all
sum recidivism_f1-recidivism_f6

* global recidivism variable
gen recidAll = 0 
replace recidAll = 1 if	 recidivism_f1==1 | recidivism_f2==1 | recidivism_f3==1 | ///
						 recidivism_f4==1 | recidivism_f5==1 | recidivism_f6==1
replace recidAll = . if  recidivism_f1==. & recidivism_f2==. & recidivism_f3==. & ///
						 recidivism_f4==. & recidivism_f5==. & recidivism_f6==.
tab recidAll
						 
* create risk assessment
factor impulse_control crime_hist peer_resist gender sub_depend 
predict riskAssess
hist riskAssess, xtitle(Risk Assessment Scores) freq
gr export "RAscores.pdf", replace


gsort -riskAssess
list riskAssess in 1/10

* binary risk assessment variable
sum riskAssess, d
gen atRisk = 0
replace atRisk = 1 if riskAssess>=`r(p75)'
replace atRisk = . if riskAssess==.
sum atRisk


						 
						 
						 
* prediction models		
logit recidivism_f1 impulse_control crime_hist peer_resist gender sub_depend, or
estat class
capture drop prob1
predict prob1
hist prob1
*gr export "prob1.pdf", replace
gr close

logit recidivism_f1 riskAssess, or



cls
foreach i of varlist recidivism_f1 recidivism_f2 recidivism_f3 recidivism_f4 recidivism_f5 recidivism_f6 {
logit `i' riskAssess, or
}



cls
foreach i of varlist recidivism_f1 recidivism_f2 recidivism_f3 recidivism_f4 recidivism_f5 recidivism_f6 {
logit `i' atRisk, or
}


/* new program for elo
drop _all
capture program drop elo
capture program define elo

* open data file
quietly use "/Users/jcbarnes/Box Sync/manuscripts/elo/pathways/data.dta", clear

factor impulse_control crime_hist peer_resist gender sub_depend moral_disengage
predict riskAssess
sum riskAssess, d
gen phi = 100
replace phi = 110 if riskAssess>=`r(p75)'
replace phi = . if riskAssess==.
sum phi

* sample 2 cases at random
quietly sample 2, count

gen R = 10^(phi/400)
gen prob = 0
replace prob = R/(R+R[_n+1]) if _n==1
replace prob = R/(R+R[_n-1]) if _n==2
order R prob

gen S = .
replace S = 1 in 1 if recidivism_f2>recidivism_f2[_n+1]
replace S = -1 in 2 if recidivism_f2>recidivism_f2[_n-1]
replace S = 0.5 in 1 if recidivism_f2==recidivism_f2[_n+1]
replace S = 0.5 in 2 if recidivism_f2==recidivism_f2[_n-1]
replace S = -1 in 1 if recidivism_f2<recidivism_f2[_n+1]
replace S = 1 in 2 if recidivism_f2<recidivism_f2[_n-1]
order recidivism_f2 S

gen K = 32

gen update = R+K*(S-prob)
order update

gen updatePhi = phi+update
order phi updatePhi

gen updateR = 10^(updatePhi/400)
gen updateProb = 0
replace updateProb = updateR/(updateR+updateR[_n+1]) if _n==1
replace updateProb = updateR/(updateR+updateR[_n-1]) if _n==2
order updateR updateProb

sum updateProb

end
*/
















* ------------------------------------------------------------------------------
* working
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
* later down the road: allow program to search for another valid case if opponent is missing recidivsim score


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







** Results
* create Elo_rank score to be used as new predictor additional to baseline risk assessment
egen Elo_rank = rank(phi_new_avg)
egen miss = rowmiss(Elo_rank riskAssess)

sum Elo_rank
hist Elo_rank, xtitle(Elo Scores) freq
gr export "EloScores.pdf", replace
gr close

cls
log using "results.txt", text replace

logit recidivism_f2 riskAssess			if miss==0
estat class
lroc
* here, let's focus on the positive predictive value






quietly sum Elo_rank
local min = `r(min)'
local max = `r(max)'
logit recidivism_f2 Elo_rank			if miss==0
estat class
lroc
* here, let's focus on the positive predictive value

margins, at(Elo_rank=(`min' (15) `max' ))
marginsplot
gr export "predictedProb.pdf", replace
gr close






logit recidivism_f2 Elo_rank riskAssess if miss==0
estat class
lroc
* here, let's focus on the positive predictive value





* sort based on elo to demonstrate how caseload could be prioritized
gsort -Elo_rank
*list CASEID recidivism_f2 Elo_rank in 1/100

cls
* high risk based on Elo
proportion recidivism_f2 if Elo_rank>=966 & Elo_rank!=.

* medium risk based on Elo
proportion recidivism_f2 if Elo_rank<966 & Elo_rank>100

* low risk based on Elo
proportion recidivism_f2 if Elo_rank<=100


log close
