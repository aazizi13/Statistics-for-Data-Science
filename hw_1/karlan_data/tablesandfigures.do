***************************************
* Project: "Karlan" - "Getting to the Top of Mind"
* Date: 04/18/2019
* Purpose: Do-file
***************************************

set more off
capture log close

global path "YOUR PATH HERE"
global data "${path}\data"
global export "${path}\table"
global log "${path}\log"
global ssc "${path}\adofiles"

*Set directory (do not change)
cd "${path}"

use "${data}\analysis_dataallcountries.dta", replace

*Create log-file*
log using "${log}\tablesandfigures", replace

*Call user-written commands
sysdir set PLUS "${ssc}"

local randfeature= "highint rewardint joint dc joint_single"
local covariates = "female age highschool_completed married inc_7d wealthy hyperbolic spent_b4isaved saved_asmuch missing_female missing_age missing_highschool_completed missing_married missing_saved_asmuch missing_spent_b4isaved"
local treatments = "rem_any gain_rem loss_rem rem_no_motive rem_motive incentive noincentive late_rem_any foto puzzle_ica"
local treatments1 = "rem_any gain_rem loss_rem rem_no_motive rem_motive late_rem_any foto puzzle_ica"
local treatments2 = "rem_any gain_rem loss_rem incentive noincentive"
local treatments3 = "rem_any gain_rem loss_rem late_rem_any"

***Table 3
*All values are not reproduced as the published values -- The "P-value from F-test" for "Wealthy", "Pooled Sample", should read ".0679", rather than ".093"//
*The mean for "Age", "Philippines", should read "32.22", rather than "32.339"
*All the values can be found in the logfile under "Table 3"

foreach x in female age highschool_completed {
		tabstat `x' if missing_`x'!=1, stats(mean sd N)
		xi: reg `x' `treatments' i.country if missing_`x'!=1, robust
		test `treatments'
	}

foreach x in wealthy {
		tabstat `x' if country==1 & missing_`x'!=1 | missing_`x'!=1 & country==3, stats(mean sd N)
		xi: reg `x' `treatments1' `treatments3' i.country if country==1 & missing_`x'!=1 | missing_`x'!=1 & country==3, robust
		test `treatments1' `treatments3'
}

foreach x in married saved_formal {
		tabstat `x' if country==2 & missing_`x'!=1 | missing_`x'!=1 & country==3, stats(mean sd N)
		xi: reg `x' `treatments3' i.country if country==2 & missing_`x'!=1 | missing_`x'!=1 & country==3, robust
		test `treatments3' 
}

foreach x in female age highschool_completed wealthy {
		tabstat `x' if country==1 & missing_`x'!=1, stats(mean sd N)
		reg `x' `treatments1' if country==1 & missing_`x' !=1, robust
		test `treatments1'
}
	
foreach x in female age highschool_completed married saved_formal {
		tabstat `x' if country==2 & missing_`x'!=1, stats(mean sd N)
		reg `x' `treatments2' if country==2 & missing_`x'!=1, robust
		test `treatments2'
}

foreach x in female age highschool_completed wealthy married saved_formal inc_7d saved_asmuch spent_b4isaved {
		tabstat `x' if country==3 & missing_`x'!=1, stats(mean sd N)
		reg `x' `treatments3' i.country if country==3 & missing_`x'!=1, robust
		test `treatments3'
}

* Savings Balance
tabstat quant_saved, stats(mean sd N)
tabstat quant_saved if country==1, stats(mean sd N)
tabstat quant_saved if country==2, stats(mean sd N)
tabstat quant_saved if country==3, stats(mean sd N)

tabstat reached_b4goal, stats(mean sd N)
tabstat reached_b4goal if country==1, stats(mean sd N)
tabstat reached_b4goal if country==2, stats(mean sd N)
tabstat reached_b4goal if country==3, stats(mean sd N)

***Figure 1
graph box quant_saved, over(rem_any) ytitle("Quantity Saved") noout scheme(s2mono)
graph save noout, replace

graph box quant_saved, over(rem_any) ytitle("Quantity Saved") scheme(s2mono)
graph save without, replace

graph combine without.gph noout.gph
graph export figure1.pdf, as(pdf) replace
graph save "${export}/figure 1", replace

***Table 4
*All the values can be found in the logfile under "Table 4"
*Panel A
xi: reg log_quant_saved rem_any `randfeature' i.country, robust
summ log_quant_saved if e(sample) ==1
local y = _result(3)

xi: reg log_quant_saved rem_any `randfeature' `covariates' i.country, robust
summ log_quant_saved if e(sample) ==1
local y = _result(3)

xi: reg reached_b4goal rem_any `randfeature' i.country, robust
summ reached_b4goal if e(sample) ==1
local y = _result(3)

xi: reg reached_b4goal rem_any `covariates' `randfeature' i.depart i.provincia i.marketer i.branch i.country, robust
summ reached_b4goal if e(sample) ==1
local y = _result(3)

* Panel B
reg log_quant_saved rem_any_peru rem_any_boli rem_any_phil `randfeature' peru bolivia, robust
summ log_quant_saved if e(sample) ==1
local y = _result(3)

test rem_any_peru = rem_any_boli
test rem_any_peru = rem_any_phil
test rem_any_bol = rem_any_phil

xi: reg log_quant_saved rem_any_peru rem_any_boli rem_any_phil `covariates' `randfeature' peru bolivia i.depart i.provincia i.marketer i.branch, robust
summ log_quant_saved if e(sample) ==1
local y = _result(3)

test rem_any_peru = rem_any_boli
test rem_any_peru = rem_any_phil
test rem_any_bol = rem_any_phil

reg reached_b4goal rem_any_peru rem_any_boli rem_any_phil `randfeature' peru bolivia, robust
summ reached_b4goal if e(sample) ==1
local y = _result(3)

test rem_any_peru = rem_any_boli
test rem_any_peru = rem_any_phil
test rem_any_bol = rem_any_phil

xi: reg reached_b4goal rem_any_peru rem_any_boli rem_any_phil `covariates' `randfeature' peru bolivia i.depart i.provincia i.marketer i.branch, robust
summ reached_b4goal if e(sample) ==1
local y = _result(3)

test rem_any_peru = rem_any_boli
test rem_any_peru = rem_any_phil
test rem_any_bol = rem_any_phil

*Table 5
*All the values can be found in the logfile under "Table 5"
*Panel A
xi: reg log_quant_saved gain_rem loss_rem `randfeature' i.country, robust
test gain_rem = loss_rem
summ log_quant_saved if e(sample) ==1
local y = _result(3)

xi: reg log_quant_saved gain_rem loss_rem `covariates' `randfeature' i.country i.depart i.provincia i.marketer i.branch, robust
test gain_rem = loss_rem
summ log_quant_saved if e(sample) ==1
local y = _result(3)

xi: reg reached_b4goal gain_rem loss_rem `randfeature' i.country, robust
test gain_rem = loss_rem
summ reached_b4goal if e(sample) ==1
local y = _result(3)

xi: reg reached_b4goal gain_rem loss_rem `covariates' `randfeature' i.country i.depart i.provincia i.marketer i.branch, robust
test gain_rem = loss_rem
summ reached_b4goal if e(sample) ==1
local y = _result(3)

**Panel B
xi: reg log_quant_saved rem_no_motive rem_motive `randfeature' i.country if country==1
est store exp

xi: reg log_quant_saved rem_no_motive rem_motive `randfeature' i.country if country==1, robust
test rem_no_motive = rem_motive
summ log_quant_saved if e(sample) ==1
local y = _result(3)

xi: reg log_quant_saved rem_no_motive rem_motive `covariates' `randfeature' i.country i.depart i.provincia i.marketer i.branch if country==1, robust
test rem_no_motive = rem_motive
summ log_quant_saved if e(sample) ==1
local y = _result(3)

xi: reg reached_b4goal rem_no_motive rem_motive `randfeature' i.country if country==1
est store exp2

xi: reg reached_b4goal rem_no_motive rem_motive `randfeature' i.country if country==1, robust
test rem_no_motive = rem_motive
summ reached_b4goal if e(sample) ==1
local y = _result(3)

xi: reg reached_b4goal rem_no_motive rem_motive  `covariates' `randfeature'  i.country i.depart i.provincia i.marketer i.branch if country==1, robust
test rem_no_motive = rem_motive
summ reached_b4goal if e(sample) ==1
local y = _result(3)

*Panel C
xi: reg log_quant_saved incentive noincentive  `randfeature' if country==2, robust
test incentive = noincentive
summ log_quant_saved if e(sample) ==1
local y = _result(3)

xi: reg log_quant_saved incentive noincentive   `covariates' `randfeature' i.depart i.provincia i.marketer i.branch if country==2, robust
test incentive = noincentive
summ log_quant_saved if e(sample) ==1
local y = _result(3)

xi: reg reached_b4goal incentive noincentive  `randfeature' if country==2, robust
test incentive = noincentive
summ reached_b4goal if e(sample) ==1
local y = _result(3)

xi: reg reached_b4goal incentive noincentive   `covariates' `randfeature' i.depart i.provincia i.marketer i.branch if country==2, robust
test incentive = noincentive
summ reached_b4goal if e(sample) ==1
local y = _result(3)

*Table 6 Timing
*All the values can be found in the logfile under "Table 6 Timing"
*Panel A
xi: reg log_quant_saved rem_any late_rem_any `randfeature' i.country if country!=2, robust
summ log_quant_saved if e(sample) ==1
local y = _result(3)

xi: reg log_quant_saved rem_any late_rem_any `covariates' `randfeature' i.country i.depart i.provincia i.marketer i.branch if country!=2, robust
summ log_quant_saved if e(sample) ==1
local y = _result(3)

xi: reg reached_b4goal rem_any late_rem_any `randfeature' i.country if country!=2, robust
summ reached_b4goal if e(sample) ==1
local y = _result(3)

xi: reg reached_b4goal rem_any late_rem_any `covariates' `randfeature'  i.country i.depart i.provincia i.marketer i.branch if country!=2, robust
summ reached_b4goal if e(sample) ==1
local y = _result(3)

**  Panel B
xi: reg log_quant_saved foto puzzle_ica `randfeature' i.country if country==1
est store foto

xi: reg log_quant_saved foto puzzle_ica `randfeature' i.country if country==1, robust
test foto = puzzle_ica
summ log_quant_saved if e(sample) ==1
local y = _result(3)

suest exp foto
test ([exp_mean]_b[rem_motive] = [foto_mean]_b[foto]) ([exp_mean]_b[rem_motive] = [foto_mean]_b[puzzle_ica])

xi: reg log_quant_saved foto puzzle_ica `covariates' `randfeature' i.country i.depart i.provincia i.marketer i.branch if country==1, robust
test foto = puzzle_ica
summ log_quant_saved if e(sample) ==1
local y = _result(3)

xi: reg reached_b4goal foto puzzle_ica `randfeature' i.country if country==1
est store foto2

xi: reg reached_b4goal foto puzzle_ica `randfeature' i.country if country==1, robust
test foto = puzzle_ica
summ reached_b4goal if e(sample) ==1
local y = _result(3)

suest exp2 foto2
test ([exp2_mean]_b[rem_motive] = [foto2_mean]_b[foto]) ([exp2_mean]_b[rem_motive] = [foto2_mean]_b[puzzle_ica])

xi: reg reached_b4goal foto puzzle_ica `covariates' `randfeature'  i.country i.depart i.provincia i.marketer i.branch if country==1, robust
test foto = puzzle_ica
summ reached_b4goal if e(sample) ==1
local y = _result(3)

*Appendix 1
*The variable "motivo" is coded incorrectly, so that when tabulated: “Percentage of sample” for “Appendix 1” – “Educacion” should read “15.21”, rather than “20.25”//
*“20.25” was the value in the “Cum.” or cumulative column in the output data*
*All the values can be found in the logfile under "Appendix 1"
tab motivo

*Appendix 2
*All the values can be found in the logfile under "Appendix 2"
*Panel A
xi: reg quant_saved rem_any `randfeature' i.country, robust
summ quant_saved if e(sample) ==1
local y = _result(3)

xi: reg quant_saved rem_any `covariates' `randfeature' i.country i.depart i.provincia i.marketer i.branch, robust
summ quant_saved if e(sample) ==1
local y = _result(3)

xi: reg logalt_quant_saved rem_any `randfeature' i.country, robust
summ logalt_quant_saved if e(sample) ==1
local y = _result(3)

xi: reg logalt_quant_saved rem_any `covariates' `randfeature' i.country i.depart i.provincia i.marketer i.branch, robust
summ logalt_quant_saved if e(sample) ==1
local y = _result(3)

xi: reg ivhquant_saved rem_any `randfeature' i.country, robust
summ ivhquant_saved if e(sample) ==1
local y = _result(3)

xi: reg ivhquant_saved rem_any `covariates' `randfeature' i.country i.depart i.provincia i.marketer i.branch, robust
summ ivhquant_saved if e(sample) ==1
local y = _result(3)

*Panel B
reg quant_saved rem_any_peru rem_any_boli rem_any_phil `randfeature' peru bolivia, robust
summ quant_saved if e(sample) ==1
local y = _result(3)

test rem_any_peru = rem_any_boli
test rem_any_peru = rem_any_phil
test rem_any_bol = rem_any_phil

xi: reg quant_saved rem_any_peru rem_any_boli rem_any_phil `covariates' `randfeature' i.country i.depart i.provincia i.marketer i.branch, robust
summ quant_saved if e(sample) ==1
local y = _result(3)

test rem_any_peru = rem_any_boli
test rem_any_peru = rem_any_phil
test rem_any_bol = rem_any_phil

reg logalt_quant_saved rem_any_peru rem_any_boli rem_any_phil `randfeature' peru bolivia, robust
summ logalt_quant_saved if e(sample) ==1
local y = _result(3)

test rem_any_peru = rem_any_boli
test rem_any_peru = rem_any_phil
test rem_any_bol = rem_any_phil

xi: reg logalt_quant_saved rem_any_peru rem_any_boli rem_any_phil `covariates' `randfeature' i.country i.depart i.provincia i.marketer i.branch, robust
summ logalt_quant_saved if e(sample) ==1
local y = _result(3)

test rem_any_peru = rem_any_boli
test rem_any_peru = rem_any_phil
test rem_any_bol = rem_any_phil

reg ivhquant_saved rem_any_peru rem_any_boli rem_any_phil `randfeature' peru bolivia, robust
summ ivhquant_saved if e(sample) ==1
local y = _result(3)

test rem_any_peru = rem_any_boli
test rem_any_peru = rem_any_phil
test rem_any_bol = rem_any_phil

xi: reg ivhquant_saved rem_any_peru rem_any_boli rem_any_phil `covariates' `randfeature' i.country i.depart i.provincia i.marketer i.branch, robust
summ ivhquant_saved if e(sample) ==1
local y = _result(3)

test rem_any_peru = rem_any_boli
test rem_any_peru = rem_any_phil
test rem_any_bol = rem_any_phil

log close

