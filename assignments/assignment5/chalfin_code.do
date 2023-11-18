local comp C:/Users/achalfin/Dropbox\aerpp
clear
set more off
use "`comp'/master5_9_14.dta", clear

capture log close
log using "`comp'/chalfin_immigr.log", replace

// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
// Set Locals
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
egen st = group(STATE) // state fixed effects
	quietly tab st, gen(st_)
local se cluster(FMSA) // Standard errors
local w popweight // analytic weight
local covs deduc* dblack dage* demployed dusbirths  // covariates
local endog dmexfb_alt
local groups dfbnonmex dushisp dmexfbk // groups
local crimes murder rape robbery assault burglary larceny motor //crimes
local other dfbnonmex dushisp

// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
// First Stage
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
// Main Results
foreach i of varlist `endog'  {

reg `i' dins i.year [aweight=`w'], `se'
	test dins
	
reg `i' dins `covs' `other' [aweight=`w'], `se'
	test dins
	
areg `i' dins `covs' `other' [aweight=`w'], absorb(grp) `se'
	test dins
	
areg `i' dins `covs' `other' [aweight=`w'] if FMSA!=1600, absorb(grp) `se'
	test dins
	
areg `i' dins `covs' `other' [aweight=`w'] if FMSA!=4480, absorb(grp) `se'
	test dins
	
}
	

// Falsification tests
foreach i of varlist `groups'  {
	
areg `i' dins `covs'  [aweight=`w'], absorb(grp) `se'
	test dins
	
}


// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
// Least Squares
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
foreach i of varlist `crimes' {
	areg dlogpc_`i' `endog' `covs' `other' ///
	[aweight=`w'], absorb(grp) `se'
}

// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
// 2SLS
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
foreach i of varlist `crimes' {
	ivreg2 dlogpc_`i' (`endog' = dins) `covs' `other' grp_* ///
	[aweight=`w'], `se'
}log close



// END
