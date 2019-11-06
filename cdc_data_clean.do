global root_path "~/Documents/GitHub/evali_and_recreational_marijuana"


import excel "$root_path/cdc_data.xlsx", sheet("Sheet1") clear
gen number = _n
gen odd = mod(number,2)
gen id = _n
gen id2 = _n - 1
replace id = id2 if odd == 0 
tostring odd, replace
drop id2 number
reshape wide A, i(id) j(odd) string 
replace A0 = subinstr(A0, " cases", "", .)
split A0, p("-")
destring A01, replace
destring A02, replace

rename A1 state

gen state_name=strupper(state)
gen state_code=""

replace state_code ="AK" if state_name=="ALASKA"
replace state_code ="AL" if state_name=="ALABAMA"
replace state_code ="AR" if state_name=="ARKANSAS"
replace state_code ="AS" if state_name=="AMERICAN SAMOA"
replace state_code ="AZ" if state_name=="ARIZONA"
replace state_code ="CA" if state_name=="CALIFORNIA"
replace state_code ="CO" if state_name=="COLORADO"
replace state_code ="CT" if state_name=="CONNECTICUT"
replace state_code ="DC" if state_name=="DISTRICT OF COLUMBIA"
replace state_code ="DC" if state_name=="D.C."
replace state_code ="DC" if state_name=="WASHINGTON D.C."
replace state_code ="DE" if state_name=="DELAWARE"
replace state_code ="FL" if state_name=="FLORIDA"
replace state_code ="GA" if state_name=="GEORGIA"
replace state_code ="GU" if state_name=="GUAM"
replace state_code ="HI" if state_name=="HAWAII"
replace state_code ="IA" if state_name=="IOWA"
replace state_code ="ID" if state_name=="IDAHO"
replace state_code ="IL" if state_name=="ILLINOIS"
replace state_code ="IN" if state_name=="INDIANA"
replace state_code ="KS" if state_name=="KANSAS"
replace state_code ="KY" if state_name=="KENTUCKY"
replace state_code ="LA" if state_name=="LOUISIANA"
replace state_code ="MA" if state_name=="MASSACHUSETTS"
replace state_code ="MD" if state_name=="MARYLAND"
replace state_code ="ME" if state_name=="MAINE"
replace state_code ="MI" if state_name=="MICHIGAN"
replace state_code ="MN" if state_name=="MINNESOTA"
replace state_code ="MO" if state_name=="MISSOURI"
replace state_code ="MS" if state_name=="MISSISSIPPI"
replace state_code ="MT" if state_name=="MONTANA"
replace state_code ="NC" if state_name=="NORTH CAROLINA"
replace state_code ="ND" if state_name=="NORTH DAKOTA"
replace state_code ="NE" if state_name=="NEBRASKA"
replace state_code ="NH" if state_name=="NEW HAMPSHIRE"
replace state_code ="NJ" if state_name=="NEW JERSEY"
replace state_code ="NM" if state_name=="NEW MEXICO"
replace state_code ="NV" if state_name=="NEVADA"
replace state_code ="NY" if state_name=="NEW YORK"
replace state_code ="OH" if state_name=="OHIO"
replace state_code ="OK" if state_name=="OKLAHOMA"
replace state_code ="OR" if state_name=="OREGON"
replace state_code ="PA" if state_name=="PENNSYLVANIA"
replace state_code ="PR" if state_name=="PUERTO RICO"
replace state_code ="RI" if state_name=="RHODE ISLAND"
replace state_code ="SC" if state_name=="SOUTH CAROLINA"
replace state_code ="SD" if state_name=="SOUTH DAKOTA"
replace state_code ="TN" if state_name=="TENNESSEE"
replace state_code ="TX" if state_name=="TEXAS"
replace state_code ="UT" if state_name=="UTAH"
replace state_code ="VA" if state_name=="VIRGINIA"
replace state_code ="VI" if state_name=="VIRGIN ISLANDS"
replace state_code ="VT" if state_name=="VERMONT"
replace state_code ="WA" if state_name=="WASHINGTON"
replace state_code ="WI" if state_name=="WISCONSIN"
replace state_code ="WV" if state_name=="WEST VIRGINIA"
replace state_code ="WY" if state_name=="WYOMING"

drop state
rename state_code state

gen StateFIPS=.					
replace StateFIPS=	15	if 	state=="HI"
replace StateFIPS=	1	if 	state=="AL"
replace StateFIPS=	2	if 	state=="AK"
replace StateFIPS=	4	if 	state=="AZ"
replace StateFIPS=	5	if 	state=="AR"
replace StateFIPS=	6	if 	state=="CA"
replace StateFIPS=	8	if 	state=="CO"
replace StateFIPS=	9	if 	state=="CT"
replace StateFIPS=	10	if 	state=="DE"
replace StateFIPS=	11	if 	state=="DC"
replace StateFIPS=	12	if 	state=="FL"
replace StateFIPS=	13	if 	state=="GA"
replace StateFIPS=	16	if 	state=="ID"
replace StateFIPS=	17	if 	state=="IL"
replace StateFIPS=	18	if 	state=="IN"
replace StateFIPS=	19	if 	state=="IA"
replace StateFIPS=	20	if 	state=="KS"
replace StateFIPS=	21	if 	state=="KY"
replace StateFIPS=	22	if 	state=="LA"
replace StateFIPS=	23	if 	state=="ME"
replace StateFIPS=	24	if 	state=="MD"
replace StateFIPS=	25	if 	state=="MA"
replace StateFIPS=	26	if 	state=="MI"
replace StateFIPS=	27	if 	state=="MN"
replace StateFIPS=	28	if 	state=="MS"
replace StateFIPS=	29	if 	state=="MO"
replace StateFIPS=	30	if 	state=="MT"
replace StateFIPS=	31	if 	state=="NE"
replace StateFIPS=	32	if 	state=="NV"
replace StateFIPS=	33	if 	state=="NH"
replace StateFIPS=	34	if 	state=="NJ"
replace StateFIPS=	35	if 	state=="NM"
replace StateFIPS=	36	if 	state=="NY"
replace StateFIPS=	37	if 	state=="NC"
replace StateFIPS=	38	if 	state=="ND"
replace StateFIPS=	39	if 	state=="OH"
replace StateFIPS=	40	if 	state=="OK"
replace StateFIPS=	41	if 	state=="OR"
replace StateFIPS=	42	if 	state=="PA"
replace StateFIPS=	44	if 	state=="RI"
replace StateFIPS=	45	if 	state=="SC"
replace StateFIPS=	46	if 	state=="SD"
replace StateFIPS=	47	if 	state=="TN"
replace StateFIPS=	48	if 	state=="TX"
replace StateFIPS=	49	if 	state=="UT"
replace StateFIPS=	50	if 	state=="VT"
replace StateFIPS=	51	if 	state=="VA"
replace StateFIPS=	53	if 	state=="WA"
replace StateFIPS=	54	if 	state=="WV"
replace StateFIPS=	55	if 	state=="WI"
replace StateFIPS=	56	if 	state=="WY"

keep StateFIPS state A01 A02
rename A01 lower_bound
rename A02 upper_bound

drop if missing(StateFIPS)
compress

save "~/Desktop/temp.dta", replace
use "/Users/hollinal/Documents/GitHub/marijuana_legalization_and_use/data_for_analysis/medical_marijuana_status.dta", clear
keep if year == "2018-2019"
rename st_abbrv state
drop year

merge 1:1 state using "~/Desktop/temp.dta"
drop _merge
merge 1:m StateFIPS using  "/Users/hollinal/Box/Arrests/data_for_analysis/population_data.dta"

keep if year == 2017
replace upper_bound = 0 if missing(upper_bound)
gen mid_point = (upper_bound+lower_bound)/2

gen cases_per_million = (mid_point/pop_total)*1000000

compress
drop _merge
save "/Users/hollinal/Desktop/lung_damage/data_for_R.dta", replace

 import excel "/Users/hollina/Documents/GitHub/evali_and_recreational_marijuana/ecigarette_use_2017.xlsx", sheet("Sheet1") firstrow clear
 merge 1:1 state using  "$root_path/data_for_R.dta", 
keep if _merge == 3
compress 
save "$root_path/data_for_R.dta", replace

/*
sumup cases_per_100k [aw = pop_total], by(rm_disp)
sumup cases_per_100k [aw = pop_total], by(rm)
sumup cases_per_100k [aw = pop_total], by(disp)
sumup cases_per_100k [aw = pop_total], by(mm)

sumup cases_per_100k [aw = pop_total]  if mm==1, by(rm_disp)

reg cases_per_100k i.rm_disp i.disp [aw = pop_total], robust
reg cases_per_100k i.rm_disp i.disp [aw = pop_total] if mm==1, robust


sumup cases_per_100k  , by(rm_disp)
sumup cases_per_100k  , by(rm)
sumup cases_per_100k  , by(disp)
sumup cases_per_100k  , by(mm)
*/


//


/////////////////////////////////////////////////////////////////////////////////
// Plot the case rate by state


// Create correct averages for US w/o mj states and just mj states
set obs `=_N+1'
sum cases_per_million  if rm == 0 & mm == 0
local us_non_recreational = `r(mean)'
/*
replace cases_per_million = `us_non_recreational' in 52
replace state = "Avg. non-marijuana state" in 52
replace rm = 0 in 52
replace mm = 0 in 52
*/
set obs `=_N+1'
sum cases_per_million   if rm_disp == 1 
local us_recreational = `r(mean)'
/*
replace cases_per_million = `us_recreational' in 53
replace state = "Avg. recreational state" in 53
replace rm_disp=1 in 53
*/

set obs `=_N+1'
sum cases_per_million   if rm == 0 & mm == 1
local us_med = `r(mean)'
/*
replace cases_per_million = `us_med' in 54
replace state = "Avg. medical state" in 54
replace mm=1 in 54
replace rm_disp=0 in 54
*/
// Sort by the difference
gsort -cases_per_million

// Create a rank based upon this difference
capture drop rank
capture drop fake_diff
gen rank = _n

// add a space so it's clear
*replace rank = rank + 1 if recreational_marijuana==0
replace rank = 2*rank
gen fake_diff=0

gen fake_case = -1
*graph dot cases_per_million, over(state) sort(rank)
twoway ///
	|| scatter   rank fake_case if rm==0 & mm == 0 , mlabel(state) mlabpos(0) mlabsize(1.75) msymbol(none) mcolor(black) ///
	|| scatter   rank fake_case if rm_disp ==0 & mm == 1 , mlabel(state) mlabpos(0) mlabsize(1.75) msymbol(none)  mcolor(turquoise) ///
	|| scatter   rank  fake_case if rm_disp==1, mlabel(state) mlabpos(0) mlabsize(1.75) msymbol(none) mcolor(turquoise) ///
	|| scatter   rank cases_per_million if rm==0 & mm == 0 ,   mlabpos(2) mlabsize(2) msymbol(Oh) mcolor(gs7) ///
	|| scatter   rank cases_per_million if rm_disp ==0 & mm == 1 ,   mlabpos(2) mlabsize(2) msymbol(Oh)  mcolor(turquoise) ///
	|| scatter   rank  cases_per_million if rm_disp==1,  mlabpos(2) mlabsize(2) msymbol(D) mcolor(turquoise) ///
	ylabel(, nolabels tlength(0) notick nogrid)		ytitle("")	 ///
	legend(pos(6) cols(3) order(6 5 4) label(4 "Non-marijuana state") label(5 "Medical marijuana state") label(6 "Recreational marijuana state") size(3)) ///
	ysc(off) ///
	xtitle("Lung disease cases per one million population", size(3)) ///
	xlabel(,  tlength(0) nogrid notick labsize(3))	///
	xline(`us_non_recreational') xline(`us_recreational') xline(`us_med') ///
	ysize(5)  ///
	yscale(range(2 102)) ///
	text(18.5 1.8 "Average", place(e) si(2)) ///
	text(16 1.75 "Recreational",  place(e) si(2)) ///
	text(13.5 1.75  "Marijuana State", place(e) si(2)) ///
	text(90 9.75 "Average", place(e) si(2)) ///
	text(87.5 9.75 "Medical",  place(e) si(2)) ///
	text(85 9.6  "Marijuana State", place(e) si(2)) ///
	text(102.5 5.25 "Average", place(e) si(2)) ///
	text(100 3.5 "non-marijuana",  place(e) si(2)) ///
	text(97.5 6.25  "State", place(e) si(2)) 

graph export "/Users/hollinal/Desktop/lung_damage/dot_plot.png", replace
	exit
	
twoway ///
	scatter   rank cases_per_million if rm==0 & mm == 0 , mlabel(state) mlabpos(2) mlabsize(2) msymbol(Oh) ///
	|| scatter   rank cases_per_million if rm_disp ==0 & mm == 1 , mlabel(state) mlabpos(2) mlabsize(2) msymbol(Oh)  mcolor(turquoise) ///
	|| scatter   rank  cases_per_million if rm_disp==1, mlabel(state) mlabpos(2) mlabsize(2) msymbol(D) mcolor(turquoise) ///
	ylabel(, nolabels tlength(0) notick nogrid)		ytitle("")	 ///
	legend(pos(6) cols(3) order(3 2 1) label(1 "Non-marijuana state") label(2 "Medical marijuana state") label(3 "Recreational marijuana state") size(2.25)) ///
	ysc(off) ///
	xtitle("Lung disease cases per one million population", size(2.5)) ///
	xlabel(,  tlength(0) notick labsize(2.25))	///
	xline(`us_non_recreational') xline(`us_recreational') xline(`us_med') ///
	ysize(5) 



exit
sumup cases_per_100k  if mm==1 , by(rm_disp)
	
		twoway (histogram cases_per_100k if rm ==1 , color(sea) bin(5)) ///
			   (histogram cases_per_100k if rm ==0, fcolor(none) color(vermillion) bin(10)), ///
			 legend(off)	///
			 xlabel(0(1)4 ,nogrid notick) ///
			 ylabel(0(.5)2 ,nogrid notick) ///
			 title("Histogram of vaping related lung disease by recreational marijuana status", pos(11) size(4))  ///
			 ytitle("Density") ///
			 xtitle("Cases per 100,000 population")
			 
graph bar cases_per_100k, by(rm)

gen policy = . 
replace policy = 0 if mm == 0 & rm ==0 
replace policy = 1 if rm_disp ==1 
graph bar cases_per_100k, by(policy)

graph bar cases_per_100k if mm == 1, by(rm)


gen no_mm_rm = 0
replace no_mm_rm = 1 if mm == 1 & rm == 0 
replace no_mm_rm = 2 if mm == 1 & rm == 1

graph bar cases_per_100k , by(no_mm_rm)

