clear all
capture log cl
set maxvar 15000

cap log close
log using Project1.log, replace

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*** Identifiers and Demographic variables:
*person specific identifier (hhipdn), sample cohort (racohbyr), wave status (inw*), race (raracem), hispanic (rahispan), year-age at interview (r*agey_b), gender (ragender), years of edu (raedyrs), religion (rarelig), verteran status (ravetrn), marital status (remstat), death month (radmonth), birth month (rabmonth), death year (radyear), birth year (rabyear)


*** Health variables:
*self-report of health (r*shlt), hospital care util (r*hosp), doctor util (r*doctor), doctor util (r*doctim) self report bmi (r*bmi), med expenditures (r*oopmd), phys exercise (r*vigact), phys activity fine (r*vgactx), drinker (r*drink), smoker (r*smokev), preventive behaviors (r*cholst r*flusht r*breast r*mammog r*papsm r*prost), diabetes (r*diab), cancer (r*cancr), heart disease (r*heart), # conditions (r*conds)


*** Financial/Wealth variables:
*check/savings value (h*achck)


*** Income variables:
* individual earnings (r*iearn), household total income (h*itot), household capital income (h*icap), pension income (r*ipena)


*** Social Security variables:
*receive soc sec (rassrecv)


***Health Insurance variable:
* covered by gov HI program (r*higov), HI covered under prev employer (r*covr), # of private HI plans (r*prpcnt)


*** Family Structure variables:
* # of ppl in household (h*hhres)


*** Retirement variables:
* retired (r*sayret), prob living to 75 (r*liv75), prob living to 85 (r*liv85), prob working after 65 (r*work65), prob working after 70 (r*work70)


*** Employment variables:
* currently working (r*work)
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
use hhidpn racohbyr inw* raracem rahispan r*agey_b ragender raedyrs rarelig ravetrn remstat r*mstat radmonth rabmonth radyear rabyear r*shlt r*hosp r*doctor r*doctim r*bmi r*oopmd r*vigact r*vgactx r*drink r*smokev r*cholst r*flusht r*breast r*mammog r*papsm r*prost r*diab r*conds r*cancr r*heart h*achck h*itot r*iearn h*icap r*ipena rassrecv r*higov r*covr r*prpcnt h*hhres r*sayret r*liv75 r*liv85 r*work65 r*work70 r*work using "C:\Users\rocho\OneDrive\Documents\R\Health\randhrs1992_2018v1.dta"

reshape long inw@ r@agey_b r@mstat r@shlt r@hosp r@doctor r@doctim r@bmi r@oopmd r@vigact r@vgactx r@drink r@smokev r@cholst r@flusht r@breast r@mammog r@papsm r@prost r@diab r@conds r@cancr r@heart h@achck r@iearn h@itot h@icap r@ipena r@higov r@covr r@prpcnt h@hhres r@sayret r@liv75 r@liv85 r@work65 r@work70 r@work, i(hhidpn) j(wave)

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
set more on

tab wave

* keep if present in that wave
keep if inw==1

tab wave

* keep those >=67 years of age
keep if ragey_b>=67 & ragey_b<=75

tab wave

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
****Table 1 averages


*g age_d=radyear-rabyear
*g dead5=(age_d-age)


*self reported health
***use i.rshlt, shows the categories
g hstatus=rshlt

*%obese and overweight (fat)
g fat=1 if rbmi>=24.5
recode fat .=0 if rbmi<24.5

*diabetes
g diab=rdiab

*cancer
g cancer=rcancr

*heart disease
g heartdis=rheart

*# of chronic health conditions
g numconditions=rconds

*dead within 5 years
g age_d=(radyear-rabyear)*(radmonth>=rabmonth)+(radyear-rabyear-1)*(radmonth<rabmonth)
g dead5years=(age_d-age)<=5

*# of dr visits
g numdocvisits=rdoctim

*hospitalized in last 2 years
g hosp2years=rhosp

*flu shot
g flushot=1 if rflusht==1
recode flushot .=0 if rflusht==0

*out-of-pocket health care spending
g hcspending=roopmd

*age
g age=ragey_b

*female
g female=ragender==2

*married
*g married=1 if remstat>=1 & remstat<=3
*recode married .=0 if remstat>=4 & remstat<=8

*non-married
g nonmarried=1 if rmstat>=5
recode nonmarried .=0 if rmstat<5

*years of education
g edu=raedyrs

*non-white
g nonwhite=1 if raracem==2 |  raracem==3
recode nonwhite .=0 if raracem==1

*hispanic
g hispanic=1 if rahispan==1
recode hispanic .=0 if rahispan==0

*income
g householdinc=hitot

*smoking
g smoker=1 if rsmokev==1
recode smoke .=0 if rsmokev==0

*physically active
g physactive=1 if rvigact==1 | rvgactx<=2
recode physactive .=0 if rvigact==0 | rvgactx>2

*drinking
g drinker=1 if rdrink==1
recode drinker .=0 if rdrink==0

*receive social security
g recsocialsec=rassrecv

*retired
g retired=1 if rsayret==1
recode retired .=0 if rsayret==0 | rsayret>1

*checking/savings account
g savings=hachck

*religion
g eligion=rarelig
recode eligion* (1 = 1 protestant) (2 = 2 catholic) (3 = 3 jewish) (4 = 4 none) (5 = 5 other), pre(r)

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
***Summary Statistics
sum hstatus fat diab cancer heartdis numconditions dead5years numdocvisits hosp2years flushot hcspending age female nonmarried edu nonwhite hispanic householdinc smoker physactive drinker recsocialsec retired savings if racohbyr==1

sum hstatus fat diab cancer heartdis numconditions dead5years numdocvisits hosp2years flushot hcspending age female nonmarried edu nonwhite hispanic householdinc smoker physactive drinker recsocialsec retired savings if racohbyr==2

sum hstatus fat diab cancer heartdis numconditions dead5years numdocvisits hosp2years flushot hcspending age female nonmarried edu nonwhite hispanic householdinc smoker physactive drinker recsocialsec retired savings if racohbyr==3

sum hstatus fat diab cancer heartdis numconditions dead5years numdocvisits hosp2years flushot hcspending age female nonmarried edu nonwhite hispanic householdinc smoker physactive drinker recsocialsec retired savings if racohbyr==4

sum hstatus fat diab cancer heartdis numconditions dead5years numdocvisits hosp2years flushot hcspending age female nonmarried edu nonwhite hispanic householdinc smoker physactive drinker recsocialsec retired savings if racohbyr==5

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
***OLS Table 2

*g ohort=racohbyr
*recode ohort* (1 = 1 ahead) (2 = 2 coda) (3 = 3 hrs) (4 = 4 warbabies) (5 = 5 earlybabyboomers), pre(c)

g ohort=1 if racohby==3
replace ohort=2 if racohby==1
replace ohort=3 if racohby==2
replace ohort=4 if racohby==4
replace ohort=5 if racohby==5
recode ohort* (1 = 1 hrs) (2 = 2 ahead) (3 = 3 coda) (4 = 4 warbabies) (5 = 5 earlybabyboomers), pre(c)

g college=1 if raedyrs>=16 & raedyrs<.
recode college .=0 if raedyrs<16

g ln_householdinc=log(householdinc+1)

g ln_hcspending=log(hcspending+1)

g age7175=1 if age>=71
recode age7175 .=0 if age<71

g mi=1 if rbmi>=18 & rbmi<=24
replace mi=2 if rbmi<18
replace mi=3 if rbmi>=25 & rbmi<=29
replace mi=4 if rbmi>=30
recode mi* (1 = 1 normal) (2 = 2 underweight) (3 = 3 overweight) (4 = 4 obese), pre(b)

****************************************************************
***bmi
*g underweight=1 if rbmi<=18.5
*recode underweight .=0 if rbmi>=18.5 & rbmi<.

*g normal=1 if rbmi>=18.5 & rbmi<=24.5 
*recode normal .=0 if bmi>=18.5 | (rbmi<. & rbmi>24.9)

*g overweight=1 if rbmi>24.5 & rbmi<=29.9
*recode overweight .=0 if rbmi<=24.9 | (rbmi<. & rbmi>29.9)

*g obese=1 if rbmi>=30
*recode obese .=0 if rbmi<30
**************************************************************
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
***Regression Models
reg hstatus i.cohort female nonwhite hispanic college nonmarried age7175 ib1.bmi numconditions ln_householdinc smoker drinker physactive retired recsocialsec ib4.religion, robust

reg dead5years i.cohort female nonwhite hispanic college nonmarried age7175 ib1.bmi numconditions ln_householdinc smoker drinker physactive retired recsocialsec ib4.religion, robust

reg hosp2years i.cohort female nonwhite hispanic college nonmarried age7175 ib1.bmi numconditions ln_householdinc smoker drinker physactive retired recsocialsec ib4.religion, robust

reg flushot i.cohort female nonwhite hispanic college nonmarried age7175 ib1.bmi numconditions ln_householdinc smoker drinker physactive retired recsocialsec ib4.religion, robust

reg ln_hcspending i.cohort female nonwhite hispanic college nonmarried age7175 ib1.bmi numconditions ln_householdinc smoker drinker physactive retired recsocialsec ib4.religion, robust




log close
