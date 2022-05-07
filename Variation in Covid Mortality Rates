*####################################################################################
*############### Project 2: variation in COVID-19 deaths across the US ##############
/*
DATA:

-	New York Times COVID-19 Data: https://github.com/nytimes/covid-19-data
-	County Health Ranking: https://www.countyhealthrankings.org/explore-health-rankings/rankings-data-documentation

*/
*####################################################################################
import delimited "C:\Users\rocho\OneDrive\Documents\R\Health\Project2\us-counties-2022.csv"

keep if date=="2022-04-11"
drop if fips>=.

drop if state=="District of Columbia" | state=="Guam" | state=="American Samoa" | state=="Northern Mariana Island" | state=="Puerto Rico" | state=="Virgin Islands"

sort fips


save "C:\Users\rocho\OneDrive\Documents\R\Health\Project2\us-counties-2022.dta", replace
clear
*####################################################################################
*####################################################################################
import delimited "C:\Users\rocho\OneDrive\Documents\R\Health\Project2\analytic_data2020_0.csv"

set more on
drop if countyfipscode==0

rename v631 age65p
rename digitfipscode fips


sort fips
save "C:\Users\rocho\OneDrive\Documents\R\Health\Project2\analytic_data2020_0.dta", replace
clear

*##################################################################################
*########################## MERGE #################################################

use "C:\Users\rocho\OneDrive\Documents\R\Health\Project2\analytic_data2020_0.dta"
sort fips

merge 1:1 fips using "C:\Users\rocho\OneDrive\Documents\R\Health\Project2\us-counties-2022.dta"
tab _m
drop _m

capture drop _m
sort fips
merge 1:1 fips using "C:\Users\rocho\OneDrive\Documents\R\Health\Project2\mr_all.dta"
tab _m
drop _m

merge 1:1 fips using "C:\Users\rocho\OneDrive\Documents\R\Health\Project2\party.dta"
tab _m
drop _m

save "C:\Users\rocho\OneDrive\Documents\R\Health\Project2\covid_all.dta", replace

*###################################################################################
*######################## Table 1: Summary Statistics ##############################

*Covid-19 cases
xtile cases_gr=cases, nq(3)
sum cases if cases_gr==1
sum cases if cases_gr==2
sum cases if cases_gr==3

*Covid-1-9 mortality rate
g covidmr100k=(deaths/populationrawvalue)*100000
xtile covid_gr=covidmr100k, nq(3)
sum covidmr100k if covid_gr==1
sum covidmr100k if covid_gr==2
sum covidmr100k if covid_gr==3

*% 65 and older
xtile age65_gr=age65p, nq(3)
sum age65p if age65_gr==1
sum age65p if age65_gr==2
sum age65p if age65_gr==3

*% Hispanic
g hisp=hispanicrawvalue
xtile hisp_gr=hispanicrawvalue, nq(3)
sum hisp if hisp_gr==1
sum hisp if hisp_gr==2
sum hisp if hisp_gr==3

*% Black
g black=nonhispanicblackrawvalue
xtile black_gr=nonhispanicblackrawvalue, nq(3)
sum black if black_gr==1
sum black if black_gr==2
sum black if black_gr==3

*Meadin Household Income
g medhhincome=medianhouseholdincomerawvalue
xtile medhhincome_gr=medianhouseholdincomerawvalue, nq(3)
sum medhhincome if medhhincome_gr==1
sum medhhincome if medhhincome_gr==2
sum medhhincome if medhhincome_gr==3

*Population
xtile population_gr=populationrawvalue, nq(3)
sum populationrawvalue if population_gr==1
sum populationrawvalue if population_gr==2
sum populationrawvalue if population_gr==3

*Uninsured = Percentage of population under age 65 without health insurance
xtile uninsured_gr=uninsuredrawvalue, nq(3)
sum uninsuredrawvalue if uninsured_gr==1
sum uninsuredrawvalue if uninsured_gr==2
sum uninsuredrawvalue if uninsured_gr==3

*Poverty
xtile poverty_gr=povertyrawvalue, nq(3)
sum povertyrawvalue if poverty_gr==1
sum povertyrawvalue if poverty_gr==2
sum povertyrawvalue if poverty_gr==3

*Unemployment
xtile unemployment_gr=unemploymentrawvalue, nq(3)
sum unemploymentrawvalue if unemployment_gr==1
sum unemploymentrawvalue if unemployment_gr==2
sum unemploymentrawvalue if unemployment_gr==3

*Diabetes
xtile diabetes_gr=diabetesprevalencerawvalue, nq(3)
sum diabetesprevalencerawvalue if diabetes_gr==1
sum diabetesprevalencerawvalue if diabetes_gr==2
sum diabetesprevalencerawvalue if diabetes_gr==3

*Physical Inactivity = Percentage of adults age 18 and over reporting no leisure-time physical activity (age-adjusted)
xtile physinact_gr=physicalinactivityrawvalue, nq(3)
sum physicalinactivityrawvalue if physinact_gr==1
sum physicalinactivityrawvalue if physinact_gr==2
sum physicalinactivityrawvalue if physinact_gr==3

*Obesity = Percentage of the adult population (age 18 and older) that reports a body mass index (BMI) greater than or equal to 30 kg/m2 (age-adjusted).
xtile obesity_gr=adultobesityrawvalue, nq(3)
sum adultobesityrawvalue if obesity_gr==1
sum adultobesityrawvalue if obesity_gr==2
sum adultobesityrawvalue if obesity_gr==3

*Physician = Ratio of population to primary care physicians

*Fluvac = % of fee-for-service (FFS) Medicare enrollees that had an annual flu shot

*Food Enviornment = Index of factors that contribute to a healthy food environment, from 0 (worst) to 10

*Limited Accessto Healthy Food = Percentage of population who are low-income and do not live close to a grocery store

*Uninsured = Percentage of population under age 65 without health insurance


*##################################################################################
*######### Table 2: Explaining Variation in ln(covid-19 mortality rate) ###########

*rename variables
rename prematureageadjustedmortalityraw premadjMR
rename populationrawvalue population
rename medianhouseholdincomerawvalue inc
rename uninsuredrawvalue unins
rename nonhispanicblackrawvalue black
rename hispanicrawvalue hisp
rename highschoolgraduationrawvalue hs
rename somecollegerawvalue somecol
rename povertyrawvalue poverty
rename adultsmokingrawvalue smoke
rename foodenvironmentindexrawvalue foodenvironment
rename primarycarephysiciansrawvalue physicians
rename ratioofpopulationtoprimarycareph pop_doc_ratio
rename fluvaccinationsrawvalue fluvac
rename unemploymentrawvalue unemp
rename violentcrimerawvalue crime
rename airpollutionparticulatematterraw pollution
rename percentageofhouseholdswithovercr hhovercrowded_p
rename diabetesprevalencerawvalue diabetes
rename limitedaccesstohealthyfoodsrawva limitedhealthyfood
rename adultobesityrawvalue obesity
rename below18yearsofagerawvalue young
rename physicalinactivityrawvalue physinact

*generate variables
g fem_p=femalesrawvalue*100

*log variables
g ln_cancer=log(adjmrcancer)
g ln_heart=log(adjmrheart)
g ln_external=log(adjmrexternal)

g ln_cmr=log(covidmr100k+1)
g ln_premd=log(premadjMR)
g ln_pop=log(population)
g ln_inc=log(inc)

* state code
g state_c=int(fips/1000)

*###################################################################################
*############################### Regressions #######################################


*###################################################################################
*########################## Covid Mortality Rate ###################################
*Model 1
reg covidmr100k population inc age65p black hisp somecol unins femalesrawvalue [w=population]

*Model 2
reg covidmr100k population inc age65p black hisp somecol unins fem_p obesity smoke fluvac premadjMR i.statefips [w=population]

*Model 3
reg covidmr100k population inc age65p black hisp somecol unins fem_p obesity smoke fluvac premadjMR crime pollution i.statefips [w=population]

*###################################################################################
*########################## Ln(Covid Mortality Rate) ###############################

*Model 4
reg ln_cmr ln_pop ln_inc age65p black hisp somecol unins fem_p i.statefips [w=population]
*Model 5
reg ln_cmr ln_pop ln_inc age65p black hisp somecol unins fem_p premadjMR young hhovercrowded_p fluvac smoke physinact i.statefipscode [w=population]
*Model 6
reg ln_cmr ln_pop ln_inc age65p black hisp somecol unins fem_p premadjMR young hhovercrowded_p fluvac smoke physinact crime pollution unemp rep_share i.statefipscode [w=population]


*###################################################################################
*########################## Other Health Outcomes ##################################
*Model 7
reg ln_cancer ln_pop ln_inc age65p black hisp somecol unins fem_p premadjMR young hhovercrowded_p fluvac smoke physinact crime pollution unemp rep_share i.statefipscode [w=population]

*Model 8
reg ln_heart ln_pop ln_inc age65p black hisp somecol unins fem_p premadjMR young hhovercrowded_p fluvac smoke physinact crime pollution unemp rep_share i.statefipscode [w=population]

*Model 9
reg ln_external ln_pop ln_inc age65p black hisp somecol unins fem_p premadjMR young hhovercrowded_p fluvac smoke physinact crime pollution unemp rep_share i.statefipscode [w=population]

*###################################################################################
*############################### Final Models ######################################
*Super Duper Model 10
reg ln_cmr ln_pop ln_inc age65p black hisp hs unins fem_p premadjMR young hhovercrowded_p physinact physicians crime foodenvironment limitedhealthyfood rep_share i.statefipscode [w=population]

*Super Duper Model Infinite
reg ln_cmr ln_pop ln_inc age65p black hisp hs fem_p premadjMR young hhovercrowded_p physinact physicians crime rep_share i.statefipscode [w=population]





