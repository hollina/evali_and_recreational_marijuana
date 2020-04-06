# Replication package for "Association of state marijuana legalization policies for medical and recreational use and vaping associated lung disease"

This repository contains code and data to replicate the results of:

Wing, Coady. Ashley Bradford, Aaron Carroll, and Alex Hollingsworth. (2020) "Association of state marijuana legalization policies for medical and recreational use and vaping associated lung disease." JAMA: Network Open. 3(4): e202187. <https://doi.org/10.1001/jamanetworkopen.2020.2187>


<figure style="float:center;">
<img src="https://github.com/hollina/evali_and_recreational_marijuana/blob/master/output/main_exhibit_with_bars.png"  width="300"  /> 
</figure>



**Abstract**: In the last decade, health care reform has dominated U.S. public policy and political discourse. Double-digit rate increases in premiums in the Health Insurance Marketplaces established by the Affordable Care Act (ACA) in 2018 make this an ongoing issue that could affect future elections. A seminal event that changed the course of policy and politics around health care reform is the 2016 presidential election. The results of the 2016 presidential election departed considerably from polling forecasts. Given the prominence of the Affordable Care Act in the election, we test whether changes in health insurance coverage at the county-level correlate with changes in party vote share in the presidential elections from 2008 through 2016. We find that a one-percentage-point increase in county health insurance coverage was associated with a 0.25-percentage-point increase in the vote share for the Democratic presidential candidate. We further find that these gains on the part of the Democratic candidate came almost fully at the expense of the Republican (as opposed to third-party) presidential candidates. We also estimate models separately for states that did and did not expand Medicaid and find no differential effect of insurance gains on Democratic vote share for states that expanded Medicaid compared to those that did not. Our results are consistent with the hypothesis that outcomes in health insurance markets played a role in the outcome of the 2016 presidential election. The decisions made by the current administration, and how those decisions affect health insurance coverage and costs, may be important factors in future elections as well.


## Data Sources:

We obtained data on **EVALI cases** in 2019 at the state-level from the Centers for Disease Control and Prevention (CDC). Available here: <http://dx.doi.org/10.15585/mmwr.mm6839e1>

We obtained data on **prevalence of current e-cigarette users** from the Behavioral Risk Factor Surveillance System (BRFSS), the most up-to-date data are from 2017. This is available here: <https://nccd.cdc.gov/BRFSSPrevalence/rdPage.aspx?rdReport=DPH_BRFSS.ExploreByTopic&irbLocationType=StatesAndMMSA&islClass=CLASS19&islTopic=TOPIC67&islYear=2017&islLocation=>

We obtain data on **state population in 2017** from the National Cancer Institute’s Surveillance, Epidemiology, and End Results (SEER) Program. Available here: https://seer.cancer.gov/popdata/

Data on **recreational and medicial marijuana status** come from Hollingsworth, Alex and Wing, Coady and Bradford, Ashley, Comparative Effects of Recreational and Medical Marijuana Laws On Drug Use Among Adult and Adolescents (September 27, 2019). Available at SSRN: https://ssrn.com/abstract=3400519 or <http://dx.doi.org/10.2139/ssrn.3400519>


## Software Used:
All analysis were done on unix machines using R 3.6.1. We use a number of user-written packages (`plyr`, `cowplot`, `tidyverse`, `estimatr`, `tidylog`, `huxtable`, `flextable`, `ggrepel`, `mfx`, and `openxlsx`). We use `pacman` to load and install each of these. 

                that should be outlined in the master.do file. We also use a number of shell commands from within stata (whenever the ! command is present). Most of these should still work on a non-unix system, but may need to be modified.

## License:
Replication Package (this github repo): [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Paper: [![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)