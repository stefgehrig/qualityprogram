# qualityprogram

This repository contains data and analysis code to reproduce results from the article **"Estimating the causal effect of a quality assurance program on quality of care in Germany"**.

## R scripts

R scripts to reproduce all analyses from the article can be found in the `/R` folder. Results are exported to `/results`.

## Data dictionary `resultsindicators.csv`

In the `/data` folder, the file `data_indicators.csv` contains all data used to produce the results. These are the annual observations of national-level quality indicator results in the observation period from 2013 to 2020 for all indicators in the analysis.

|Column name |Explanation                                                                                                                             |Official term (German) |
|:-----------|:---------------------------------------------------------------------------------------------------------------------------------------|:----------------------|
|year        |Year of observation                                                                                                                     |Erfassungsjahr         |
|indic_id    |Unique public ID of quality indicator                                                                                                   |ID                     |
|indic_descr |Verbal description of quality indicator                                                                                                 |Indikatorbezeichnung   |
|indic_type  |Type of quality indicator (P: Process; I: Indication)                                                                                   |Indikatorart           |
|clin_area   |Clinical area of quality indicator                                                                                                      |Leistungsbereich       |
|o           |Number of observed events                                                                                                               |Ergebnis: Zähler       |
|n           |Population size                                                                                                                         |Ergebnis: Nenner       |
|o_adverse   |Number of observed events, rescaled as adverse care events (i.e., reverse coded in case indicator originally measures favorable events) |-                      |
|group       |Treatment group of quality indicator                                                                                                    |-                      |

## R session info

R session info (from `R/compute_results.R`) can be accessed <a href="sessionInfo.txt">here</a>.
