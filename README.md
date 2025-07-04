# qualityprogram

This repository contains data and analysis code to reproduce results from the article **"Estimating the causal effect of a quality assurance program on quality of care in Germany"** published in [BMC Health Services Research]([/guides/content/editing-an-existing-page#modifying-front-matter](https://doi.org/10.1186/s12913-025-12939-8)).

<div style="display: flex;"> 
  <img src="results/figure_did_results.png" style="height: 350px;"> 
</div> 

## R scripts

The R script to reproduce all analyses from the article can be found in the `/R` folder. Results are exported to `/results`.

## Data dictionary `data_indicators.csv`

In the `/data` folder, the file `data_indicators.csv` contains all data used to produce the results. These are the annual observations of national-level quality indicator results in the observation period from 2013 to 2020 for all indicators in the analysis (final analysis data set).

|Column name |Explanation                                                                                                                             |Official German name                               |
|:-----------|:---------------------------------------------------------------------------------------------------------------------------------------|:--------------------------------------------------|
|year        |Year of observation                                                                                                                     |Erfassungsjahr                                     |
|indic_id    |Unique public ID of quality indicator                                                                                                   |ID                                                 |
|indic_descr |Verbal description of quality indicator                                                                                                 |Indikatorbezeichnung                               |
|indic_type  |Type of quality indicator (P: Process; I: Indication)                                                                                   |Indikatorart                                       |
|clin_area   |Clinical area of quality indicator                                                                                                      |Leistungsbereich                                   |
|o           |Number of observed events                                                                                                               |Ergebnis: Zähler                                   |
|n           |Population size                                                                                                                         |Ergebnis: Nenner                                   |
|o_adverse   |Number of observed events, rescaled as adverse care events (i.e., reverse coded in case indicator originally measures favorable events) |-                                                  |
|group       |Treatment group of quality indicator                                                                                                    |-                                                  |
|n_hospitals |Number of hospitals with a quality indicator result (not included for year 2013, as hospital unit was defined differently)              |Anzahl der Krankenhäuser mit mindestens einem Fall |

In the alternative data set in `data/data_hospitalreports.csv`, the same column names apply. The additional column 'hospital' contains the hospital ID (Institutionskennzeichen).

## R session info

R session info (from `R/compute_results.R`) can be accessed <a href="sessionInfo.txt">here</a>.
