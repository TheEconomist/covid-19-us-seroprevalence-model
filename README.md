# covid-19-us-seroprevalence-model

This document provides replication code and data for our US model of covid-19 infections. The R script estimates the level of infections in the country in any given week, with primitives from the CDC (deaths by age and gender, and estimated time between infection and death), official case data from John Hopkins CSSE, and the study "Age-specific mortality and immunity patterns of SARS-CoV-2" by Megan O’Driscoll et al (2020). 

To replicate our analysis, clone this repository to your local computer and run "DC_us_seropositivity_model_rep.R".

This analysis is contingent on the assumptions we lay out, particularly on the accuracy of the relation between registered deaths and underlying associated infections, which may change over time. We hope future work can improve and expand upon the approach presented here, and thank the research community and data providers for providing the data and estimates we make use of. 
