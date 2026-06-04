# The eye that guards the rock: does it shape the world? Nest site attendance patterns in a colonially breeding pelagic seabird

<div align="justify">
This repository is associated with the manuscript titled: "The eye that
guards the rock: does it shape the world? Nest site attendance patterns
in a pelagic seabird, the little auk" It is now submitted to a
scientific journal: Behavioural Ecology and Sociobiology, being
considered for publication.

This study examines territory attendance—defined as 
non‑parental nest‑site attendance—in male and female little auks (*Alle alle*) 
across the entire breeding season. The attendance patterns of successful breeders with those 
of pairs that experienced hatching failure is also compared.


The repository provides all the data used in the study (three .xlsx
files, and their .rds equivalent) and the script with these data
analysis. Specific research questions, methodology and results as well
as all the scientific background and the discussion provided in the text
of the manuscript - for now beyond the repo; to be linked when
published. Originally, the data were analyzed using a frequentist approach. 
However, in response to the reviewers’ concerns about the small sample size 
in the failed‑breeder dataset, we reanalyzed the data within a Bayesian framework, 
which is better suited for handling such limitations. 
The original frequentist analysis script has been retained (*Data analysis.R*), 
and a Bayesian version is now provided as well (*Data analysis_bayes.R*).


**DA_patterns of breeders.rds** – data with duration of nest site attendance
per 24 hours time period of breeding individuals, followed over
different stages of the breeding season (mating, incubation, chick
rearing).

*season* – year of the breeding season nest – nest identitity of the
followed individual   

*sx* – sex of the followed individual 

*ringno* – unique ring number of the followed individual 

*pheno_day* – consecutive days of the breeding season, with 0 being hatching day, and so negative numbers
representing days before hatching and positive numbers representing days
of the chick rearing period 

*col_att_min_per24*– nest site attendance per
24 hours of the followed individual (in minutes)  

*time index* – a sequence
of the integers for the time serie for the focal individual; an
analytical variable for handling the issue of autocorrelation.  



**DA_breeders_non_breeders_24.rds** – data with duration of nest site
attendance per 24 hours time period of breeding individuals and failed
breeders (unsuccessful hatching), followed in two stages of the chick
rearing period.

*session* – recording session – stage of the chick rearing (early and mid
chick rearing) 

*sx* – sex of the followed individual status – breeding

*status* - breeder or failed breeder ringno – unique ring number of the
followed individual

*nest* – nest identitity of the followed individual 

*on_screen_min24* - nest site attendance per 24 hours of the followed
individual (in minutes)

**DA_breeders_non_breeders_46.rds**– data with duration of nest site
attendance per 46 hours time period of breeding individuals and failed
breeders (unsuccessful hatching), followed in two stages of the chick
rearing period.

*session* – recording session – stage of the chick rearing (early and mid
chick rearing) 

*sx* – sex of the followed individual status – breeding

*status* - breeder or failed breeder ringno – unique ring number of the
followed individual

*nest* – nest identitity of the followed individual 

*on_screen_min46* - nest site attendance per 46 hours of the followed
individual (in minutes)

</div>
