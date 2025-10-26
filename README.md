# The eye that guards the rock: does it shape the world? Nest site attendance patterns in a colonially breeding pelagic seabird

This repository is associated with the manuscript titled: "The eye that
guards the rock: does it shape the world? Nest site attendance patterns
in a pelagic seabird, the little auk" It is now submitted to a
scientific journal: Behavioural Ecology and Sociobiology, being
considered for publication.

In this study, patterns of nest site attendance in male and female of
the little auk throughout the entire breeding season were investigated
in the context of quite ancient Williams' hypothesis The hypothesis has
been suggested to explain male-parental care in fish, here is used to
explain a transition from biparental to and male-only care as it is
observed in the little auk. Data of failed breeders nest site attendance
was also considered, to disablement the relative contributions of
parental investment and territoriality.

The repository provides all the data used in the study (three .xlsx
files, and their .rds equivalent) and the script with these data
analysis. Specific research questions, methodology and results as well
as all the scientific background and the discussion provided in the text
of the manuscript - for now beyond the repo; to be linked when
published.

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

