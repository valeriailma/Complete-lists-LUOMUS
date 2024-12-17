# Complete-lists-LUOMUS
 
This code is for analysing the Complete list (in Finnish Täydelliset listat) species observation data downloaded from Finnish Biodiversity Information Facility (https://laji.fi/observation).  

The purpose of the script 1 is to transform data downloaded from laji.fi portal into a correct form for Joint Species Distribution Modelling using HSMC modeling framework (Hierarchical Modelling of Species Communities, scripts 2-7). For more information on HMSC, please refer to the book from Ovaskainen and Abrego (2020), or to Ovaskainen et al. 2017 (see below). The code of script “1 - Download and modify data” has been written on the basis of code by Ovaskainen et al., and all other scripts (2-7) applying HMSC framework are entirely written by Ovaskainen et al., and have been downloaded from this website: https://www.helsinki.fi/en/researchgroups/statistical-ecology/software/hmsc .  

Instructions on downloading Complete lists data from FinBIF (laji.fi): 

1) Go to https://laji.fi/en/observation and choose ‘Sources’ from the right panel. 
2) Clikc ‘Select cources’ and write ‘Complete lists’ into the search bar. (Complete list datasets can be found also under Collections of the Finnish Museum of Natural History > Luomus monitoring schemes > Complete list monitoring scheme.) 
3) Choose a Complete list (species group) you are interested in and press ok. You can choose also other filters from the panel on the right, such as place or date. 
4) Click Search from upper right corner. Then click Download above the updated observations table, and a pop up window appears. Click here Select columns (next to another Download-button). 
5) Select columns you wish to be included. Recommended to choose at least  Submission identifier, Taxonomic order, Scientific name, ETRS.TM35FIN (or WGS84), Location accuracy (m), Time, Observer(s)/Collector(s), and The stated certainty of the observation. Depending on the species group, you may also include columns such as Life stage (plants) or Number.  
6) Press ok and Download the data. A tsv file will be downloaded. 


References:

Ovaskainen, O. and Abrego, N. 2020. Joint Species Distribution Modelling - With Applications in R. Cambridge University Press  

Ovaskainen, O., Tikhonov, G., Norberg, A., Blanchet, F. G., Duan, L., Dunson, D., Roslin, T. and Abrego, N. 2017. How to make more out of community data? A conceptual framework and its implementation as models and software. Ecology Letters 20, 561-576. https://doi.org/10.1111/ele.12757.  
