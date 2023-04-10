Data and code used in Diniz and Aguiar (2023). Journal of Animal Ecology (under review). 

"Spatiotemporal trends in floral visitation and interaction networks reveal shifting niches for bats in a Neotropical savanna"

Version: 10.04.2023

Authors: Ugo M. Diniz (Technische Universität München), Ludmilla M. S. Aguiar (Universidade de Brasília).

E-mail for contact: ugo.diniz@tum.de

*DISCLAIMER*: The content of this repository is intented to be used a tool for research only. You may reproduce the code and data freely and use in published scientific material, as long you cite the original source. We are not responsible for consequences of its misuse in any occasion, be it for research or any other purpose. 

R version used: 4.1.0 (2021-05-18) "Camp Potanezen".

-> Content
	1. Excel (.xlsx) files
		- bat_phenology -> species-specific abundance of bats per month for graph construction.
		- pca_bats -> species-specific abundance of bats per site for ordination analysis.
		- pca_plants -> species-specific abundance of plants per site for ordination analysis.
		- phenology_general -> abundance data for all groups per month with corresponding degrees. Used for circular analysis.
		- plant_phenology -> species-specific abundance per month (plants) for graph construction.
		- raw_bat_occurrence -> raw data with all bat individuals captured per sampling night and per site. Not used in the code.
		- spatial_means -> averaged spatial distribution of RON and POG for graph construction.
		- spatial_raw -> Detailed distribution of RON and POG per site, used for ANOVA analysis.
		- temporal_means -> temporal distribution of RON and POG plus trasformations for circular analysis.

	2. Text (.txt) files
		- dry -> partial interaction matrix corresponding to the peak dry season.
		- dry-wet -> partial interaction matrix corresponding to the dry-wet transition.
		- edge_net -> partial interaction matrix corresponding to the forest edge sites.
		- forest_net -> partial interaction matrix corresponding to the forest interior sites.
		- savanna_net -> partial interaction matrix corresponding to the savanna sites.
		- wet -> partial interaction matrix corresponding to the peak wet season.
		- wet-dry -> partial interaction matrix corresponding to the wet-dry transition.

	3. script.R -> R script with all the code used in the article.


## Reference

Diniz, U. M., Aguiar, L. M. S. (2023) "Spatiotemporal trends in floral visitation and interaction networks reveal shifting niches for bats in a Neotropical savanna". Journal of Animal Ecology (Under review)
