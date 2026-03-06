Pseudomonas-aeruginosa-and-Klebsiella-pneumoniae-phenazines
Source data and code for the associated manuscript

Hi there! Thanks for taking a look at this. This repository is intended to store R files and raw data files associated with the above project. 
The goal of this repository is to provide open access to the analysis performed for the above project, such that it can be replicated by any end-user. 
If you decide to run this analysis yourself, make sure to change all of the directories!

# The raw data files are:
# associated with analysis file 1
combined_LB_competition.csv
combined_LB_growth_curves.csv
# associated with analysis file 2
combined_cas_competition.csv
combined_cas_growth_curves.csv
# associated with analysis file 3
combined_glu_competition.csv
# associated with analysis file 4
combined_biolog.csv
# associated with analysis file 5
pa14nr_all_data.csv
combined_LB_Pa14_Tn_growth_curves.csv
combined_Tn_LB_competition.csv
combined_Tn_phen_absord_data.csv
# associated with analysis file 6
combined_phen_Ec_competition.csv
# associated with analysis file 7
207-PCAfragment_long_format.csv
211-pyo_long_format.csv
239-5MPCA_long_format.csv
# associated with analysis file 8
combined_aerobic_MIC_MBC.csv
combined_anaerobic_MIC.csv
combined_anaerobic_NO3_MIC.csv
PYO_std.csv
5MPCA.csv
PYO_5MPCA_checkerboard.csv
# associated with analysis file 9
combined_Kp_clin_screen_data.csv
Kp_clin_validation.csv
Kp_clinical_Ec.csv
# associated with analysis file 10
combined_clin_pa_restrict_data.csv
combined_clin_phen_absord_data.csv
combined_clin_Pa_validation.csv
combined_clin_Pa_spent_data.csv
5MPCA_PYO_spectra.csv
Pa_clin_OD500.csv
# associated with analysis file 11
all_gut_data.csv
all_BALF_data.csv
all_blad_data.csv
# associated with analysis file 12
original_mouse_cfus.csv
gut_colonization_data.csv

# The data analysis files are:
# Please note that these data analysis files will need to be edited by the endpoint user to import the raw data
# (1) analysis of Klebsiella pneumoniae and Pseudomonas aeruginosa growth in LB
pseudomonas_Kp_LB_analysis.R
# (2) analysis of Klebsiella pneumoniae and Pseudomonas aeruginosa growth in casamino acids
pseudomonas_Kp_cas_analysis.R
# (3) analysis of Klebsiella pneumoniae and Pseudomonas aeruginosa growth in glucose
pseudomonas_Kp_glu_analysis.R
# (4) analysis of Klebsiella pneumoniae, Pseudomonas aeruginosa, and Escherichia coli growth in BioLog assays
pseudomonas_Kp_biolog_analysis.R
# (5) analysis of Klebsiella pneumoniae growth PA14NR transposon library screen 
Pa14NR_screen_analysis.R		
# (6) analysis of Klebsiella pneumoniae growth in the context of phenazine-expressing Escherichia coli strains
phen_Ec_Kp_LB_analysis.R		
# (7) analysis of phenazine LC-MS profiles
phen_LCMS.R		
# (8) analysis of phenazine activity and quantification
phen_quant.R
# (9) analysis of Klebsiella pneumoniae clinical strains
Kp_clinical_screen_analysis.R		
# (10) analysis of Pseudomonas aeruginosa clinical strains
pseudomonas_clinical_screen_analysis.R	
# (11) analysis of Klebsiella pneumoniae growth in ex vivo tissue
pseudomonas_ex_vivo.R			
# (12) analysis of Klebsiella pneumoniae growth in vivo
pseudomonas_in_vivo.R
