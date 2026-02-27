These values are based on LFMM lasso analysis with the different environmental variables. Each variable was scaled apart from exposure which was a categorical variable. I would also like to get the selective sweep analysis and FST analysis into the same table. There are more environmental variables but I reduced them down to this set based on clustering and their correlation.

Columns are as follows.
SNP_ID - SNP location REF and ALT
p_value - Raw p-value of that SNP
q_value - FDR adjusted p-value
Bonferroni- BF adjusted p-value
Remaining columns are the effect size of the SNP.
Sig_fdr - binary 0 or 1 whether the SNP is significant or not based on FDR
Sig_bf - binary 0 or 1 whether the SNP is significant or not based on BF correction.
