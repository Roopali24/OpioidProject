# OpioidProject
Regarding paper "Keeping Homeless Youth off Opioids: Optimizing Intervention Delivery using Causal Inference"

File (OpioidProject.R) has all the code required to:
1. Impute missing data using mice.
2. Run BART, DT and LASSO regression on the modified imputed data.
3. Make 95% CI for causal inference for each pair of interventions.

File (OpioidProject.R) has all the code required to:
1. randomly select k people when the assignment to some intervention exceeds capacity.
2. calculate the no. of true opioid users that are assigned a intervention.

Some modifications to the data files and computation were done using excel.


