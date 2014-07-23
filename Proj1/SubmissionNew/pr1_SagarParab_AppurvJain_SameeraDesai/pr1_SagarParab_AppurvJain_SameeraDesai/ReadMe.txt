List of files in this submission:

1. AnomalyDetectionConfusion.txt - Text version of the output of the SVM classifier on the Anomaly Data.

2. MisuseDetectionConfusion.txt - Text version of the output of the SVM classifier on the Misuse Data.

3. DataPrep.R - This is a R code file which combines instances from 5 attack files and instances from the normal file and generates 2 CSV files 'AnomalyMasterDataSet.csv' and 'MisuseMasterDataSet.csv'. Afer that, it also trains and tests the data using the Suport Vector Machine Classifier and prints the confusion matrices along with some other performance metrics.
Note: Inorder to run the R script, one will have to change the folder path which is hardcoded in the script and also include the the data files provided by the professor in the folder. This is becuase the script pulls data from those files in order to construct the above mentioned data sets.

4. AnomalyMasterDataSet.csv - The file generated during Data Preprocessing by the above mentioned 'DataPrep.R' file for Anomaly detection. This file consists of a total 3000 instances, Normal(no-attack) 2000 instances and not-normal(attack) 1000 instances.

5. MisuseMasterDataSet.csv - The file generated during Data Preprocessing by the above mentioned 'DataPrep.R' file for Misuse detection. This file consists of a total 3000 instances, Normal(no-attack) 2000 instances and Attack 1000 instances.