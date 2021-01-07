# 2020-TSE-Developers-Perceptible-Ethnicity-and-PR-evaluation
This is the reproducibility package of our TSE paper  "On the Relationship Between the Developer's Perceptible Ethnicity and the Evaluation ofContributions in GitHub"

In this package you will find the datasets and scripts we have used in the paper as well as some appendix that explain our decisions and give more insights into the results.

The Datasets are found in the dataset folder, the appendixes in the Appendix folder, and the scripts in the R scripts folder.

**Inside the dataset folder**, the dataset used for building the models is: **"pull_requests.csv"**. Also, there are other intermediate datasets in the folder that were used to analyze the distribution of the data (see Appendix A).

**Inside the R scripts folder**, the script that reproduce the models is called **"replication_script.R"**. The script **"comparing_models.R"** was used to statistically compare the models build with "replication_script.R". Finally, the script **"distribution_analysis.R"** was used to analyze the distribution of our data.

**Inside the Appendix folder**, there are three appendixes. **Appendix A** has the information about the distribution of the data. **Appendix B** has the sensitivity analysis of the Name-Prism tool that we used to select the best threshold for inferring perceptible ethnicities. **Appendix C** has the summary of all the models build in the paper.
