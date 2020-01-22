# Libraries and Packages
library(reshape)
library(reshape2)

# Reading all versions of class level data of one project
AndroidClass_V1=read.csv("/media/pramod/Education/Research/Work/DataSets/GitHubBugDataSet/database/Android-Universal-Image-Loader/2013-01-19/Android-Universal-Image-Loader-Class.csv",header = TRUE, sep = ",")
AndroidClass_V2=read.csv("/media/pramod/Education/Research/Work/DataSets/GitHubBugDataSet/database/Android-Universal-Image-Loader/2013-07-24/Android-Universal-Image-Loader-Class.csv",header = TRUE, sep = ",")
AndroidClass_V3=read.csv("/media/pramod/Education/Research/Work/DataSets/GitHubBugDataSet/database/Android-Universal-Image-Loader/2013-12-27/Android-Universal-Image-Loader-Class.csv",header = TRUE, sep = ",")
AndroidClass_V4=read.csv("/media/pramod/Education/Research/Work/DataSets/GitHubBugDataSet/database/Android-Universal-Image-Loader/2014-05-24/Android-Universal-Image-Loader-Class.csv",header = TRUE, sep = ",")
AndroidClass_V5=read.csv("/media/pramod/Education/Research/Work/DataSets/GitHubBugDataSet/database/Android-Universal-Image-Loader/2014-09-06/Android-Universal-Image-Loader-Class.csv",header = TRUE, sep = ",")
AndroidClass_V6=read.csv("/media/pramod/Education/Research/Work/DataSets/GitHubBugDataSet/database/Android-Universal-Image-Loader/2015-05-29/Android-Universal-Image-Loader-Class.csv",header = TRUE, sep = ",")

# Combine all these versions of class level metrics data into one file.
MergedAndroidDataFrames <- merge(AndroidClass_V1,AndroidClass_V2,AndroidClass_V3,AndroidClass_V4,AndroidClass_V5,AndroidClass_V6, by.x="Component")
head(MergedAndroidDataFrames)
MergedAndroidDataFrames <- list(AndroidClass_V1,AndroidClass_V2,AndroidClass_V3,AndroidClass_V4,AndroidClass_V5,AndroidClass_V6)
merged_df <- merge_all(MergedAndroidDataFrames)

