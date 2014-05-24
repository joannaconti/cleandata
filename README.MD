README.MD
run_analysis.R was created to calculate the means of 33 variables by subject and activity for body acceleration and velocity.  The original data is contained in 8 files, each of which need to be present in the working directory in order for run_analysis to work.  The 8 files are:  "subject_test.txt",  "subject_train.txt",  "X_test.txt",  "y_test.txt", "X_train.txt", "y_train.txt", "features.txt", and "activity_labels.txt".  
The script reads in the 8 files and then renames the columns in the xtest and xtrain files with descriptive names from the features file using the following code:
    featuresvector = features[ , "V2"]
    colnames(xtest) <- featuresvector
The description of the activity is added to the ytest and ytrain files as follows:
    newtest = cbind(ytest, activity[ytest[ , 1], "V2"])
    newtrain = cbind(ytrain, activity[ytrain[ , 1], "V2"])
Information about the subject for each row is added to the xtrain and xtest dataframes as follows:
    test = cbind(subjecttest, newtest, xtest)
    train = cbind(subjecttrain, newtrain, xtrain)
The test and train dataframes are combined into one dataframe:
    total = rbind(test, train)
A new dataframe containing just those variables referencing mean or standard deviation (std) was created in a while loop with these key lines of code:
        if (grepl("mean", columnname) == TRUE) {addcolumn = TRUE}
        if (grepl("std", columnname) == TRUE) {addcolumn = TRUE}
        if (grepl("meanFreq", columnname) == TRUE) {addcolumn = FALSE}
        if (addcolumn == TRUE) {
            extract = cbind(extract, total[, x])
            columnnumb = ncol(extract)
            colnames(extract)[columnnumb] = columnname  }
Dashes in column names were replaced by periods, and parentheses removed.  At this point, the dataframe extract containing data on mean and standard deviation readings by subject and activity level was ready for analysis.
Because standard deviation data was not required in the rest of the analysis, a new dataframe extract2 was created without standard deviation data.
Extract2 was then melted into a long-form data file and then the means calculated by subject and activity and recast into a wide datafile containing the average readings for each of the 33 variables using these commands:
   meltdf = melt(extract2, id.vars = c("Subject", "ActivityNum", "Activity"))
   dcastdb=dcast(meltdf, Subject + Activity ~ variable, mean)
Dcastdb was returned as a tidy data set.

	                      by Joanna L. Conti on  5/24/14