run_analysis <- function(x) {
    # Read all 8 files into R
    subjecttest = read.table("subject_test.txt")
    subjecttrain = read.table("subject_train.txt")
    xtest = read.table("X_test.txt")
    ytest = read.table("y_test.txt")
    xtrain = read.table("X_train.txt")
    ytrain = read.table("y_train.txt")
    features = read.table("features.txt")
    activity = read.table("activity_labels.txt")
                             
    #Create vector of feature names to use for column names
    featuresvector = features[ , "V2"]
    colnames(xtest) <- featuresvector
    colnames(xtrain) <- featuresvector
    
    #Add description of activity to ytest & ytrain
    newtest = cbind(ytest, activity[ytest[ , 1], "V2"])
    newtrain = cbind(ytrain, activity[ytrain[ , 1], "V2"])
    
    #Create column names for subject and activity
    colnames(subjecttest) = "Subject"
    colnames(subjecttrain) = "Subject"
    colnames(newtest) = c("ActivityNum", "Activity")
    colnames(newtrain) = c("ActivityNum", "Activity")
    
    # Create new test and train databases with subject number included
    test = cbind(subjecttest, newtest, xtest)
    train = cbind(subjecttrain, newtrain, xtrain)
    
    #Create final database with test and train combined
    total = rbind(test, train)
    
    #Extract the columns that have mean or std but not meanFreq in their name to a new file
    extract = total[ , 1:3]
    x = 4
    
    while (x <= 564)  {
        addcolumn = FALSE
        columnname = colnames(total) [x]
        if (grepl("mean", columnname) == TRUE) {addcolumn = TRUE}
        if (grepl("std", columnname) == TRUE) {addcolumn = TRUE}
        if (grepl("meanFreq", columnname) == TRUE) {addcolumn = FALSE}
        
        if (addcolumn == TRUE) {
            extract = cbind(extract, total[, x])
            columnnumb = ncol(extract)
            colnames(extract)[columnnumb] = columnname
        }
        x = x+1
   }

    #Pretty up the column names
    numcolumns = ncol(extract)
    x = 4
    
    while (x <= numcolumns)  {
        oldname = colnames(extract) [x]
        numchar = nchar(oldname)
        z = 6
        newname = substr(oldname, 1, 5)
        while (z <= numchar)   {
           curchar = substr(oldname, z, z) 
           if (curchar == "-") {curchar = "."}
           if (curchar == "(")  {curchar = ""}
           if (curchar == ")")  {curchar = ""}
           newname = paste(newname, curchar, sep="")
           z = z+1
        }
        colnames(extract)[x] = newname
        x = x+1
    }  

    #Extract just the columns that have mean in their name to extract2 file
    extract2 = extract[ , 1:3]
    x = 4
    
    while (x <= 69)  {
        addcolumn = FALSE
        columnname = colnames(extract) [x]
        if (grepl("mean", columnname) == TRUE) {addcolumn = TRUE}
    
        if (addcolumn == TRUE) {
            extract2 = cbind(extract2, extract[, x])
            columnnumb = ncol(extract2)
            colnames(extract2)[columnnumb] = columnname
        }
        x = x+1
    } 
   


   #Melt extract2 into a long file & dcast back the means
   meltdf = melt(extract2, id.vars = c("Subject", "ActivityNum", "Activity"))
   dcastdb=dcast(meltdf, Subject + Activity ~ variable, mean)

    
    return(dcastdb)   
    
    
}    