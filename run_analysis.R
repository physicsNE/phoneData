## run_analysis.R
## McGuirk 6/14-21/2015

## Problem directions:
##  You should create one R script called run_analysis.R that does the following. 
##    1) Merges the training and the test sets to create one data set.

##    2) Extracts only the measurements on the mean and standard deviation for each measurement. 
#***     Interpreted 2) as the calculated mean and standard deviation features reported 
#***     for each of the six measured dimensions. 


##    3) Uses descriptive activity names to name the activities in the data set
##    4) Appropriately labels the data set with descriptive variable names. 
##    5) From the data set in step 4, creates a second, independent tidy data set, 
##       with the average of each variable for each activity and each subject.

##  The data comes from the zip file: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
##  Easiest way is double click on link to download to browser download folder, then unzip to R working directory.

##  The observations are organized into training and test folders, each with six directly observed variables: 
##      three linear and three angular accelerations. 
##  There are also three processed linear variables obtained by subtracing gravity from the linear accelerations. 
##  The phone could be in any orientation, so gravity generally will have x, y, and z components.
##  We are ignoring the raw data and using only the "feature set" files, "X-train.txt" and "X-test.txt" 


##  The fixed variables identifying which volunteer and which task
##      were correlated with each observation are given in separate files.

##  defining "constants", such as file names and formats
    ## format to read the 12 variables of interest from the 561 features derived from each observation
    ## col 1:6 are linear, 7:120 are not used, and 121:126 are rotational, >126 are ignored
##  we define the read format to skip data other than means and standard deviations
    fmt<-(c(16,16,16,16,16,16,1824,16,16,16,16,16,16))            ## 1824 = 114 discarded variables, 
                                                                  ##            each 16 characters long
    ## data file names with relative paths from working directory
    X_train<-"./UCI HAR Dataset/train/X_train.txt "               ## data: training feature set 
    subject_train<-"./UCI HAR Dataset/train/subject_train.txt "   ## Fixed: ID number of the volunteer observed
    y_train<-"./UCI HAR Dataset/train/y_train.txt "               ## Fixed: index number of the activity observed
    
    X_test<-"./UCI HAR Dataset/test/X_test.txt "                  ## data: test feature set
    subject_test<-"./UCI HAR Dataset/test/subject_test.txt "      ## Fixed: ID number of the volunteer observed
    y_test<-"./UCI HAR Dataset/test/y_test.txt "                  ## Fixed: index number of the activity observed
    
    featureNames<-"./UCI HAR Dataset/features.txt "               ## the file of 561 feature names 
    featColNames<-read.table(featureNames, nrows=126)             ## read the up to the last line used
    
        ## create the column names for all tidy datasets ******************************
        col<-c("Activity","Subject#", as.character(featColNames[1:6,2]), as.character(featColNames[121:126,2]))
    
    activity<-"./UCI HAR Dataset/activity_labels.txt "            ## the file of 6 activity names
    activityCodes<-read.table(activity)                           ## factor level number, activity name pairs

## read in the training files                                       
    trainingFeat<-read_fwf(X_train, fwf_widths(fmt), n_max=-1)    ## training feature data, 1 row/observation
    training12feat<-trainingFeat[,c(1:6,8:13)]                    ## discard 114 features, keep 12 means, stdev
    actIndex<-read.table(y_train)                                 ## activity observed, 1 row/observation, as factors
    subjIndexTrain<-read.table(subject_train)                     ## subject observed, 1 row/observation, as ID
    ## bind fixed columns to data, TRAIN is unlabeled but tidy training data set
    TRAIN<-cbind(actIndex, subjIndexTrain, training12feat)         
    
    ## read in the test files
    testFeat<-read_fwf(X_test, fwf_widths(fmt), n_max=-1)         ## test feature data, 1 row/observation
    test12feat<-testFeat[,c(1:6,8:13)]                            ## discard 114 features, keep 12 means, stdev
    actIndex<-read.table(y_test)                                  ## activity observed, 1 row/observation, as factors
    subjIndexTest<-read.table(subject_test)                       ## subject observed, 1 row/observation, as ID
    ## bind fixed columns to data, TEST is unlabeled but tidy test data set
    TEST<-cbind(actIndex, subjIndexTest, test12feat)        
    
## STEPs 1 and 2:  merge the two almost tidy datasets
    bigTidy<-rbind(TRAIN, TEST)
#    print(head(bigTidy))                                      ## debug print
    
## STEP 3:  Use descriptive activity names to name the activities in the data set
    ## before converting to names, sort by activity indices to make step 5 easier
        sortedTidy<-bigTidy[order(bigTidy[,1],bigTidy[,2]),]  

    ## convert activity indices in the first column to activity names
        nrowBig<-nrow(bigTidy)
        for (i in 1:nrowBig) {
            bigTidy[i,1]<-as.character(activityCodes[bigTidy[i,1],2])          ## have to tell actName names are not factors
        }

## STEP 4:  Appropriately labels the data set with descriptive variable names
    ## the tidy data has each variable in a separate column
    ## we prepared the program variable "col" with the descriptive variable names in column order
        colnames(bigTidy)<-col                                 ## assigns descriptive column names
    
#    print(str(bigTidy)) 
#    print(sortedTidy[1:500, 1:3])
## STEP 5:  From the data set in step 4, creates a second, independent tidy data set, 
##          with the average of each variable for each activity and each subject.  
    
    ##  There are 6 activities, and 30 subjects, so we should have 180 observations of 12 average
    smallTidy<-sortedTidy[1,]

          for (i in 1:6){
            for (j in 1:30){
              tmean<-sortedTidy[sortedTidy[,1]==i & sortedTidy[,2]==j,]
              for (k in 1:14){
                outRow[k]<-mean(tmean[,k])
            }
              smallTidy<-rbind(smallTidy, outRow)
            }
          }
    smallTidy<-smallTidy[-1,]
    colnames(smallTidy)<-col
    write.table(smallTidy, "./smallTidy.txt", row.names=FALSE)
                                        brk<-"placeholder"
return(date())
