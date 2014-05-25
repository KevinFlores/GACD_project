+	CodeBook
+	Raw Data Used:
+	The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.
+	
+	The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details.
+	
+	Raw Data Files:
+	
+	X_train.txt: contains variables for the train set
+	y_train.txt: contains labels for the train set
+	X_test.txt: contains variables for the test set
+	y_test.txt: contains labels for the test set
+	subject_train.txt: IDs of subjects in train
+	subject_test.txt: IDs of subjects in test
+	features.txt: names of features, indicating the measurement
+	activity_labels.txt: names of each of the activities corresponding to the label
+	Objective:
+	Script run_analysis.R will build a new tidy data set in several steps:
+	
+	Merges the training and the test sets to create one data set.
+	Extracts only the measurements on the mean and standard deviation for each measurement.
+	Uses descriptive activity names to name the activities in the data set
+	Appropriately labels the data set with descriptive activity names.
+	Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
+	Variable :
+	 * subject                    : There is 30 volunteers so this value will contain integer value from 1 - 30
+	 * activity                   : This is a Factor/Categorical variable  with 6 levels WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING
+	
+	 You can see the detail about the above value from [Human Activity Recognition Using Smartphones Data Set](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)
+	
+	 * tBodyAcc.mean...X          : num  
+	 * tBodyAcc.mean...Y          : num  
+	 * tBodyAcc.mean...Z          : num  
+	 * tBodyAcc.std...X           : num  
+	 * tBodyAcc.std...Y           : num  
+	 * tBodyAcc.std...Z           : num  
+	 * tGravityAcc.mean...X       : num  
+	 * tGravityAcc.mean...Y       : num  
+	 * tGravityAcc.mean...Z       : num  
+	 * tGravityAcc.std...X        : num  
+	 * tGravityAcc.std...Y        : num  
+	 * tGravityAcc.std...Z        : num  
+	 * tBodyAccJerk.mean...X      : num  
+	 * tBodyAccJerk.mean...Y      : num  
+	 * tBodyAccJerk.mean...Z      : num  
+	 * tBodyAccJerk.std...X       : num  
+	 * tBodyAccJerk.std...Y       : num  
+	 * tBodyAccJerk.std...Z       : num  
+	 * tBodyGyro.mean...X         : num  
+	 * tBodyGyro.mean...Y         : num  
+	 * tBodyGyro.mean...Z         : num  
+	 * tBodyGyro.std...X          : num  
+	 * tBodyGyro.std...Y          : num  
+	 * tBodyGyro.std...Z          : num  
+	 * tBodyGyroJerk.mean...X     : num  
+	 * tBodyGyroJerk.mean...Y     : num  
+	 * tBodyGyroJerk.mean...Z     : num  
+	 * tBodyGyroJerk.std...X      : num  
+	 * tBodyGyroJerk.std...Y      : num  
+	 * tBodyGyroJerk.std...Z      : num  
+	 * tBodyAccMag.mean..         : num  
+	 * tBodyAccMag.std..          : num  
+	 * tGravityAccMag.mean..      : num  
+	 * tGravityAccMag.std..       : num  
+	 * tBodyAccJerkMag.mean..     : num  
+	 * tBodyAccJerkMag.std..      : num  
+	 * tBodyGyroMag.mean..        : num  
+	 * tBodyGyroMag.std..         : num  
+	 * tBodyGyroJerkMag.mean..    : num  
+	 * tBodyGyroJerkMag.std..     : num  
+	 * fBodyAcc.mean...X          : num  
+	 * fBodyAcc.mean...Y          : num  
+	 * fBodyAcc.mean...Z          : num  
+	 * fBodyAcc.std...X           : num  
+	 * fBodyAcc.std...Y           : num  
+	 * fBodyAcc.std...Z           : num  
+	 * fBodyAccJerk.mean...X      : num  
+	 * fBodyAccJerk.mean...Y      : num  
+	 * fBodyAccJerk.mean...Z      : num  
+	 * fBodyAccJerk.std...X       : num  
+	 * fBodyAccJerk.std...Y       : num  
+	 * fBodyAccJerk.std...Z       : num  
+	 * fBodyGyro.mean...X         : num  
+	 * fBodyGyro.mean...Y         : num  
+	 * fBodyGyro.mean...Z         : num  
+	 * fBodyGyro.std...X          : num  
+	 * fBodyGyro.std...Y          : num  
+	 * fBodyGyro.std...Z          : num  
+	 * fBodyAccMag.mean..         : num  
+	 * fBodyAccMag.std..          : num  
+	 * fBodyBodyAccJerkMag.mean.. : num  
+	 * fBodyBodyAccJerkMag.std..  : num  
+	 * fBodyBodyGyroMag.mean..    : num  
+	 * fBodyBodyGyroMag.std..     : num  
+	 * fBodyBodyGyroJerkMag.mean..: num  
+	 * fBodyBodyGyroJerkMag.std.. : num
