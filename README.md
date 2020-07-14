
# Information Diffusion in Online Communities
M1_Internship_Laboratory_HubertCurien_Information Diffusion in Online Communities

## Main contributions

- Performing the Edge Homogeneity and preprocessing the data for getting the Source Node and Target Node 
- Tool for visualizing the Probability Density Function of Edge Homogeneity
- Prediction of future political stance based on labels with Recurrent Neural Network with LSTM model

# Dataset 
* Data is collected from Nov 2015 - April 2019
* Dataset we have divided into 14 different periods 

## Distribution of TimePeriods from Nov 2015 to April 2019 with number of samples in each periods

| TimePeriod | Number of Samples | StartDate | EndDate |
|------------|-------------------| --------- | ------- |
|T1| 3367| 16th Nov 2015 | 24th June 2016 |
|T2| 6265| 25th June 2016 | 13th July 2016|
|T3| 3084| 14th July 2016 | 7th Dec 2016 |
|T4| 1466| 8th Dec 2017 | 26th Jan 2017 |
|T5| 2300| 27th Jan 2017 | 29th Mar 2017 |
|T6| 4102 | 30th Mar 2017 | 19th June 2017 |
|T7| 54505| 20th June 2017 | 08th July 2018 |
|T8| 23067| 9th July 2018 | 21st Sep 2018 |
|T9| 15385| 22nd Sept 2018 |15th Nov 2018|
|T10| 3718| 16th Nov 2018 | 25th Nov 2018 |
|T11| 25468| 26th Nov 2018 | 15th Jan 2019 |
|T12| 54850| 16th Jan 2019 | 14th Mar 2019 |
|T13| 9119 | 15th Mar 2019  | 21st Mar 2019 |
|T14| 13414| 22nd Mar 2019 | 29th Mar 2019|


# Code
### R - code:

#### 1. EdgeHomogeneity_AllPeriods.R

- We take entire data and perform the preprocessing task to get Source Node and Target Nodes
- As entire dataset has around 220110 samples , This requires good amound of memory(Min 16GB Ram) to do the preprocessing task
- After geting Source Node and Target Node , we divide data according to the Cases and Scenarios
- PDF of Edge Homogeneity is plotted by using the preprocessed data intercept the relation

#### 2- Edge Homogeneity from T1 to T14 periods

- We take period wise data in this individually and perform the preprocessing task
- After geting Source Node and Target Node , we divide data according to the Cases and Scenarios
- PDF of Edge Homogeneity is plotted by using the preprocessed data intercept the relation


### Python - Code :

#### 1. LSTM_allperiods.ipynb

- We take entire data and take the labels of the each user 
- We take labels data and create X,Y pairs for input and output of the Neural Network
- We convert the input data in LSTM format and give it as input to the network
- After building the network we evaluate the model and note test and train score
- We report the classification report from sklearn library and take the results
- we report the F1 score ,Accuracy, Precision, Recall as results


#### 2. LSTM_T1.ipynb to LSTM_T14.ipynb

- We do the predictions of the labels according to the periods wise
- We compare the results for each time period
- we report the F1 score ,Accuracy, Precision, Recall as results

#### 3. LSTM model with Textual Data

- We take the entire dataset in this context
- We include textual features to classify related labels
- We predict labels using the textual data and compare the results


### Results:

- Report folder contains the weekly reports throughout the internship

### FinalDocuments:
-  Final Report of the internship
-  Presentation Slides of the internship
