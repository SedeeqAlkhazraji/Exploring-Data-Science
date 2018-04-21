# Exploring-Data-Science
Exploring "Kaggle ML and Data Science Survey, 2017" to get better understanding for Data Science field.

Kaggle conducted a survey to establish a comprehensive view of the state of data science and machine learning. Number of participated was over 16,000 where they show their way of working with data, what tool they are using, what the relation between machine learning and industries, and how new data scientists can best break into the field.

We used visualization techniques we explore this dataset, discovered important patterns in the data science field, and concludes different recommendations for data scientist.

#### Required Libraries:
```` 
require(data.table)
require(highcharter)
require(ggplot2)
require(tidyverse)
```` 

#### Read data
```` 
SurveyDf<-read.csv("multipleChoiceResponses.csv",header = TRUE)
```` 

#### Part 1.	Analyzing General Demographics information:
Explor data numarically 
```` 
attach(SurveyDf)
table(SurveyDf$GenderSelect)
table(SurveyDf$Country)
summary(na.omit(SurveyDf$Age))
```` 
barplot of Gender
```` 
hchart(SurveyDf$GenderSelect,type="bar",name="count",color="green") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Barplot of gender",align="center") %>%
  hc_add_theme(hc_theme_elementary()) 
```` 


![Images are easy](https://github.com/SedeeqAlkhazraji/Exploring-Data-Science/blob/master/Report_Img/1.png)


histogram of age of participants
```` 
hchart(na.omit(SurveyDf$Age),name="count",color="orange") %>%
  hc_title(text="Histogram of Ages of the participants",align="center") %>%
  hc_exporting(enabled=TRUE) %>%
  hc_add_theme(hc_theme_elementary())
```` 

2.	Region:	9
3.	Learning data science in Education:	10
4.	Job info:	10
5.	Tools for implementing Data science:	12
6.	ML method:	13
7.	Data Collection and learning:	14
8.	Language Recommendation:	15
9.	Look-Alike Model Using Euclidean Distance:	16
