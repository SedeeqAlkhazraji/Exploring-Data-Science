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

#### Part 1)	Analyzing General Demographics information:
Explor data numarically 
```` 
attach(SurveyDf)
table(SurveyDf$GenderSelect)
table(SurveyDf$Country)
summary(na.omit(SurveyDf$Age))
```` 
Gender 
```` 
hchart(SurveyDf$GenderSelect,type="bar",name="count",color="green") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Barplot of gender",align="center") %>%
  hc_add_theme(hc_theme_elementary()) 
```` 


![barplot of Gender](https://github.com/SedeeqAlkhazraji/Exploring-Data-Science/blob/master/Report_Img/1.png)

[Interactive barplot of Gender](http://rpubs.com/Sedeeq/No1)

Age of participants
```` 
hchart(na.omit(SurveyDf$Age),name="count",color="orange") %>%
  hc_title(text="Histogram of Ages of the participants",align="center") %>%
  hc_exporting(enabled=TRUE) %>%
  hc_add_theme(hc_theme_elementary())
```` 
![barplot of Gender](https://github.com/SedeeqAlkhazraji/Exploring-Data-Science/blob/master/Report_Img/2.png)

[Interactive histogram for age of participants](http://rpubs.com/Sedeeq/No2)

#### Part 2)	Analyzing Region data:
#barplot of country

```` 
hchart(SurveyDf$Country,type="bar",name="Count",color="blue") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Barplot of Country or participants",align="center") %>%
  hc_add_theme(hc_theme_elementary()) 
```` 

![barplot of Gender](https://github.com/SedeeqAlkhazraji/Exploring-Data-Science/blob/master/Report_Img/3.png)

[Interactive histogram for age of participants](http://rpubs.com/Sedeeq/No3)

#treemap of top 10  countries  of participant
```` 
countryCount<-as.data.frame(table(SurveyDf$Country)) %>%  top_n(10) 
hchart(countryCount,hcaes(Var1,value=Freq,color=Freq),name="Count of participants",type="treemap") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Tree map of top 10 countries of participants",align="center") %>%
  hc_add_theme(hc_theme_elementary()) 
```` 

![barplot of Gender](https://github.com/SedeeqAlkhazraji/Exploring-Data-Science/blob/master/Report_Img/4.png)

[Interactive histogram for age of participants](http://rpubs.com/Sedeeq/No4)

#### Part 3) Learning data science in Education:

Let’s see how many are students

```` 
table(SurveyDf$StudentStatus)#most didn't fill this field
#let's check if participants are learning DS or not
table(SurveyDf$LearningDataScience) #most of them didn't answered this too

hchart(LearningDataScience,name="count",type="column",color="#99FF33") %>%
  hc_title(text="Barplot of Learning Data science field",align="center") %>%
  hc_exporting(enabled=TRUE) %>%
  hc_add_theme(hc_theme_elementary())
```` 

![barplot of Gender](https://github.com/SedeeqAlkhazraji/Exploring-Data-Science/blob/master/Report_Img/5.png)

[Interactive histogram for age of participants](http://rpubs.com/Sedeeq/No5)


Let’s see how many are Coders

```` 
table(CodeWriter)
hchart(CodeWriter,type="column",name="Count",color="#9645FF") %>%
  hc_title(text="Barplot of Number of Coders",align="center") %>%
  hc_exporting(enabled=TRUE) %>%
  hc_add_theme(hc_theme_elementary())
```` 

![barplot of Gender](https://github.com/SedeeqAlkhazraji/Exploring-Data-Science/blob/master/Report_Img/6.png)

[Interactive histogram for age of participants](http://rpubs.com/Sedeeq/No6)

  

4.	Job info:	10
5.	Tools for implementing Data science:	12
6.	ML method:	13
7.	Data Collection and learning:	14
8.	Language Recommendation:	15
9.	Look-Alike Model Using Euclidean Distance:	16
