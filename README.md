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

  

4.	Job info:	

#barplot of Emp_status
````
hchart(SurveyDf$EmploymentStatus,type="bar",name="count",color="red") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Barplot of Employment Status",align="center") %>%
  hc_add_theme(hc_theme_elementary()) 
````
![barplot of Gender](https://github.com/SedeeqAlkhazraji/Exploring-Data-Science/blob/master/Report_Img/7.png)

[Interactive histogram for age of participants](http://rpubs.com/Sedeeq/No7)


Age and Employment status
````
hcboxplot(x= Age , name="Age of participants", var = EmploymentStatus,color="purple",outliers = FALSE) %>%
  hc_title(text="Boxplot of Ages of the participants and their Employment Status",align="center") %>%
  hc_exporting(enabled=TRUE) %>%
  hc_add_theme(hc_theme_elementary()) %>% 
  hc_chart(type="column") #for vertical box plot
````
![barplot of Gender](https://github.com/SedeeqAlkhazraji/Exploring-Data-Science/blob/master/Report_Img/8.png)

[Interactive histogram for age of participants](http://rpubs.com/Sedeeq/No8)


Current job title
````
table(CareerSwitcher) #more have changed their jobs than others who haven't
table(CurrentJobTitleSelect)
#let's make a dataframe to plot a barplot
jobdf<-as.data.frame(table(CurrentJobTitleSelect))
jobdf[1,1]<-"Not answered"
jobdf<-na.omit(jobdf)

jobdf %>% arrange(desc(Freq)) %>%   hchart(hcaes(x=CurrentJobTitleSelect,y=Freq),name="Count",color="#751A75",type="column") %>%
  hc_title(text="Barplot of Current Job titles of the participants",align="center") %>%
  hc_exporting(enabled=TRUE) %>%
  hc_add_theme(hc_theme_elementary())
````
![barplot of Gender](https://github.com/SedeeqAlkhazraji/Exploring-Data-Science/blob/master/Report_Img/9.png)

[Interactive histogram for age of participants](http://rpubs.com/Sedeeq/No9)



Trends countrywise X
````
countryJobs<-SurveyDf %>% group_by(Country,CurrentJobTitleSelect) %>%
  filter(Country %in% countryCount$Var1) %>%
  dplyr::select(Country,CurrentJobTitleSelect) %>%
  summarize(total=n()) %>%
  arrange(desc(total))

countryJobs[1:2,]<-NA
countryJobs<-na.omit(countryJobs)

#making separate countries for comparative plots of Jobs of that country's participants.

#Colors vectors for plotting
USIndJobs<-countryJobs %>% filter(Country %in% c("United States","India"))
colors <- c("#d35400", "#2980b9", "#2ecc71", "#f1c40f", "#2c3e50", "#7f8c8d","#000004", "#3B0F70", "#8C2981", "#DE4968", "#FE9F6D", "#FCFDBF","#ffb3b3","#66ff33","#00b3b3","#4d4dff")

colors2<-c("#d35400", "#2980b9", "#2ecc71", "#f1c40f", "#2c3e50", "#7f8c8d","#000004", "#3B0F70", "#8C2981", "#DE4968", "#FE9F6D", "#FCFDBF","#ffb3b3","#66ff33","#00b3b3","#4d4dff","7D8A16")


hchart(USIndJobs,type="column",hcaes(Country, y=total,group=CurrentJobTitleSelect),color=colors) %>%
  hc_title(text="Barplot of Jobs of Country's participants",align="center") %>%
  hc_exporting(enabled=TRUE) %>%
  hc_add_theme(hc_theme_elementary())
#USA INDIA and RUSSIA-Top 3 countries with maximum response X
````

![barplot of Gender](https://github.com/SedeeqAlkhazraji/Exploring-Data-Science/blob/master/Report_Img/10.png)

[Interactive histogram for age of participants](http://rpubs.com/Sedeeq/No10)




Job Satsfaction Map
````
######################
SurveyDf<-read.csv("multipleChoiceResponses.csv",header = TRUE)
worldMap <- map_data(map = "world") 
Survey=SurveyDf
# ALIGN KAGGLE AND WORLDMAP COUNTRY NAMES
Survey$Country = as.character(Survey$Country)
Survey$Country[Survey$Country %in% "United States"] <- "USA"
Survey$Country[Survey$Country %in% "United Kingdom"] <- "UK"
Survey$Country[grepl("China|Hong Kong", Survey$Country)] <- "China"


# processing on JobSatisfaction
# the goal is to calculate the average of the satisfaction, so we need a numeric variable
Survey$JobSatisfaction = as.character(Survey$JobSatisfaction)
Survey$JobSatisfaction[which(Survey$JobSatisfaction == "1 - Highly Dissatisfied")] = 1
Survey$JobSatisfaction[which(Survey$JobSatisfaction == "10 - Highly Satisfied")] = 10
Survey$JobSatisfaction[which(Survey$JobSatisfaction == "I prefer not to share")] = ""

df = Survey %>% 
  dplyr::select(Country,JobSatisfaction) %>% 
  filter(Country != "") %>% 
  filter(JobSatisfaction!= "") %>% 
  mutate(score = as.numeric(JobSatisfaction)) %>% 
  group_by(Country) %>% 
  summarise(mean_satis = mean(score,na.rm = T))

# CREATE THEME FOR WORLDMAP PLOT
theme_worldMap <- theme(
  plot.background = element_rect(fill = "white"),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  panel.background = element_blank(),
  legend.background = element_blank(),
  legend.position = c(0, 0.2),
  legend.justification = c(0, 0),
  legend.title = element_text(colour = "black"),
  legend.text = element_text(colour = "black"),
  legend.key = element_blank(),
  legend.key.size = unit(0.04, "npc"),
  axis.text = element_blank(), 
  axis.title = element_blank(),
  axis.ticks = element_blank()
)

# PLOT WORLDMAP OF RESPONSE RATE
ggplot(df) + 
  geom_map(data = worldMap, 
           aes(map_id = region, x = long, y = lat),
           map = worldMap, fill = "grey") +
  geom_map(aes(map_id = Country, fill = mean_satis),
           map = worldMap, size = 0.3) +
  scale_fill_gradient(low = "blue", high = "red", name = "Satisfaction mean") +
  theme_worldMap +
  labs(title = "Data Scientist Satisfaction around the World 2017") +
  coord_equal() 
````
![barplot of Gender](https://github.com/SedeeqAlkhazraji/Exploring-Data-Science/blob/master/Report_Img/11.png)

[Interactive histogram for age of participants](http://rpubs.com/Sedeeq/No11)


5.	Tools for implementing Data science:	12
6.	ML method:	13
7.	Data Collection and learning:	14
8.	Language Recommendation:	15
9.	Look-Alike Model Using Euclidean Distance:	16
