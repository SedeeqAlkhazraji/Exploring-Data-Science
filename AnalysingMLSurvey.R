require(data.table)
require(highcharter)
require(ggplot2)
require(tidyverse)

setwd("/Users/latlab/Downloads/Kaggle ML and Data Science Survey, 2017/kaggle-survey-2017")

#Read data
SurveyDf<-read.csv("multipleChoiceResponses.csv",header = TRUE)

###############
#1) Part 1- Analyzing country,ethnicity, gender,age,employment status, learning datascience and other related basic features of the survey participants.
###############

#Explor data numarically 
attach(SurveyDf)
table(SurveyDf$GenderSelect)
table(SurveyDf$Country)
summary(na.omit(SurveyDf$Age))
#barplot of Gender
hchart(SurveyDf$GenderSelect,type="bar",name="count",color="green") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Barplot of gender",align="center") %>%
  hc_add_theme(hc_theme_elementary()) 

#barplot of Emp_status
hchart(SurveyDf$EmploymentStatus,type="bar",name="count",color="red") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Barplot of Employment Status",align="center") %>%
  hc_add_theme(hc_theme_elementary()) 

#barplot of country
hchart(SurveyDf$Country,type="bar",name="Count",color="blue") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Barplot of Country or participants",align="center") %>%
  hc_add_theme(hc_theme_elementary()) 


#treemap of top 10  countries  of participant
countryCount<-as.data.frame(table(SurveyDf$Country)) %>%  top_n(10) 
hchart(countryCount,hcaes(Var1,value=Freq,color=Freq),name="Count of participants",type="treemap") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Tree map of top 10 countries of participants",align="center") %>%
  hc_add_theme(hc_theme_elementary()) 

#histogram of age of participants
hchart(na.omit(SurveyDf$Age),name="count",color="orange") %>%
  hc_title(text="Histogram of Ages of the participants",align="center") %>%
  hc_exporting(enabled=TRUE) %>%
  hc_add_theme(hc_theme_elementary())

#Age and Employment status
hcboxplot(x= Age , name="Age of participants", var = EmploymentStatus,color="purple",outliers = FALSE) %>%
  hc_title(text="Boxplot of Ages of the participants and their Employment Status",align="center") %>%
  hc_exporting(enabled=TRUE) %>%
  hc_add_theme(hc_theme_elementary()) %>% 
  hc_chart(type="column") #for vertical box plot

#Let’s see how many are students ?
table(SurveyDf$StudentStatus)#most didn't fill this field
#let's check if participants are learning DS or not
table(SurveyDf$LearningDataScience) #most of them didn't answered this too

hchart(LearningDataScience,name="count",type="column",color="#99FF33") %>%
  hc_title(text="Barplot of Learning Data science field",align="center") %>%
  hc_exporting(enabled=TRUE) %>%
  hc_add_theme(hc_theme_elementary())

#Let’s see how many are Coders
table(CodeWriter)
#so Most of them have entered Yes

hchart(CodeWriter,type="column",name="Count",color="#9645FF") %>%
  hc_title(text="Barplot of Number of Coders",align="center") %>%
  hc_exporting(enabled=TRUE) %>%
  hc_add_theme(hc_theme_elementary())



#let's now check which country has most coders X
#Who all have switched their Carriers and their Current Job titles
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

#let's see the trends countrywise X
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


#Let’s check the where the participants were Employed?
#finding the top 15 employers type
Employer<-as.data.frame(table(CurrentEmployerType)) %>% top_n(15) %>% arrange(desc(Freq))  
Employer[1,]<-NA
names(Employer)<-c("EmployerType","Count")
hchart(na.omit(Employer),type="column",hcaes(x=EmployerType,y=Count),color="#0E2E93") %>%
  hc_title(text="Barplot of top 15 Type of Employers",align="center") %>%
  hc_exporting(enabled=TRUE) %>%
  hc_add_theme(hc_theme_elementary())

EmployerType<-as.data.frame(table(CurrentEmployerType,Country)) %>% top_n(20) %>% arrange(desc(Freq))
#assigning NA values to missing ones
EmployerType[c(1,2,8,12,13,16,19,20),1]<-NA

colnames(EmployerType)<-c("EmployerType","Country","Count")

#plotting data
hchart(na.omit(EmployerType),type="column",hcaes(x=EmployerType, y=Count,group=Country),color=c("#DE4968","#f1c40f","black")) %>%
  hc_title(text="Barplot of Type of Employer and Country",align="center") %>%
  hc_exporting(enabled=TRUE) %>%
  hc_add_theme(hc_theme_elementary())

#Job Satsfaction
######################
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
######################

###############
#1) Part 2- Will mostly aim at analyzing things such as what is the most preferred tool for implementing Datascience, Most used ML method, learning platform,What language they recommend etc and other more specific answers related to datascience and machine learning given by the survey participants.
###############
require(data.table)
require(highcharter)
require(ggplot2)
require(tidyverse)

SurveyDf<-fread("multipleChoiceResponses.csv") #for faster data reading
#SurveyDf<-read.csv("multipleChoiceResponses.csv",header = TRUE)


attach(SurveyDf)
table(MLToolNextYearSelect)
#Let’s start with the most preferred tool for implementing DS and ML.
tooldf<-as.data.frame(table(MLToolNextYearSelect)) %>% arrange(desc(Freq))
#let's remove missing value
tooldf[1,]<-NA
tooldf<-na.omit(tooldf)
names(tooldf)<-c("Tool","Count")
#now let's plot the data
hchart(tooldf,hcaes(x=Tool,y=Count),type="column",name="Count",color="#9B6ED8") %>%  
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Barplot of tools used by participants",align="center") %>%
  hc_add_theme(hc_theme_elementary()) 

#Funnel chart of top 10 most used tools used by survey participants
toptooldf<-na.omit(tooldf) %>% arrange(desc(Count)) %>% top_n(10)
#plotting a funnel chart of top 10 most used tools entered by the users
col <- c("#d35400", "#2980b9", "#2ecc71", "#f1c40f", "#2c3e50", "#7f8c8d","#000004", "#3B0F70", "#8C2981", "#DE4968")

hchart(toptooldf,hcaes(x=Tool,y=Count),type="funnel",name="Count",color=col) %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Funnel chart of top 10 most used tools used by survey participants",align="center")

#Let’s check Top countries uses which tools ?
#dataframe of top 10 countries 
countryCount<-as.data.frame(table(SurveyDf$Country)) %>%  top_n(10)

countryTool<-SurveyDf %>% group_by(MLToolNextYearSelect,Country) %>%
  dplyr::select(Country,MLToolNextYearSelect) %>% 
  filter(Country %in% countryCount$Var1, MLToolNextYearSelect %in% toptooldf$Tool) %>%
  summarise(total_count=n()) %>%
  arrange(desc(total_count))
#Barplot of Top 10 Country grouped by Tool
hchart(countryTool,hcaes(x=Country,y=total_count,group=MLToolNextYearSelect),type="column") %>% 
  hc_title(text="Barplot of Top 10 Country grouped by Tool",align="center") %>%
  hc_exporting(enabled=TRUE) 

#We can notice that Every top 10 country uses Tensorflow, followed by python , then R as their preferred tools for implementing machine learning and data science.
ggplot(aes(x=Country,y=total_count),data=countryTool) +
  geom_col(fill="purple") +
  coord_flip() +
  facet_wrap(~MLToolNextYearSelect)

#Gender vs Most preferred tool
genderTool<-SurveyDf %>% group_by(MLToolNextYearSelect,GenderSelect) %>%
  dplyr::select(GenderSelect,MLToolNextYearSelect) %>% 
  filter(MLToolNextYearSelect %in% toptooldf$Tool,GenderSelect %in% c("Male","Female")) %>%
  summarise(total_count=n()) %>%
  arrange(desc(total_count))

hchart(genderTool,hcaes(x=MLToolNextYearSelect,y=total_count,group=GenderSelect),type="column") %>%   hc_title(text="Gender vs Most preferred tool",align="center") %>%
  hc_exporting(enabled=TRUE) 

#checking the ratio of male to female users for tools-
genderTooldf<-genderTool %>% spread(key = GenderSelect,value=total_count) %>%
  mutate(Percent_male=round((Male/(Male+Female))*100,2),Percent_female=round((Female/(Male+Female))*100,2)) %>%
  arrange()

hchart(genderTooldf,hcaes(x=MLToolNextYearSelect,y=Percent_female),name="Percent",type="column",color="#ED2FAE") %>%   hc_title(text="Percent of female using tools out of 100",align="center") %>%
  hc_exporting(enabled=TRUE) 

#Percent of Male using tools out of 100
hchart(genderTooldf,hcaes(x=MLToolNextYearSelect,y=Percent_male),name="Percent",type="column",color="#F36A20") %>%   hc_title(text="Percent of Male using tools out of 100",align="center") %>%
  hc_exporting(enabled=TRUE)

#Boxplot of Top 3 Ml tools used by participants and their Ages
AgeTooldf<-SurveyDf %>% group_by(MLToolNextYearSelect) %>%
  filter(MLToolNextYearSelect %in% c("TensorFlow","R","Python","Spark / MLlib","Amazon Web services","Hadoop/Hive/Pig","Google Cloud Compute","Jupyter notebooks")) %>% 
  dplyr::select(MLToolNextYearSelect,Age)

hcboxplot(x = AgeTooldf$Age, var = AgeTooldf$MLToolNextYearSelect ,name = "Age", color = "#FF5733",outliers = FALSE) %>%
  hc_chart(type="column") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Boxplot of Top 3 Ml tools used by participants and their Ages",align="center") %>% 
  hc_add_theme(hc_theme_elementary())

#Scatter plot of ML tools used by participants and their Mean Ages
meanAgeTool<-SurveyDf %>% group_by(MLToolNextYearSelect) %>%
  summarise(meanAge=mean(Age,na.rm=T)) %>%      
  arrange(desc(meanAge))

#rounding off the mean ages
meanAgeTool$meanAge<-round(meanAgeTool$meanAge,1)
names(meanAgeTool)<-c("Tool","MeanAge")#renaming the columns


topAgeTool<-meanAgeTool %>% filter(Tool %in% toptooldf$Tool)
col<-c("#d35400", "#2980b9", "#2ecc71", "#f1c40f", "#2c3e50", "#7f8c8d","#000004", "#3B0F70", "#8C2981", "#DE4968")
#let's make a scatterplot

hchart(meanAgeTool,hcaes(x=Tool,y=MeanAge),name="Tool" ,type="scatter",color="#F44E1D") %>% 
  hc_chart(type="column") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Scatter plot of ML tools used by participants and their Mean  
           Ages",align="center") %>% 
  hc_add_theme(hc_theme_elementary())


#Bubble chart of top 10 ML tools used by participants and their Mean Ages
hchart(topAgeTool,hcaes(x=Tool,y=MeanAge,size=MeanAge),color="#7CE3B0",name="Mean Age" ,type="bubble") %>% 
  hc_chart(type="column") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Bubble chart of top 10 ML tools used by participants and their Mean Ages",align="center") %>% 
  hc_add_theme(hc_theme_elementary())

#Let’s analyze the most used ML method?
table(MLMethodNextYearSelect)
#Barplot of ML Method used by participants
Mlmethod<-as.data.frame(table(MLMethodNextYearSelect)) %>% arrange(desc(Freq))

Mlmethod[1,]<-NA
names(Mlmethod)<-c("Method","Count")

hchart(na.omit(Mlmethod),hcaes(x=Method,y=Count),type="column",name="Count",color="#FF5733") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Barplot of ML Method used by participants",align="center") %>%
  hc_add_theme(hc_theme_elementary()) 

#19
#treemap of most preferred methods of ML
hchart(Mlmethod, "treemap", hcaes(x = Method, value = Count,color=Count)) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Treemap of ML Method used by participants",align="center") 

#Boxplot of ML Method used by participants and their Ages
hcboxplot(x = SurveyDf$Age, var =SurveyDf$MLMethodNextYearSelect ,name = "Age", color = "#E127AB",outliers = FALSE) %>%
  hc_chart(type="column") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Boxplot of ML Method used by participants and their Ages",align="center") %>% 
  hc_add_theme(hc_theme_elementary())

  
#Let’s check the countrywise usage of ML methods. I am going to do it for top 3 countries.
countryMethod<-SurveyDf %>% dplyr::select(Country,MLMethodNextYearSelect) %>%
  group_by(Country,MLMethodNextYearSelect) %>%
  filter(Country %in% countryCount$Var1) %>%
  summarize(total_count=n())%>%
  arrange(desc(total_count))

#First for United states
USmethoddf<-countryMethod %>% filter(Country=="United States") %>% arrange(desc(total_count))
USmethoddf[1,]<-NA
hchart(na.omit(USmethoddf),hcaes(x=MLMethodNextYearSelect,y=total_count),type="column",color="#BFD13D") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Barplot of ML Method used by participants in United States",align="center") %>%
  hc_add_theme(hc_theme_elementary()) 

#For India.
Indiamethoddf<-countryMethod %>% filter(Country=="India") %>% arrange(desc(total_count))
Indiamethoddf[1,]<-NA
hchart(na.omit(Indiamethoddf),hcaes(x=MLMethodNextYearSelect,y=total_count),type="column",color="red") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Barplot of ML Method used by participants in India",align="center") %>%
  hc_add_theme(hc_theme_elementary()) 

#For China.
ChinaMethoddf<-countryMethod %>% filter(Country=="People 's Republic of China") %>% arrange(desc(total_count))
ChinaMethoddf[2,]<-NA
hchart(na.omit(ChinaMethoddf),hcaes(x=MLMethodNextYearSelect,y=total_count,color=MLMethodNextYearSelect),type="column") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Barplot of ML Method used by participants in China",align="center") %>%
  hc_add_theme(hc_theme_elementary()) 

#USA,India and China combined.
MethodTopCountry<-countryMethod %>% filter(Country %in% c("United States","India","People 's Republic of China"))
MethodTopCountry[c(1,2,7),]<-NA
hchart(na.omit(MethodTopCountry),hcaes(x=MLMethodNextYearSelect,y=total_count,group=Country),type="column",color=c("#FFA500","#C71585","#00FF00")) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Barplot of ML Method used by top 3 participants Countries",align="center") %>%
  hc_add_theme(hc_theme_elementary()) 

#25
#Let’s check which is the most recommended tool by participants
table(LanguageRecommendationSelect)
toolRecomdf<-as.data.frame(table(LanguageRecommendationSelect)) 
names(toolRecomdf)<-c("tool","count")
toolRecomdf[1,]<-0 
toolRecomdf <- toolRecomdf %>% arrange(desc(count))
hchart(na.omit(toolRecomdf),hcaes(x=tool,y=count),color="#56C1FE",type="column") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Barplot of Recommended tools of participants",align="center") %>%
  hc_add_theme(hc_theme_elementary()) 

#26
#Barplot of Recommended tools of participants
hcboxplot(x = SurveyDf$Age , var = SurveyDf$LanguageRecommendationSelect,name = "Age", color = "#E127AB",outliers = FALSE) %>%
  hc_chart(type="column") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Boxplot of tools recommended by participants and their Ages",align="center") %>% 
  hc_add_theme(hc_theme_elementary())

#Boxplot of tools recommended by participants and their Ages
hcboxplot(x = SurveyDf$Age , var = SurveyDf$LanguageRecommendationSelect,name = "Age", color = "#E127AB",outliers = FALSE) %>%
  hc_chart(type="column") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Boxplot of tools recommended by participants and their Ages",align="center") %>% 
  hc_add_theme(hc_theme_elementary())

#Let’s check from where participants collect data?
publicData<-as.data.frame(table(PublicDatasetsSelect)) %>% arrange(desc(Freq)) %>% top_n(20)
publicData[1,]<-NA
names(publicData)<-c("source",'count')

hchart(na.omit(publicData),hcaes(x=source,y=count),type="column",color="#26BA0B") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="From where participants collect data",align="center") %>%
  hc_add_theme(hc_theme_elementary())  

#20
#Let’s check which is the favourite Platform to learn Data science?
#Let's check for the top 10 learning platforms
platformdf<-SurveyDf %>% group_by(LearningPlatformSelect) %>%
  summarise(count=n()) %>% top_n(20) %>% arrange(desc(count))
platformdf[1,]<-NA

hchart(na.omit(platformdf),hcaes(x=LearningPlatformSelect,y=count),type="column",color="yellow") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Barplot of top 20 platforms used by participants to learn Data science",align="center") %>% 
  hc_add_theme(hc_theme_elementary())  

#21-24
#Pie chart of Question How usefull is Arxiv as learning platform
df<- SurveyDf %>% dplyr::select(LearningPlatformUsefulnessArxiv,LearningPlatformUsefulnessYouTube,LearningPlatformUsefulnessKaggle,LearningPlatformUsefulnessCollege,LearningPlatformUsefulnessCourses) 
df<-df[complete.cases(df),]
# Data
hchart(df$LearningPlatformUsefulnessArxiv, "pie") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Pie chart of Question How usefull is Arxiv as learning platform",align="center") %>% 
  hc_add_theme(hc_theme_elementary())

#Pie chart of Question How usefull are Online courses as learning platform
hchart(df$LearningPlatformUsefulnessCourses, "pie")  %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Pie chart of Question How usefull are Online courses as learning platform",align="center") %>% 
  hc_add_theme(hc_theme_elementary()) 

#Pie chart of Question How usefull us Collage as learning platform
hchart(df$LearningPlatformUsefulnessCollege, "pie")  %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Pie chart of Question How usefull us Collage as learning platform",align="center") %>% 
  hc_add_theme(hc_theme_elementary()) 

#Pie chart of Question How usefull is Kaggle as learning platform
hchart(df$LearningPlatformUsefulnessKaggle, "pie")  %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Pie chart of Question How usefull is Kaggle as learning platform",align="center") %>% 
  hc_add_theme(hc_theme_elementary()) 

#Pie chart of Question How usefull is Youtube as learning platform
hchart(df$LearningPlatformUsefulnessYouTube, "pie") %>%  
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Pie chart of Question How usefull is Youtube as learning platform",align="center") %>% 
  hc_add_theme(hc_theme_elementary()) 


###############
#1) Part 3 - This section will analyze and study the professional lives of the participants, their major degree ,time spend studying data science topics, what job titles they hold,which ML method they actually use in the industries , which bolgs the participants prefer the most for studying data science etc.
###############

blogs<-SurveyDf %>% group_by(BlogsPodcastsNewslettersSelect) %>%
  summarise(count=n()) %>% 
  top_n(15) %>% 
  arrange(desc(count))

#removing NA value
blogs[1,1]<-NA
colnames(blogs)<-c("Blogname","Count")

#Barplot of most preferred blogs for learning
#let's plot them
hchart(na.omit(blogs),hcaes(x=Blogname,y=Count),type="column",color="#062D67") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Barplot of most preferred blogs for learning",align="center") %>%
  hc_add_theme(hc_theme_elementary()) 

table(LearningDataScienceTime)

#Learning Data Science Time
hchart(SurveyDf$LearningDataScienceTime,type="pie",name="count")

#Boxplot of ages and the learning time of participants
hcboxplot(x=SurveyDf$Age,var=SurveyDf$LearningDataScienceTime,outliers = F,color="#09870D",name="Age Distribution") %>%
  hc_chart(type="column")  %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Boxplot of ages and the learning time of participants",align="center") %>%
  hc_add_theme(hc_theme_elementary()) 

#27-
#Let’s now study what participants entered and feel which skills are important for becoming a data scientist?

#let's make a function to ease things
#function takes argument as a dataframe and the categorical variable which we want summarize and group
aggr<-function(df,var) 
{
  require(dplyr)
  var <- enquo(var) #quoting
  dfname<-df %>% 
    group_by_at(vars(!!var)) %>%  ## Group by variables selected by name:
    summarise(count=n()) %>%
    arrange(desc(count))
  
  dfname#function returns a summarized dataframe
  
}

RSkill<-aggr(SurveyDf,JobSkillImportanceR)
RSkill[1,]<-NA
SqlSkill<-aggr(SurveyDf,JobSkillImportanceSQL)
SqlSkill[1,]<-NA
PythonSkill<-aggr(SurveyDf,JobSkillImportancePython)
PythonSkill[1,]<-NA
BigDataSkill<-aggr(SurveyDf,JobSkillImportanceBigData)
BigDataSkill[1,]<-NA
StatsSkill<-aggr(SurveyDf,JobSkillImportanceStats)
StatsSkill[1,]<-NA
DegreeSkill<-aggr(SurveyDf,JobSkillImportanceDegree)
DegreeSkill[1,]<-NA
EnterToolsSkill<-aggr(SurveyDf,JobSkillImportanceEnterpriseTools)
EnterToolsSkill[1,]<-NA
MOOCSkill<-aggr(SurveyDf,JobSkillImportanceMOOC)
MOOCSkill[1,]<-NA
DataVisSkill<-aggr(SurveyDf,JobSkillImportanceVisualizations)
DataVisSkill[1,]<-NA
KaggleRankSkill<-aggr(SurveyDf,JobSkillImportanceKaggleRanking)
KaggleRankSkill[1,]<-NA

#R
hchart(na.omit(RSkill),hcaes(x=JobSkillImportanceR,y=count),type="pie",name="Count") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Piechart of importance of R skill",align="center") %>%
  hc_add_theme(hc_theme_elementary())

#Python
hchart(na.omit(PythonSkill),hcaes(x=JobSkillImportancePython,y=count),type="pie",name="Count") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Piechart of importance of Python skill",align="center") %>%
  hc_add_theme(hc_theme_elementary())

#SQL
hchart(na.omit(SqlSkill),hcaes(x=JobSkillImportanceSQL,y=count),type="pie",name="Count") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Piechart of importance of SQL skill",align="center") %>%
  hc_add_theme(hc_theme_elementary())

#Big Data
hchart(na.omit(BigDataSkill),hcaes(x=JobSkillImportanceBigData,y=count),type="pie",name="Count") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Piechart of importance of Big Data skill",align="center") %>%
  hc_add_theme(hc_theme_elementary())

#Statistic
hchart(na.omit(StatsSkill),hcaes(x=JobSkillImportanceStats,y=count),type="pie",name="Count") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Piechart of importance of Statistics kill",align="center") %>%
  hc_add_theme(hc_theme_elementary())

#Data Visulaization
hchart(na.omit(DataVisSkill),hcaes(x=JobSkillImportanceVisualizations,y=count),type="pie",name="Count") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Piechart of importance of Data Viz skill",align="center") %>%
  hc_add_theme(hc_theme_elementary())

#Importance of Degree
hchart(na.omit(DegreeSkill),hcaes(x=JobSkillImportanceDegree,y=count),type="pie",name="Count") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Piechart of importance of Degree",align="center") %>%
  hc_add_theme(hc_theme_elementary())

#Importance of Enterprise Tools skill
hchart(na.omit(EnterToolsSkill),hcaes(x=JobSkillImportanceEnterpriseTools,y=count),type="pie",name="Count") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Piechart of importance of Enterprise Tools skill",align="center") %>%
  hc_add_theme(hc_theme_elementary())

#Importance of MOOCs
hchart(na.omit(MOOCSkill),hcaes(x=JobSkillImportanceMOOC,y=count),type="pie",name="Count") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Piechart of importance of MOOCs",align="center") %>%
  hc_add_theme(hc_theme_elementary())

#Importance of Kaggle Rankings
hchart(na.omit(KaggleRankSkill),hcaes(x=JobSkillImportanceKaggleRanking,y=count),type="pie",name="Count") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Piechart of importance of Kaggle Rankings",align="center") %>%
  hc_add_theme(hc_theme_elementary())

#What proves that you have good Data science knowledge?
knowlegdeDf<-SurveyDf %>% group_by(ProveKnowledgeSelect) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

knowlegdeDf[1,]<-NA 

hchart(na.omit(knowlegdeDf),hcaes(x=ProveKnowledgeSelect,y=count),type="column",color="#049382",name="count") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Barplot of what proves you have Datascience knowledge",align="center") %>%
  hc_add_theme(hc_theme_elementary())

table(FormalEducation)

#competent ML techniques of participants
Mltechique<-SurveyDf %>% group_by(MLTechniquesSelect) %>%
  summarise(count=n()) %>% 
  arrange(desc(count)) %>%
  top_n(20)

Mltechique[1,]<-NA

hchart(na.omit(Mltechique),hcaes(x=MLTechniquesSelect,y=count),type="column",color="purple",name="count") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Barplot of competent ML techniques of participants",align="center") %>%
  hc_add_theme(hc_theme_elementary())

#Let’s check which Learning algorithm participants use at work ?
MLalgoWork<-SurveyDf %>% group_by(WorkAlgorithmsSelect) %>%
  summarise(count=n()) %>%
  arrange(desc(count)) %>%
  top_n(20)
MLalgoWork[c(1,3),]<-NA


hchart(na.omit(MLalgoWork),hcaes(x=WorkAlgorithmsSelect,y=count),type="column",color="green",name="count") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Barplot of Most used ML algorithms at Work",align="center") %>%
  hc_add_theme(hc_theme_elementary())

#Now let’s check which tools are used most at work?
ToolatWork<-SurveyDf %>% group_by(WorkToolsSelect) %>%
  summarise(count=n()) %>%
  arrange(desc(count)) %>%
  top_n(20)

ToolatWork[c(1),]<-NA


hchart(na.omit(ToolatWork),hcaes(x=WorkToolsSelect,y=count),type="column",color="#7C0E3E",name="count") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Barplot of Most used data science tools used at Work",align="center") %>%
  hc_add_theme(hc_theme_elementary())


#Most used ML method at work?
MethodatWork<-SurveyDf %>% group_by(WorkMethodsSelect
) %>%
  summarise(count=n()) %>%
  arrange(desc(count)) %>%
  top_n(20)

MethodatWork[c(1,3),]<-NA


hchart(na.omit(MethodatWork),hcaes(x=WorkMethodsSelect
                                   ,y=count),type="column",color="#F14B5B",name="count") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Barplot of Most used ML and DS methods used at Work",align="center") %>%
  hc_add_theme(hc_theme_elementary())

