getwd()
setwd("C:/Users/Sagar/Desktop/Interview/Final Project/")
#Importing all the necessary libraries
install.packages("dplyr")
library(dplyr)
install.packages("cowplot")
library(cowplot)
install.packages("leaflet")
library(leaflet)
install.packages("rgdal")
library(rgdal)
install.packages("htmltools")
library(htmltools)
install.packages("ggplot2")
library(ggplot2)
install.packages("ggmap")
library(ggmap)
install.packages("mime")
library(mime)
install.packages("e1071")
library(e1071)
install.packages("cwhmisc")
library(cwhmisc)

#Downloading and storing the data in assignment data
assignmentdata<-read.csv('Center_for_Medicare___Medicaid_Services__CMS____Medicare_Claims_data.csv')
str(assignmentdata)

#Since the indicator names are very big and they were causing an issue at the time of viewing a map,we will reduce the indicator names
assignmentdata$Indicator<-as.character(assignmentdata$Indicator) #Removing the factor level

i=1
N=42640
for (i in 1:N){
  if(assignmentdata$Indicator[i]=="Prevalence of all heart disease hospitalizations among all hospitalizations, US Medicare FFS beneficiaries (65+)"){
    assignmentdata$Indicator[i]<-"Prevalence heart disease hospitalizations % (65+)"
  }else if(assignmentdata$Indicator[i]=="Prevalence of cerebrovascular disease hospitalizations among all hospitalizations, US Medicare FFS beneficiaries (65+)"){
    assignmentdata$Indicator[i]<-"Prevalence cerebrovascular disease hospitalizations % (65+)"
  }else if(assignmentdata$Indicator[i]=="Prevalence of coronary heart disease hospitalizations among all hospitalizations, US Medicare FFS beneficiaries (65+)"){
    assignmentdata$Indicator[i]<-"Prevalence coronary heart disease hospitalizations % (65+)"
  }else if(assignmentdata$Indicator[i]=="Prevalence of heart attack hospitalizations among all hospitalizations, US Medicare FFS beneficiaries (65+)"){
    assignmentdata$Indicator[i]<-"Prevalence heart attack hospitalizations %(65+)"
  }else if(assignmentdata$Indicator[i]=="Prevalence of heart failure hospitalizations among all hospitalizations, US Medicare FFS beneficiaries (65+)"){
    assignmentdata$Indicator[i]<-"Prevalence heart failure hospitalizations % (65+)"
  }else if(assignmentdata$Indicator[i]=="Prevalence of major cardiovascular disease hospitalizations among all hospitalizations, US Medicare FFS beneficiaries (65+)"){
    assignmentdata$Indicator[i]<-"Prevalence major cardiovascular disease hospitalizations % (65+)"
  }else if(assignmentdata$Indicator[i]=="Rate of hospitalizations among adults aged 65 to 74 years with heart failure as the principal diagnosis (among FFS Medicare beneficiaries (65+))"){
    assignmentdata$Indicator[i]<-"hospitalizations aged 65 to 74 years for heart failure (65+)"
  }else if(assignmentdata$Indicator[i]=="Rate of hospitalizations among adults aged 75 to 84 years with heart failure as the principal diagnosis (among FFS Medicare beneficiaries (65+))"){
    assignmentdata$Indicator[i]<-"hospitalizations aged 75 to 84 years for heart failure (65+)"
  }else if(assignmentdata$Indicator[i]=="Rate of hospitalizations among adults aged 85 years and older with heart failure as the principal diagnosis (among FFS Medicare beneficiaries (65+))"){
    assignmentdata$Indicator[i]<-"hospitalizations aged 85 years and older with heart failure"
  }else if(assignmentdata$Indicator[i]=="Rate of hospitalizations among older adults with heart failure as the principal diagnosis (among FFS Medicare beneficiaries (65+))"){
    assignmentdata$Indicator[i]<-"hospitalizations among older adults with heart failure"
  }
  i+1
}

assignmentdata$Indicator<-as.factor(assignmentdata$Indicator)

str(assignmentdata) #Bring Back factor level


assignmentdata1<-assignmentdata[(assignmentdata$LocationDesc!="United States"),]
assignmentdata1<-assignmentdata1[(assignmentdata1$Break_Out=="Overall"),] #Removing United States data and considering only the "Overall values" of the break_out 


shapefile<-readOGR('C:/Users/Sagar/Desktop/Interview/Final Project/shapeFiles/cb_2017_us_state_500k.shp') #Downloading the specfic shapefile. The package 'rgdal' helps to download


#The below code creates lat and long columns by splitting the geolocation column
#For lat function
lat<-function(ross) {
  start<-cpos(ross,"(")
  end<-cpos(ross,",")  
  if(!is.na(start) && !is.na(end)) {
    take_string<-substring(ross,start+1,end-1)  
    return(take_string)
  }
}

assignmentdata1$lat<-apply(assignmentdata1,1,function(x){lat(x['GeoLocation'])})

#For Long function

long<-function(chandler) {
  start<-cpos(chandler,",")
  end<-cpos(chandler,")")  
  if(!is.na(start) && !is.na(end)) {
    take_string1<-substring(chandler,start+1,end-1)  
    return(take_string1)
  }
}

assignmentdata1$long<-apply(assignmentdata1,1,function(x){long(x['GeoLocation'])})

assignmentdata1$lat<-as.numeric(assignmentdata1$lat)
assignmentdata1$long<-as.numeric(assignmentdata1$long) #Convert the lat and long to numeric

#Creating Types
assignmentdata1Type1<-assignmentdata1[(assignmentdata1$Indicator=="Prevalence heart disease hospitalizations % (65+)"),]
assignmentdata1Type2<-assignmentdata1[(assignmentdata1$Indicator=="Prevalence cerebrovascular disease hospitalizations % (65+)"),]
assignmentdata1Type3<-assignmentdata1[(assignmentdata1$Indicator=="Prevalence coronary heart disease hospitalizations % (65+)"),]
assignmentdata1Type4<-assignmentdata1[(assignmentdata1$Indicator=="Prevalence heart attack hospitalizations %(65+)"),]
assignmentdata1Type5<-assignmentdata1[(assignmentdata1$Indicator=="Prevalence heart failure hospitalizations % (65+)"),]
assignmentdata1Type6<-assignmentdata1[(assignmentdata1$Indicator=="Prevalence major cardiovascular disease hospitalizations % (65+)"),]
assignmentdata1Type7<-assignmentdata1[(assignmentdata1$Indicator=="hospitalizations aged 65 to 74 years for heart failure (65+)"),]
assignmentdata1Type8<-assignmentdata1[(assignmentdata1$Indicator=="hospitalizations aged 75 to 84 years for heart failure (65+)"),]
assignmentdata1Type9<-assignmentdata1[(assignmentdata1$Indicator=="hospitalizations aged 85 years and older with heart failure"),]
assignmentdata1Type10<-assignmentdata1[(assignmentdata1$Indicator=="hospitalizations among older adults with heart failure"),]

#Creating Labels for each of the types
assignmentdata1Type1$label<-paste("<p>", assignmentdata1Type1$Data_Value, "</p>",
                                  "<p>", assignmentdata1Type1$Data_Value_Unit,"</p>",
                                  "<p>", assignmentdata1Type1$Year, "</p>")
assignmentdata1Type2$label<-paste("<p>", assignmentdata1Type2$Data_Value, "</p>",
                                  "<p>", assignmentdata1Type2$Data_Value_Unit,"</p>",
                                  "<p>", assignmentdata1Type2$Year, "</p>")
assignmentdata1Type3$label<-paste("<p>", assignmentdata1Type3$Data_Value, "</p>",
                                  "<p>", assignmentdata1Type3$Data_Value_Unit,"</p>",
                                  "<p>", assignmentdata1Type3$Year, "</p>")
assignmentdata1Type4$label<-paste("<p>", assignmentdata1Type4$Data_Value, "</p>",
                                  "<p>", assignmentdata1Type4$Data_Value_Unit,"</p>",
                                  "<p>", assignmentdata1Type4$Year, "</p>")
assignmentdata1Type5$label<-paste("<p>", assignmentdata1Type5$Data_Value, "</p>",
                                  "<p>", assignmentdata1Type5$Data_Value_Unit,"</p>",
                                  "<p>", assignmentdata1Type5$Year, "</p>")
assignmentdata1Type6$label<-paste("<p>", assignmentdata1Type6$Data_Value, "</p>",
                                  "<p>", assignmentdata1Type6$Data_Value_Unit,"</p>",
                                  "<p>", assignmentdata1Type6$Year, "</p>")
assignmentdata1Type7$label<-paste("<p>", assignmentdata1Type7$Data_Value, "</p>",
                                  "<p>", assignmentdata1Type7$Data_Value_Unit,"</p>",
                                  "<p>", assignmentdata1Type7$Year, "</p>")
assignmentdata1Type8$label<-paste("<p>", assignmentdata1Type8$Data_Value, "</p>",
                                  "<p>", assignmentdata1Type8$Data_Value_Unit,"</p>",
                                  "<p>", assignmentdata1Type8$Year, "</p>")
assignmentdata1Type9$label<-paste("<p>", assignmentdata1Type9$Data_Value, "</p>",
                                  "<p>", assignmentdata1Type9$Data_Value_Unit,"</p>",
                                  "<p>", assignmentdata1Type9$Year, "</p>")
assignmentdata1Type10$label<-paste("<p>", assignmentdata1Type10$Data_Value, "</p>",
                                   "<p>", assignmentdata1Type10$Data_Value_Unit,"</p>",
                                   "<p>", assignmentdata1Type10$Year, "</p>")

#Creating Map

map<-leaflet() %>%  
  addProviderTiles(providers$Stamen.Toner) %>%
  setView(lng=-96,lat=37.8,zoom=4) %>%
  addPolygons(data= shapefile,
              color="#660000",
              weight=1,
              smoothFactor=1) %>%
  addCircleMarkers(lat=assignmentdata1Type1$lat,
                   lng=assignmentdata1Type1$long,
                   color="Blue",
                   weight=1,
                   radius=5,
                   group="Prevalence heart disease hospitalizations % (65+)",
                   
                   label=lapply(assignmentdata1Type1$label, HTML),
                   clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)) %>%
  
  addCircleMarkers(lat=assignmentdata1Type2$lat,
                   lng=assignmentdata1Type2$long,
                   color="Blue",
                   weight=1,
                   radius=5,
                   group="Prevalence cerebrovascular disease hospitalizations % (65+)",
                   
                   label=lapply(assignmentdata1Type2$label, HTML),
                   clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)) %>%
  
  addCircleMarkers(lat=assignmentdata1Type3$lat,
                   lng=assignmentdata1Type3$long,
                   color="Blue",
                   weight=1,
                   radius=5,
                   group="Prevalence coronary heart disease hospitalizations % (65+)",
                   
                   label=lapply(assignmentdata1Type3$label, HTML),
                   clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)) %>% 
  addCircleMarkers(lat=assignmentdata1Type4$lat,
                   lng=assignmentdata1Type4$long,
                   color="Blue",
                   weight=1,
                   radius=5,
                   group="Prevalence heart attack hospitalizations %(65+)",
                   
                   label=lapply(assignmentdata1Type4$label, HTML),
                   clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)) %>% 
  addCircleMarkers(lat=assignmentdata1Type5$lat,
                   lng=assignmentdata1Type5$long,
                   color="Blue",
                   weight=1,
                   radius=5,
                   group="Prevalence heart failure hospitalizations % (65+)",
                   
                   label=lapply(assignmentdata1Type5$label, HTML),
                   clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)) %>%  
  addCircleMarkers(lat=assignmentdata1Type6$lat,
                   lng=assignmentdata1Type6$long,
                   color="Blue",
                   weight=1,
                   radius=5,
                   group="Prevalence major cardiovascular disease hospitalizations % (65+)",
                   
                   label=lapply(assignmentdata1Type6$label, HTML),
                   clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)) %>%  
  
  addCircleMarkers(lat=assignmentdata1Type7$lat,
                   lng=assignmentdata1Type7$long,
                   color="Blue",
                   weight=1,
                   radius=5,
                   group="hospitalizations aged 65 to 74 years for heart failure (65+)",
                   
                   label=lapply(assignmentdata1Type7$label, HTML),
                   clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)) %>% 
  addCircleMarkers(lat=assignmentdata1Type8$lat,
                   lng=assignmentdata1Type8$long,
                   color="Blue",
                   weight=1,
                   radius=5,
                   group="hospitalizations aged 75 to 84 years for heart failure (65+)",
                   
                   label=lapply(assignmentdata1Type8$label, HTML),
                   clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)) %>%
  
  addCircleMarkers(lat=assignmentdata1Type9$lat,
                   lng=assignmentdata1Type9$long,
                   color="Blue",
                   weight=1,
                   radius=5,
                   group="hospitalizations aged 85 years and older with heart failure",
                   
                   label=lapply(assignmentdata1Type9$label, HTML),
                   clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)) %>%
  
  addCircleMarkers(lat=assignmentdata1Type10$lat,
                   lng=assignmentdata1Type10$long,
                   color="Blue",
                   weight=1,
                   radius=5,
                   group="hospitalizations among older adults with heart failure",
                   
                   label=lapply(assignmentdata1Type10$label, HTML),
                   clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)) %>%
  
  
  
  
  addLayersControl(overlayGroups = c("Prevalence heart disease hospitalizations % (65+)","Prevalence cerebrovascular disease hospitalizations % (65+)","Prevalence coronary heart disease hospitalizations % (65+)","Prevalence heart attack hospitalizations %(65+)","Prevalence heart failure hospitalizations % (65+)","Prevalence major cardiovascular disease hospitalizations % (65+)","hospitalizations aged 65 to 74 years for heart failure (65+)","hospitalizations aged 75 to 84 years for heart failure (65+)","hospitalizations aged 85 years and older with heart failure","hospitalizations among older adults with heart failure"),
                   options=layersControlOptions(collapsed =FALSE))
map

#This map tells you how one state is doing for the specified indicators over the years

#Example : If select rate of hospitalization for 85 years and older adults, we see that for maryland we see that for

#2005 : 49.7 
#2006 : 47.8 
#2007 : 45.7 
#2008 : 44.4 
#2009 : 46.3 
#2010 : 46.2 
#2011 : 43.7 
#2012 : 41.9 
#2013 : 39.9

#So we see that as years pass, marland is doing better as its 85 years and gretaer elders having heart failures are becoming less.

#We thus can see for all the indicators and all the states by selecting our choice on map. It will also help us compare with other states and come to conclusions as compared to others what indicator will reduce or increase over years.


#Finding the prominent illness on the basis of Break_Out
head(assignmentdata)

#Checking whether all the Topics have same unit for Data Value
ggplot(assignmentdata,aes(x=Topic, fill=Data_Value_Unit))+geom_bar(position = 'dodge')

#From the above graph it can be clearly seen that the unit for data value is different for Heart Failure. So it cannot be included in the overall consideration

#Considering all topics expect Heart Failure to find which of those is prominent

#1.Women
#(I) Checking which illness is prominent in women

#Creating a Table containing aggregate values of the topics for the years 2004, 2005, 2012 and 2013

#for year 2004
y2004.w <- assignmentdata[assignmentdata$Break_Out == 'Female'& assignmentdata$Year == 2004 & assignmentdata$Topic != 'Heart Failure',]
y2004.w.new <- aggregate(y2004.w[,14],list(y2004.w$Topic), mean)

#for year 2005
y2005.w <- assignmentdata[assignmentdata$Break_Out == 'Female'& assignmentdata$Year == 2005 & assignmentdata$Topic != 'Heart Failure',]
y2005.w.new <- aggregate(y2005.w[,14],list(y2005.w$Topic), mean)

#for year 2012
y2012.w <- assignmentdata[assignmentdata$Break_Out == 'Female'& assignmentdata$Year == 2012 & assignmentdata$Topic != 'Heart Failure',]
y2012.w.new <- aggregate(y2012.w[,14],list(y2012.w$Topic), mean)

#for year 2013
y2013.w <- assignmentdata[assignmentdata$Break_Out == 'Female'& assignmentdata$Year == 2013 & assignmentdata$Topic != 'Heart Failure',]
y2013.w.new <- aggregate(y2013.w[,14],list(y2013.w$Topic), mean)


df.w <-  data.frame(y2004.w.new,y2005.w.new[,2],y2012.w.new[,2],y2013.w.new[,2])
names(df.w) <- c('Topic','AggVal_2004','AggVal_2005','AggVal_2012','AggVal_2013')
df.w

#From the above table it can be seen that the Major Cardiovascular Disease is more prominent in Women. Also, the percent of women suffering from all the illness has considerably decreased from 2004 till 2013, which shows increase in medical awareness
#Graphical Representation of the above table:
ggplot(data = df.w,aes(x=Topic))+ geom_line( aes(y= AggVal_2004, group = 1, color ='2004'), size= 1)+ geom_line( aes(y= AggVal_2005, group = 1, color ='2005'), size= 1)+ geom_line(aes(y= AggVal_2012, color ='2012', group = 1), size= 1)+ geom_line(aes(y=AggVal_2013,color ='2013', group = 1),  size= 1)+labs(title = 'Females : Distribution of Topic for different Years. ', x='Topics', y = 'Aggregate Values for Years') + scale_color_manual(name ='Years', values= c('2004'='darkgreen','2005'='blue','2012'='red','2013'='darkgoldenrod1'))
#The graph shows that number of women suffering from Acute Myocardial Infraction is least and that suffering from Major Cardiovascular Disease are more. It also shows the decrease in the percentage of people suffering from these diseases in 2013 as compared to that in 2004


# Finding Top 5 states in USA where women suffering from Major Cardiovascular disease is prominent for the year 2004,2005,2012 and 2013

# for 2004
y2004.w <- y2004.w[y2004.w$Topic == 'Major Cardiovascular Disease',]
Top.2004.w <- y2004.w[order(y2004.w$Data_Value, decreasing = T),]
t.2004.w <- head(data.frame(unique(Top.2004.w$LocationDesc),Top.2004.w$Data_Value),5)
names(t.2004.w) <- c('States','DataValue')

# for 2005
y2005.w <- y2005.w[y2005.w$Topic == 'Major Cardiovascular Disease',]
Top.2005.w <- y2005.w[order(y2005.w$Data_Value, decreasing = T),]
t.2005.w <- head(data.frame(unique(Top.2005.w$LocationDesc),Top.2005.w$Data_Value),5)
names(t.2005.w) <- c('States','DataValue')

# for 2012
y2012.w <- y2012.w[y2012.w$Topic == 'Major Cardiovascular Disease',]
Top.2012.w <- y2012.w[order(y2012.w$Data_Value, decreasing = T),]
t.2012.w <- head(data.frame(unique(Top.2012.w$LocationDesc),Top.2012.w$Data_Value),5)
names(t.2012.w) <- c('States','DataValue')

# for 2013
y2013.w <- y2013.w[y2004.w$Topic == 'Major Cardiovascular Disease',]
Top.2013.w <- y2013.w[order(y2013.w$Data_Value, decreasing = T),]
t.2013.w <- head(data.frame(unique(Top.2013.w$LocationDesc),Top.2013.w$Data_Value),5)
names(t.2013.w) <- c('States','DataValue')


p.w.1 <- ggplot(t.2004.w, aes(x=States,y=DataValue,fill=States))+ geom_bar(stat = 'identity', color='black')+ geom_text(aes(label=DataValue),position = position_dodge(0.9),color = 'black',vjust=-0.2,hjust = -0.2, size = 5) + labs(title = 'Top 5 States having Women with Major Cardiovascular Disease for 2004', x = 'States', y = 'Data Value')+ theme(axis.title.x = element_text(size=10),axis.title.y = element_text(size=10),legend.title = element_text(size = 10), legend.text = element_text(size = 10)) + theme(plot.title = element_text(size= 10))+theme(axis.text.x = element_text(size=10),axis.text.y = element_text(size = 10))+ coord_flip()

p.w.2 <- ggplot(t.2005.w, aes(x=States,y=DataValue,fill=States))+ geom_bar(stat = 'identity', color='black')+ geom_text(aes(label=DataValue),position = position_dodge(0.9),color = 'black',vjust=-0.2,hjust = -0.2, size = 5) + labs(title = 'Top 5 States having Women with Major Cardiovascular Disease for 2005', x = 'States', y = 'Data Value')+ theme(axis.title.x = element_text(size=10),axis.title.y = element_text(size=10),legend.title = element_text(size = 10), legend.text = element_text(size = 10)) + theme(plot.title = element_text(size= 10))+theme(axis.text.x = element_text(size=10),axis.text.y = element_text(size = 10))+ coord_flip()

p.w.3 <- ggplot(t.2012.w, aes(x=States,y=DataValue,fill=States))+ geom_bar(stat = 'identity', color='black')+ geom_text(aes(label=DataValue),position = position_dodge(0.9),color = 'black',vjust=-0.2,hjust = -0.2, size = 5) + labs(title = 'Top 5 States having Women with Major Cardiovascular Disease for 2012', x = 'States', y = 'Data Value')+ theme(axis.title.x = element_text(size=10),axis.title.y = element_text(size=10),legend.title = element_text(size = 10), legend.text = element_text(size = 10)) + theme(plot.title = element_text(size= 10))+theme(axis.text.x = element_text(size=10),axis.text.y = element_text(size = 10))+ coord_flip()

p.w.4 <- ggplot(t.2013.w, aes(x=States,y=DataValue,fill=States))+ geom_bar(stat = 'identity', color='black')+ geom_text(aes(label=DataValue),position = position_dodge(0.9),color = 'black',vjust=-0.2,hjust = -0.2, size = 5) + labs(title = 'Top 5 States having Women with Major Cardiovascular Disease for 2013', x = 'States', y = 'Data Value')+ theme(axis.title.x = element_text(size=10),axis.title.y = element_text(size=10),legend.title = element_text(size = 10), legend.text = element_text(size = 10)) + theme(plot.title = element_text(size= 10))+theme(axis.text.x = element_text(size=10),axis.text.y = element_text(size = 10))+ coord_flip()

plot_grid(p.w.1,p.w.2,p.w.3,p.w.4)


#From all the four plots it can be seen that NewJersey and West Virginia have constantly been amongst the top 5 states where women are suffering from Major Cardiovascular Disease in all the 4 years. As compared to 2004, the women suffering from Major Cardiovascular disease have reduced in 2013. In 2004 the maximum was 26.7 where as in 2013 the maximum was 24 which is good reduction.


#Finding Bottom 5 states in USA where women suffering from Major Cardiovascular disease is prominent for the year 2004,2005,2012 and 2013
# for 2004
y2004.w <- y2004.w[y2004.w$Topic == 'Major Cardiovascular Disease',]
Bot.2004.w <- y2004.w[order(y2004.w$Data_Value, decreasing = F),]
b.2004.w <- head(data.frame(unique(Bot.2004.w$LocationDesc),Bot.2004.w$Data_Value),5)
names(b.2004.w) <- c('States','DataValue')

# for 2005
y2005.w <- y2005.w[y2005.w$Topic == 'Major Cardiovascular Disease',]
Bot.2005.w <- y2005.w[order(y2005.w$Data_Value, decreasing = F),]
b.2005.w <- head(data.frame(unique(Bot.2005.w$LocationDesc),Bot.2005.w$Data_Value),5)
names(b.2005.w) <- c('States','DataValue')

# for 2012
y2012.w <- y2012.w[y2012.w$Topic == 'Major Cardiovascular Disease',]
Bot.2012.w <- y2012.w[order(y2012.w$Data_Value, decreasing = F),]
b.2012.w <- head(data.frame(unique(Bot.2012.w$LocationDesc),Bot.2012.w$Data_Value),5)
names(b.2012.w) <- c('States','DataValue')

# for 2013
y2013.w <- y2013.w[y2013.w$Topic == 'Major Cardiovascular Disease',]
Bot.2013.w <- y2013.w[order(y2013.w$Data_Value, decreasing = F),]
b.2013.w <- head(data.frame(unique(Bot.2013.w$LocationDesc),Bot.2013.w$Data_Value),5)
names(b.2013.w) <- c('States','DataValue')

#plots.....
p.w.5 <- ggplot(b.2004.w, aes(x=States,y=DataValue,fill=States))+ geom_bar(stat = 'identity', color='black')+ geom_text(aes(label=DataValue),position = position_dodge(0.9),color = 'black',vjust=-0.2,hjust = -0.2, size = 5) + labs(title = 'Bottom 5 States having Women with Major Cardiovascular Disease for 2004', x = 'States', y = 'Data Value')+ theme(axis.title.x = element_text(size=10),axis.title.y = element_text(size=10),legend.title = element_text(size = 10), legend.text = element_text(size = 10)) + theme(plot.title = element_text(size= 10))+theme(axis.text.x = element_text(size=10),axis.text.y = element_text(size = 10))+ coord_flip()

p.w.6 <- ggplot(b.2005.w, aes(x=States,y=DataValue,fill=States))+ geom_bar(stat = 'identity', color='black')+ geom_text(aes(label=DataValue),position = position_dodge(0.9),color = 'black',vjust=-0.2,hjust = -0.2, size = 5) + labs(title = 'Bottom 5 States having Women with Major Cardiovascular Disease for 2005', x = 'States', y = 'Data Value')+ theme(axis.title.x = element_text(size=10),axis.title.y = element_text(size=10),legend.title = element_text(size = 10), legend.text = element_text(size = 10)) + theme(plot.title = element_text(size= 10))+theme(axis.text.x = element_text(size=10),axis.text.y = element_text(size = 10))+ coord_flip()

p.w.7 <- ggplot(b.2012.w, aes(x=States,y=DataValue,fill=States))+ geom_bar(stat = 'identity', color='black')+ geom_text(aes(label=DataValue),position = position_dodge(0.9),color = 'black',vjust=-0.2,hjust = -0.2, size = 5) + labs(title = 'Bottom 5 States having Women with Major Cardiovascular Disease for 2012', x = 'States', y = 'Data Value')+ theme(axis.title.x = element_text(size=10),axis.title.y = element_text(size=10),legend.title = element_text(size = 10), legend.text = element_text(size = 10)) + theme(plot.title = element_text(size= 10))+theme(axis.text.x = element_text(size=10),axis.text.y = element_text(size = 10))+ coord_flip()

p.w.8 <- ggplot(b.2013.w, aes(x=States,y=DataValue,fill=States))+ geom_bar(stat = 'identity', color='black')+ geom_text(aes(label=DataValue),position = position_dodge(0.9),color = 'black',vjust=-0.2,hjust = -0.2, size = 5) + labs(title = 'Bottom 5 States having Women with Major Cardiovascular Disease for 2013', x = 'States', y = 'Data Value')+ theme(axis.title.x = element_text(size=10),axis.title.y = element_text(size=10),legend.title = element_text(size = 10), legend.text = element_text(size = 10)) + theme(plot.title = element_text(size= 10))+theme(axis.text.x = element_text(size=10),axis.text.y = element_text(size = 10))+ coord_flip()

plot_grid(p.w.5,p.w.6,p.w.7,p.w.8)
#From all the four plots it can be seen that Nevada, Utah and Colorado have constantly been amongst the bottom 5 states where women are suffering from Major Cardiovascular Disease in all the 4 years. As compared to 2004, the women suffering from Major Cardiovascular disease have reduced in 2013. In 2004 the minimum was 19.4 where as in 2013 the minimum was 17 which is good reduction

#Similarly we can do the same for other break_out categories

#Since we could not work on the Topic "Heart Failure" above because the data type, we will work by considering it's indicators
#1. Prevalence of heart failure hospitalizations among all hospitalizations, US Medicare FFS beneficiaries (65+)
#2. Rate of hospitalizations among adults aged 65 to 74 years with heart failure as the principal diagnosis (among FFS Medicare beneficiaries (65+))
#3. Rate of hospitalizations among adults aged 75 to 84 years with heart failure as the principal diagnosis (among FFS Medicare beneficiaries (65+))
#4. Rate of hospitalizations among adults aged 85 years and older with heart failure as the principal diagnosis (among FFS Medicare beneficiaries (65+))

#Working on the first indicator mentioned above

Heartfailuredata<-read.csv('Center_for_Medicare___Medicaid_Services__CMS____Medicare_Claims_data.csv') #Reading the file again and saving in the dataset named Heartfailuredata

#After reading the files, we prepare a subset of data containing its Toipc as "Heart Failure" and a seperate data that contains only the United States data of the produced subset.
HeartfailureUS<-Heartfailuredata[(Heartfailuredata$LocationDesc=="United States" & Heartfailuredata$Topic=="Heart Failure"),]

Heartfailuredata<-Heartfailuredata[(Heartfailuredata$Topic=="Heart Failure"),]
Heartfailuredata1st<-Heartfailuredata[(Heartfailuredata$Indicator=="Prevalence of heart failure hospitalizations among all hospitalizations, US Medicare FFS beneficiaries (65+)" &Heartfailuredata$LocationDesc!="United States"),]

##Working by break_out

#Men
Heartfailuredata1stmen<-Heartfailuredata1st[(Heartfailuredata1st$Break_Out=="Male"),]
Chandler<-aggregate(Heartfailuredata1stmen[, 14], list(Heartfailuredata1stmen$Year), mean)

men<-ggplot(data=Chandler,aes(x=Group.1,y=x)) + 
  geom_step(color="red") +
  xlab("Years") + ylab("Average percentage by Years") +
  theme(axis.title.x= element_text(colour="Dark Green", size=10),
        axis.title.y= element_text(colour="Blue", size=10),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.title= element_text(size=5),
        legend.text =  element_text(size=10),
        plot.title=element_text(size=7,face="bold")) +
  ggtitle("Prevalence heart failure hospitalizations % for men")

#Women
Heartfailuredata1stwomen<-Heartfailuredata1st[(Heartfailuredata1st$Break_Out=="Female"),]

Chandler1<-aggregate(Heartfailuredata1stwomen[, 14], list(Heartfailuredata1stwomen$Year), mean)

women<-ggplot(data=Chandler1,aes(x=Group.1,y=x)) + 
  geom_step(color="Green") +
  xlab("Years") + ylab("Average percentage by Years") +
  theme(axis.title.x= element_text(colour="Dark Green", size=10),
        axis.title.y= element_text(colour="Blue", size=10),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.title= element_text(size=5),
        legend.text =  element_text(size=10),
        plot.title=element_text(size=7,face="bold")) +
  ggtitle("Prevalence heart failure hospitalizations % for women")

#65+ Group
Heartfailuredata1st65<-Heartfailuredata1st[(Heartfailuredata1st$Break_Out=="65+"),]
Chandler2<-aggregate(Heartfailuredata1st65[, 14], list(Heartfailuredata1st65$Year), mean)

sixtyfiveplus<-ggplot(data=Chandler2,aes(x=Group.1,y=x)) + 
  geom_step(color="Maroon") +
  xlab("Years") + ylab("Average percentage by Years") +
  theme(axis.title.x= element_text(colour="Dark Green", size=10),
        axis.title.y= element_text(colour="Blue", size=10),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.title= element_text(size=5),
        legend.text =  element_text(size=10),
        plot.title=element_text(size=7,face="bold")) +
  ggtitle("Prevalence heart failure hospitalizations % for 65+")

#75+ Group
Heartfailuredata1st75<-Heartfailuredata1st[(Heartfailuredata1st$Break_Out=="75+"),]
Chandler3<-aggregate(Heartfailuredata1st75[, 14], list(Heartfailuredata1st75$Year), mean)

seventyfiveplus<-ggplot(data=Chandler3,aes(x=Group.1,y=x)) + 
  geom_step(color="Orange") +
  xlab("Years") + ylab("Average percentage by Years") +
  theme(axis.title.x= element_text(colour="Dark Green", size=10),
        axis.title.y= element_text(colour="Blue", size=10),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.title= element_text(size=5),
        legend.text =  element_text(size=10),
        plot.title=element_text(size=7,face="bold")) +
  ggtitle("Prevalence heart failure hospitalizations % for 75+")

#Non-Hispanic Whites
Heartfailuredata1stNonHispanicWhite<-Heartfailuredata1st[(Heartfailuredata1st$Break_Out=="Non-Hispanic White"),]
Chandler4<-aggregate(Heartfailuredata1stNonHispanicWhite[, 14], list(Heartfailuredata1stNonHispanicWhite$Year), mean)

NonHispanicWhite<-ggplot(data=Chandler4,aes(x=Group.1,y=x)) + 
  geom_step(color="Purple") +
  xlab("Years") + ylab("Average percentage by Years") +
  theme(axis.title.x= element_text(colour="Dark Green", size=10),
        axis.title.y= element_text(colour="Blue", size=10),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.title= element_text(size=5),
        legend.text =  element_text(size=10),
        plot.title=element_text(size=7,face="bold")) +
  ggtitle("Prevalence heart failure hospitalizations % for Non-Hispanic White")

#Non-Hispanic Blacks
HeartfailureUSNonHispanicBlack<-HeartfailureUS[(HeartfailureUS$Break_Out=="Non-Hispanic Black" & HeartfailureUS$Indicator=="Prevalence of heart failure hospitalizations among all hospitalizations, US Medicare FFS beneficiaries (65+)"),]

NonHispanicBlack<-ggplot(data=HeartfailureUSNonHispanicBlack,aes(x=Year,y=Data_Value)) + 
  geom_step(color="Pink") +
  xlab("Years") + ylab("Average percentage by Years") +
  theme(axis.title.x= element_text(colour="Dark Green", size=10),
        axis.title.y= element_text(colour="Blue", size=10),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.title= element_text(size=5),
        legend.text =  element_text(size=10),
        plot.title=element_text(size=7,face="bold")) +
  ggtitle("Prevalence heart failure hospitalizations % for Non-Hispanic Black")

#Hispanics
HeartfailureUSHispanic<-HeartfailureUS[(HeartfailureUS$Break_Out=="Hispanic" & HeartfailureUS$Indicator=="Prevalence of heart failure hospitalizations among all hospitalizations, US Medicare FFS beneficiaries (65+)"),]

Hispanic<-ggplot(data=HeartfailureUSHispanic,aes(x=Year,y=Data_Value)) + 
  geom_step(color="Black") +
  xlab("Years") + ylab("Average percentage by Years") +
  theme(axis.title.x= element_text(colour="Dark Green", size=10),
        axis.title.y= element_text(colour="Blue", size=10),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.title= element_text(size=5),
        legend.text =  element_text(size=10),
        plot.title=element_text(size=7,face="bold")) +
  ggtitle("Prevalence heart failure hospitalizations % for Hispanic")

#Others
Heartfailuredata1stother<-Heartfailuredata1st[(Heartfailuredata1st$Break_Out=="Other"),]
Chandler7<-aggregate(Heartfailuredata1stother[, 14], list(Heartfailuredata1stother$Year), mean)

Other<-ggplot(data=Chandler7,aes(x=Group.1,y=x)) + 
  geom_step(color="grey") +
  xlab("Years") + ylab("Average percentage by Years") +
  theme(axis.title.x= element_text(colour="Dark Green", size=10),
        axis.title.y= element_text(colour="Blue", size=10),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.title= element_text(size=5),
        legend.text =  element_text(size=10),
        plot.title=element_text(size=7,face="bold")) +
  ggtitle("Prevalence heart failure hospitalizations % for Other")

#Plotting the graph for all the topics:
plot_grid(men,women,sixtyfiveplus,seventyfiveplus,NonHispanicWhite,NonHispanicBlack,Hispanic,Other)

#Similarly we can work with the other indicators of topic 'Heart Failure'

#We plan to divide the states by Region
#We plan to created four regions 1. Northeast 2. Midwest 3. South 4. West

dataset<-read.csv('Center_for_Medicare___Medicaid_Services__CMS____Medicare_Claims_data.csv') #Reading the file again and saving in the dataset

datasetwihtoutUS<-dataset[(dataset$LocationDesc!="United States"),] #Removing United States Data

#The below code assigns a region to the States

i=1
N=41820
for (i in 1:N){
  if(datasetwihtoutUS$LocationDesc[i]=="Connecticut" | datasetwihtoutUS$LocationDesc[i]=="Maine" |datasetwihtoutUS$LocationDesc[i]=="New Hampshire" | datasetwihtoutUS$LocationDesc[i]=="Rhode Island"| datasetwihtoutUS$LocationDesc[i]=="Vermont" | datasetwihtoutUS$LocationDesc[i]=="New Jersey" | datasetwihtoutUS$LocationDesc[i]=="New York" | datasetwihtoutUS$LocationDesc[i]=="Pennsylvania"){
    
    datasetwihtoutUS$Region[i]<-"Northeast"
    
  }else if(datasetwihtoutUS$LocationDesc[i]=="Illinois" | datasetwihtoutUS$LocationDesc[i]=="Indiana"| datasetwihtoutUS$LocationDesc[i]=="Michigan" | datasetwihtoutUS$LocationDesc[i]=="Ohio" | datasetwihtoutUS$LocationDesc[i]=="Wisconsin" |datasetwihtoutUS$LocationDesc[i]=="Iowa"| datasetwihtoutUS$LocationDesc[i]=="Kansas" | datasetwihtoutUS$LocationDesc[i]=="Minnesota" | datasetwihtoutUS$LocationDesc[i]=="Missouri" | datasetwihtoutUS$LocationDesc[i]=="Nebraska"  | datasetwihtoutUS$LocationDesc[i]=="North Dakota" | datasetwihtoutUS$LocationDesc[i]=="South Dakota"){
    datasetwihtoutUS$Region[i]<-"Midwest"
  }else if (datasetwihtoutUS$LocationDesc[i]=="Delaware" | datasetwihtoutUS$LocationDesc[i]=="Florida" | datasetwihtoutUS$LocationDesc[i]=="Georgia" | datasetwihtoutUS$LocationDesc[i]=="Maryland" | datasetwihtoutUS$LocationDesc[i]=="North Carolina" | datasetwihtoutUS$LocationDesc[i]=="South Carolina"|datasetwihtoutUS$LocationDesc[i]=="Virginia"| datasetwihtoutUS$LocationDesc[i]=="Washington, DC"| datasetwihtoutUS$LocationDesc[i]=="West Virginia"|datasetwihtoutUS$LocationDesc[i]=="Alabama"|datasetwihtoutUS$LocationDesc[i]=="Kentucky"|datasetwihtoutUS$LocationDesc[i]=="Mississippi"|datasetwihtoutUS$LocationDesc[i]=="Tennessee"|datasetwihtoutUS$LocationDesc[i]=="Arkansas"|datasetwihtoutUS$LocationDesc[i]=="Texas"|datasetwihtoutUS$LocationDesc[i]=="Oklahoma"|datasetwihtoutUS$LocationDesc[i]=="Louisiana"){
    
    datasetwihtoutUS$Region[i]<-"South"
  }else if (datasetwihtoutUS$LocationDesc[i]=="Arizona"|datasetwihtoutUS$LocationDesc[i]=="Wyoming"|datasetwihtoutUS$LocationDesc[i]=="Utah"|datasetwihtoutUS$LocationDesc[i]=="Colorado"|datasetwihtoutUS$LocationDesc[i]=="New Mexico"|datasetwihtoutUS$LocationDesc[i]=="Idaho"| datasetwihtoutUS$LocationDesc[i]=="Montana"| datasetwihtoutUS$LocationDesc[i]=="Nevada"|datasetwihtoutUS$LocationDesc[i]=="Alaska"|datasetwihtoutUS$LocationDesc[i]=="California"|datasetwihtoutUS$LocationDesc[i]=="Hawaii"|datasetwihtoutUS$LocationDesc[i]=="Oregon"|datasetwihtoutUS$LocationDesc[i]=="Washington"){
    datasetwihtoutUS$Region[i]<-"West"
  }
  i+1
}

datasetwihtoutUS$Region<-as.factor(datasetwihtoutUS$Region) #Making the new variable a factor


#We select one region and one topic for example
#We selected Major Cardiovascular disease
majorcardio<-datasetwihtoutUS[(datasetwihtoutUS$Topic=="Major Cardiovascular Disease" & datasetwihtoutUS$Region=="Northeast"),]
majorcardio<-majorcardio[(majorcardio$Break_Out!="Overall"),]

#Working on all the breakout_categories

#(A) Men

majorcardiomale<-majorcardio[(majorcardio$Break_Out=="Male"),]

Chandler1<-aggregate(majorcardiomale[, 14], list(majorcardiomale$Year), mean)

m1<-ggplot(data=Chandler1,aes(x=Group.1,y=x)) + 
  geom_step(color="red") +
  xlab("Years") + ylab("Average percentage by Years") +
  theme(axis.title.x= element_text(colour="Dark Green", size=10),
        axis.title.y= element_text(colour="Blue", size=10),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        
        plot.title=element_text(size=7,face="bold")) +
  ggtitle("Prevalence major cardio hospitalization for men (Northeast)")

#(B) Women

majorcardiofemale<-majorcardio[(majorcardio$Break_Out=="Female"),]

Chandler2<-aggregate(majorcardiofemale[, 14], list(majorcardiofemale$Year), mean)

m2<-ggplot(data=Chandler2,aes(x=Group.1,y=x)) + 
  geom_step(color="pink") +
  xlab("Years") + ylab("Average percentage by Years") +
  theme(axis.title.x= element_text(colour="Dark Green", size=10),
        axis.title.y= element_text(colour="Blue", size=10),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        
        plot.title=element_text(size=7,face="bold")) +
  ggtitle("Prevalence major cardio hospitalization for female (Northeast)")

#(C) 65+ Group

majorcardiosf<-majorcardio[(majorcardio$Break_Out=="65+"),]

Chandler3<-aggregate(majorcardiosf[, 14], list(majorcardiosf$Year), mean)

m4<-ggplot(data=Chandler3,aes(x=Group.1,y=x)) + 
  geom_step(color="green") +
  xlab("Years") + ylab("Average percentage by Years") +
  theme(axis.title.x= element_text(colour="Dark Green", size=10),
        axis.title.y= element_text(colour="Blue", size=10),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        
        plot.title=element_text(size=7,face="bold")) +
  ggtitle("Prevalence major cardio hospitalization for +65 (Northeast)")

#(D) 75+ Group

majorcardiosevenf<-majorcardio[(majorcardio$Break_Out=="75+"),]

Chandler4<-aggregate(majorcardiosevenf[, 14], list(majorcardiosevenf$Year), mean)

m5<-ggplot(data=Chandler4,aes(x=Group.1,y=x)) + 
  geom_step(color="purple") +
  xlab("Years") + ylab("Average percentage by Years") +
  theme(axis.title.x= element_text(colour="Dark Green", size=10),
        axis.title.y= element_text(colour="Blue", size=10),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        
        plot.title=element_text(size=7,face="bold")) +
  ggtitle("Prevalence major cardio hospitalization for +75 (Northeast)")

#(E) Non-Hispanic Whites

majorcardionhw<-majorcardio[(majorcardio$Break_Out=="Non-Hispanic White"),]

Chandler7<-aggregate(majorcardionhw[, 14], list(majorcardionhw$Year), mean)

m8<-ggplot(data=Chandler7,aes(x=Group.1,y=x)) + 
  geom_step(color="green") +
  xlab("Years") + ylab("Average percentage by Years") +
  theme(axis.title.x= element_text(colour="Dark Green", size=10),
        axis.title.y= element_text(colour="Blue", size=10),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        
        plot.title=element_text(size=7,face="bold")) +
  ggtitle("Prevalence major cardio hospitalization for Non Hispanic White (Northeast)")

#(F) Non-Hispanic Blacks

majorcardionhb<-majorcardio[(majorcardio$Break_Out=="Non-Hispanic Black"),]

Chandler6<-aggregate(majorcardionhb[, 14], list(majorcardionhb$Year), mean)

m7<-ggplot(data=Chandler6,aes(x=Group.1,y=x)) + 
  geom_step(color="purple") +
  xlab("Years") + ylab("Average percentage by Years") +
  theme(axis.title.x= element_text(colour="Dark Green", size=10),
        axis.title.y= element_text(colour="Blue", size=10),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        
        plot.title=element_text(size=7,face="bold")) +
  ggtitle("Prevalence major cardio hospitalization for Non Hispanic Black (Northeast)")

#(G) Others

majorcardioother<-majorcardio[(majorcardio$Break_Out=="Other"),]

Chandler8<-aggregate(majorcardioother[, 14], list(majorcardioother$Year), mean)

m9<-ggplot(data=Chandler8,aes(x=Group.1,y=x)) + 
  geom_step(color="blue") +
  xlab("Years") + ylab("Average percentage by Years") +
  theme(axis.title.x= element_text(colour="Dark Green", size=10),
        axis.title.y= element_text(colour="Blue", size=10),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        
        plot.title=element_text(size=7,face="bold")) +
  ggtitle("Prevalence major cardio hospitalization for Other(Northeast)")

#Plotting the graph for all the topics:
  
  plot_grid(m1,m2,m4,m7,m5,m8,m9)

#Similarly we can work on other Topics for other Regions (Midwest, South, West)
  
#All the data we worked was State-wise. We can similary create a map that we created above only for United States data