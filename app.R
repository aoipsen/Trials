data <- read.csv(file="C:/Users/Ipsen/Documents/Git/TrialData/clinical_trials.csv",head=TRUE,encoding = "UTF-8")


library(highcharter) 
library(dplyr)
library(ggplot2)
library(stringr)
library(purrr)
library(plotly)
library(anytime)
library(lubridate)



highchart() %>%
  hc_chart(type="column") %>%
  hc_add_series(data$Enrollment,ShowinLegend=FALSE)

data$logEnrollment <- log(data$Enrollment,base=10)
highchart() %>%
  hc_chart(type="column") %>%
  hc_add_series(data$logEnrollment,ShowinLegend=FALSE)

PhasesEnr <- data %>% group_by(Phases) %>%
          summarize(Enroll=sum(Enrollment,na.rm=TRUE))

hchart() %>%
  hc_chart(type="column") %>%
  hc_xAxis(categories=PhasesEnr$Enroll) %>%
  hc_add_series(hcaes(x=PhasesEnr$Phases, y=PhasesEnr$Enroll),Name="Enrollment",ShowinLegend=FALSE)
  highchart() %>%
    hc_chart(type="column") %>%
    hc_add_series(data$logEnrollment,ShowinLegend=FALSE)
  
  #Chart of enrollment by phases.
  data %>% group_by(Phases) %>%
    summarize(Enroll=sum(Enrollment,na.rm=TRUE)) %>% arrange(Enroll) %>%
  hchart(type="column", hcaes(x=Phases,y=Enroll))
  
## Update Date vaiables 
  data$Start.Date <- anytime( data$Start.Date)  
  data$Last.Update.Posted<- anytime( data$Last.Update.Posted)
  
## If recruting or active days since last update by Phase
  
data %>% 
  filter(Status== "Recruiting" | Status== "Active, not recruiting") %>%
    mutate(DaysNoUpdate = as.POSIXct.Date(today()) - Last.Update.Posted)   %>%
      hchart(type="column", hcaes(x=Status, y=DaysNoUpdate))

## tRIALS BY PHASE

data %>% count(Phases) %>% arrange(n) %>%
  hchart(type="column", hcaes (x=Phases, y=n))
## Trials by study type

data %>% count(Study.Type) %>% arrange(n) %>%
  hchart(type="column", hcaes (x=Study.Type, y=n))

## Traisl by Study condition

data %>% count(Conditions) %>% arrange(n) %>% filter(n > 50) %>%
  hchart(type="column", hcaes (x=Conditions, y=n))

### How many studies are diabetic

data$IsDiabetes <- ifelse (grepl("Diabetic", data$Conditions)| grepl("Diabetes", data$Conditions),"Diabetes","Other")
data$IsDiabetes <- as.factor(data$IsDiabetes)
data %>% count (IsDiabetes) %>% arrange (n) %>%
        hchart(type="column", hcaes (x=IsDiabetes, y=n)) %>%
        hc_title(text="Distribution of current studies in the US") %>%
        hc_xAxis(title=list(text="Condition Types")) %>%
        hc_yAxis(title=list(text="Number of studies in category"))
        
  
      
 
            
