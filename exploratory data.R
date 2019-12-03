data <- read.csv(file="C:/Users/Ipsen/Documents/Git/TrialData/clinical_trials.csv",head=TRUE,encoding = "UTF-8")


library(highcharter) 
library(dplyr)
library(ggplot2)
library(stringr)
library(purrr)
library(plotly)
library(anytime)
library(lubridate)
library(gdata)
library(viridisLite)

NN_Color <- c("#0299F4","#01387B","#0299F4")
Color_Cols<- viridis(3)
              

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
  hchart(type="column", hcaes(x=Phases,y=Enroll)) %>% hc_colors(NN_Color) %>% 
  hc_title(text="Enrollment of patients in different phases") %>%
  hc_add_series(Name="Enrollment",ShowInLegend=FALSE)
  
  ## Update Date vaiables 
  data$Start.Date <- anytime( data$Start.Date)  
  data$Last.Update.Posted<- anytime( data$Last.Update.Posted)
  
## If recruting or active days since last update by Phase
  
data %>% 
  filter(Status== "Recruiting" | Status== "Active, not recruiting") %>%
    mutate(DaysNoUpdate = as.POSIXct.Date(today()) - Last.Update.Posted)   %>%
      hchart(type="column", hcaes(y=DaysNoUpdate, group=Status))

## tRIALS BY PHASE

data %>% count(Phases) %>% arrange(n) %>%
  hchart(type="column", hcaes (x=Phases, y=n),dataLabels = list(
    enabled = TRUE,
    formatter = JS(
      "function() {
      return this.point.y  ;
    }"))) %>% hc_title(text="Number of trials in different phases") %>%
  hc_yAxis(title=list(text="Number of Studies"))
## Trials by study type

data %>% count(Study.Type) %>% arrange(n) %>%
  hchart(type="column", hcaes (x=Study.Type, y=n))

## Traisl by Study condition

data %>% count(Conditions) %>% arrange(n) %>% filter(n > 50) %>%
  hchart(type="column", hcaes (x=Conditions, y=n))

### How many studies are diabetic

data$IsDiabetes <- ifelse (grepl("Diabetic", data$Conditions)|grepl("Diabete", data$Conditions)| grepl("Diabetes", data$Conditions),"Diabetes", str_to_title(data$Conditions))
data$IsDiabetes <- as.factor(data$IsDiabetes)
data %>% count (IsDiabetes) %>% arrange (n) %>% filter(n>5) %>%
        hchart(type="column", hcaes (x=IsDiabetes, y=n), dataLabels = list(
          enabled = TRUE,
          formatter = JS(
            "function() {
                  return this.point.y;
                  }" ))) %>%
        hc_title(text="Distribution of current studies in the US") %>%
        hc_xAxis(title=list(text="Condition Types")) %>%
        hc_yAxis(title=list(text="Log Scaled Number of studies in category"),type="logarithmic") %>%
        hc_colors(NN_Color)
        
### Study design
data$Study.Designs <- as.character(data$Study.Designs)
data %>% filter(!is.na(Study.Designs)) %>%
          mutate(substr1=sapply(strsplit(Study.Designs,split='\\|'),"[", 1)) %>%
          count(substr1) %>% arrange(n) %>% #hchart() %>%
          hchart(type="column", hcaes(x=substr1,y=n),dataLabels = list(
            enabled = TRUE,
            formatter = JS(
              "function() {
      
      return this.point.y  ;
    }"
            ))) %>% 
          hc_yAxis(title=list(text="Log Scaled distribution of study designs"),type="logarithmic")
          


dat2 <- data.frame(
  type_1 = c("a", "a", "b", "b", "c", "c"),
  type_2 = c("1", "2", "1", "2", "1", "2"),
  n = c(5,8,10,4,7,9))

# create chart
highchart() %>% 
  hc_add_series(dat2, type = "bar", hcaes(x = type_1, group = type_2, y = n), dataLabels = list(
    enabled = TRUE,
    formatter = JS(
      "function() {
      
      return this.point.y  ;
    }"
    ))) %>% 
  hc_xAxis(categories = dat2$type_1)  
      
filter <- data %>% filter(Study.Results=="Has Results")

data %>% count(Funded.Bys) %>% arrange(n) %>% hchart(type="pie",hcaes(x=Funded.Bys, y=n), name = "Value") %>%
  hc_tooltip(pointformat=paste("<b>{point.percentage:.1f}%</b>"))
            
  
  