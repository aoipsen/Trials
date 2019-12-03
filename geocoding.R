###  Load library

library(ggmap)
library(dplyr)

data = read.csv("clinical_trials.csv",head=TRUE,encoding = "UTF-8")


geocoded <- data.frame(stringsAsFactors = False)
data$Locations <- as.character(data$Locations)
data$HospitalName <- sapply(strsplit(data$Locations,split=','),"[", 1)
data$temp <- strsplit(data$Locations,split=',')
data$temp <- sapply(data$temp,tail,2)
data$temp2 <- sapply(data$temp,"[",1) 
data$temp3 <- sapply(data$temp,"[",2)
data$Address <- paste0(as.character(data$HospitalName), ", ", as.character(data$temp2), ", ",as.character(data$temp3) )

getdat <- data %>% filter (Address != "NA, NA, NA")
x <- getdat %>% filter (HospitalName == "Asia Diabetes Foundation")

## Clean China Problems
for (i in 1:nrow(getdat)){
  if (trim(getdat$temp2[i])=="Hong Kong" & trim(getdat$temp3[i])=="China") {
    print("Working")
    getdat$temp2[i]<- trim(getdat$temp2[i])
    getdat$temp3[i]<-"Hong Kong"
    getdat$Address[i]<- paste0(as.character(getdat$HospitalName[i]), ", ", as.character(getdat$temp2[i]), ", ",as.character(getdat$temp3[i]) )
  }
}
# Clean Korea Problems
for (i in 1:nrow(getdat)){
  if (trim(getdat$temp2[i])=="Korea" & trim(getdat$temp3[i])=="Republic of Korea") {
    getdat$temp2[i]<- trim(getdat$temp2[i])
    getdat$temp3[i]<-"Republic of South Korea"
    getdat$Address[i]<- paste0(as.character(getdat$HospitalName[i]), ", ", as.character(getdat$temp3[i]) )
  }
}
  
for(i in 1:5000){
  # Print("Working...")
  try({
  result <- geocode(getdat$Address[i], output = "latlona")
  getdat$lon[i] <- as.numeric(result[1])
  getdat$lat[i] <- as.numeric(result[2])
  getdat$geoAddress[i] <- as.character(result[3])
  },silent=TRUE)
  
}

getdat1 <- head(getdat,5000)
getdat1$IsDiabetes <- ifelse (grepl("Diabetic", geodata$Conditions)|grepl("Diabete", geodata$Conditions)| grepl("Diabetes", geodata$Conditions),"Diabetes", str_to_title(geodata$Conditions))
getdat1$IsDiabetes <- as.factor(geodata$IsDiabetes)
# Write a CSV file containing origAddress to the working directory
getdat1 %>% select(-c(temp,temp2,temp3)) %>% write.csv("geocoded.csv", row.names=FALSE)
