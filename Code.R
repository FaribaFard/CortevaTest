#############################   Problem 1 ##############################################################
# Read the directory where files stored
dir <- 'C:/Users/fs0163/Documents/FaribaUNT/PhDSemester10/ApplyJob/CortenaTest/DataSciTest/wx_data/'
files <- list.files(dir,pattern = '*.txt', full.names = TRUE)

for (f in files){
  my_data <- read.table(f)   # Read a txt file
  row_store<-0 #  rows where maximum and minimum temperature are present but the precipitation is missing.
  for (row in 1:nrow(my_data)){
    max_temp <- my_data[row,2]
    min_temp <- my_data[row,3]
    precipitation <- my_data[row,4]
    ifelse(((max_temp != "-9999")&(min_temp != "-9999")&(precipitation == "-9999")),row_store<- row_store+1, row_store)
  }
  cat(basename(f), row_store,"\n")
  }

#############################   Problem 2 ##############################################################
dir <- 'C:/Users/fs0163/Documents/FaribaUNT/PhDSemester10/ApplyJob/CortenaTest/DataSciTest/wx_data/'
files <- list.files(dir,pattern = '*.txt', full.names = TRUE)

problem2 = data.frame()


for (f in files){

  # Read one weather data
  data <- read.table(f)
  
  # Keep only years from date
  data[,1] <- round(data[,1]/10000)


  ### max_temp ###
  # Separate the second column and store as max_temp
  max_temp <- data[,c(1,2)]
  
  # Remove any records containing missing values in max_temp
  max_temp_NoNull <- max_temp[max_temp$V2 != "-9999",]
  
  # Calculate the mean of max_temp in each single year and divide by 10 to compute Celsius
  library(dplyr)
  max_temp_NoNull_GroupMeanCelsius <- max_temp_NoNull %>% group_by(V1) %>% summarize(max_temp_GroupMean = round(mean(V2)/10,digits = 2))


  ### min_temp ###
  # Separate the third column and store as min_temp
  min_temp <- data[,c(1,3)]
  # Remove any records containing missing values in max_temp
  min_temp_NoNull <- min_temp[min_temp$V3 != "-9999",]
  # Calculate the mean of max_temp in each single year and divide by 10 to compute Celsius
  library(dplyr)
  min_temp_NoNull_GroupMeanCelsius <- min_temp_NoNull %>% group_by(V1) %>% summarize(min_temp_GroupMean = round(mean(V3)/10,digits = 2))


  ### precipitation ###
  # Separate the fourth column and store as precipitation
  precipitation <- data[,c(1,4)]
  
  # Remove any records containing missing values in precipitation
  precipitation_NoNull <- precipitation[precipitation$V4 != "-9999",]
  
  # Calculate the sum of precipitation in each single year
  # Note that each value is divided to 100 to transfer it to centimeter
  precipitation_NoNull_GroupAccumulate <- precipitation_NoNull %>% group_by(V1) %>% summarize(precipitation_GroupAccumulate = round(sum(V4/100),digits = 2))
  
  ### provide the results for each station ###
  Merge1 <- merge(x = max_temp_NoNull_GroupMeanCelsius, y = min_temp_NoNull_GroupMeanCelsius,all.x = T, all.y = T)
  Merge2 <- merge(x = Merge1, y = precipitation_NoNull_GroupAccumulate,all.x = T, all.y = T)
  Merge2$weatherStation <- basename(f)
  Merge2[is.na(Merge2)] <- -9999.00
  Merge2 <- Merge2[, c(5, 1, 2, 3,4)]
  colnames(Merge2) <- c('FileName','Year', 'AveMaxTemp','AveMinTemp','AccumPrec')
  problem2 <- rbind(Merge2 , problem2)
}

problem2 <- problem2[order(problem2$FileName, decreasing = FALSE),]

write.csv(problem2,file='C:/Users/fs0163/Documents/FaribaUNT/PhDSemester10/ApplyJob/CortenaTest/DataSciTest/answers/problem2.csv')



#############################   Problem 3 ##############################################################

library(plyr)



##################################################### how often each year from 1985-2014 had the highest average maximum temperature
# retain only Year, station, AveMaxTemp 
df1 <- problem2[,c(1,2,3)]

#### Remove records containing -9999.00 for AveMaxTemp   
df2<-df1[!(df1$AveMaxTemp==-9999.00),]

#### name of weather station and the year where AveMaxTemp is maximum over the years in each station 
df3 <- df2 %>% group_by(FileName) %>% slice_max(AveMaxTemp) 

# Count the number of times each year experienced maximum "AveMaxTemp" in all weather stations 
df4 <- df3 %>% group_by(Year) %>% summarize(count = n())


#Rename column names of df4  
colnames(df4) <- c('Year','CountStationsWithMax_AveMaxTemp')



##################################################### how often each year from 1985-2014 had the highest average minimum temperature

## retain only Year, station, AveMinTemp
df11 <- problem2[,c(1,2,4)]

#### Remove records containing -9999.00 for AveMaxTemp
df12<-df11[!(df11$AveMinTemp==-9999.00),]

#### name of weather station and the year where AveMaxTemp is maximum over the years in each station
df13 <- df12 %>% group_by(FileName) %>% slice_max(AveMinTemp) 

# Count the number of times each year experienced maximum "AveMaxTemp" in all weather stations
df14 <- df13 %>% group_by(Year) %>% summarize(count = n())

#Rename column names of df14
colnames(df14) <- c('Year','CountStationsWithMax_AveMinTemp')



##################################################### how often each year from 1985-2014 had the highest total precipitation

## retain only Year, station, AveMinTemp
df111 <- problem2[,c(1,2,5)]

#### Remove records containing -9999.00 for AveMaxTemp
df112<-df111[!(df111$AccumPrec==-9999.00),]

#### name of weather station and the year where AveMaxTemp is maximum over the years in each station
df113 <- df112 %>% group_by(FileName) %>% slice_max(AccumPrec) 

# Count the number of times each year experienced maximum "AveMaxTemp" in all weather stations
df114 <- df113 %>% group_by(Year) %>% summarize(count = n())

#Rename column names of df114
colnames(df114) <- c('Year','CountStationsWithMax_AccumPrec')


############ Create a single file
Merge_df4df14 <- merge(x = df4, y = df14,all.x = T, all.y = T)

Merge_df114Merge_df4df14 <- merge(x = Merge_df4df14, y = df114,all.x = T, all.y = T)


write.csv(Merge_df114Merge_df4df14,file='C:/Users/fs0163/Documents/FaribaUNT/PhDSemester10/ApplyJob/CortenaTest/DataSciTest/answers/problem3.csv')


#############################   Problem 4 ##############################################################
library(dplyr)
# Read the Yield data
Yield <- read.table('C:/Users/fs0163/Documents/FaribaUNT/PhDSemester10/ApplyJob/CortenaTest/DataSciTest/yld_data/US_corn_grain_yield.txt')

# Rename the column names of Yield data
colnames(Yield) <- c('Year','Yield')

# Merge problem2 data with Yield data using the "Year" column
Merge_problem2Yield <- merge(x = problem2, y = Yield,all.x = T)

############################################ Separate "Merge_problem2Yield" based on "AveMaxTemp"
Merge_problem2Yield_AveMaxTemp <- Merge_problem2Yield[,c(2,3,6)]

# Remove records containing -9999.00 in "AveMaxTemp"
Merge_problem2Yield_AveMaxTemp<-Merge_problem2Yield_AveMaxTemp[!(Merge_problem2Yield_AveMaxTemp$AveMaxTemp==-9999.00),]

# Compute correlation between Yields and AveMAxTemp by grouping(FileName)
Corr_AveMaxTempYield <- Merge_problem2Yield_AveMaxTemp %>% group_by(FileName) %>%  summarize(cor=cor(AveMaxTemp, Yield))

# Round correlation by 2 digits
Corr_AveMaxTempYield$Corr_AveMaxTempYield  <- round(Corr_AveMaxTempYield$cor, digits = 2)



############################################ Separate "Merge_problem2Yield" based on "AveMinTemp"
Merge_problem2Yield_AveMinTemp <- Merge_problem2Yield[,c(2,4,6)]

# Remove records containing -9999.00 in "AveMaxTemp"
Merge_problem2Yield_AveMinTemp<-Merge_problem2Yield_AveMinTemp[!(Merge_problem2Yield_AveMinTemp$AveMinTemp==-9999.00),]

# Compute correlation between Yields and AveMAxTemp by grouping(FileName)
Corr_AveMinTempYield <- Merge_problem2Yield_AveMinTemp %>% group_by(FileName) %>%  summarize(cor=cor(AveMinTemp, Yield))

# Round correlation by 2 digits
Corr_AveMinTempYield$Corr_AveMinTempYield  <- round(Corr_AveMinTempYield$cor, digits = 2)




############################################ Separate "Merge_problem2Yield" based on "AveMinTemp"
Merge_problem2Yield_AccumPrec <- Merge_problem2Yield[,c(2,5,6)]

# Remove records containing -9999.00 in "AveMaxTemp"
Merge_problem2Yield_AccumPrec<-Merge_problem2Yield_AccumPrec[!(Merge_problem2Yield_AccumPrec$AccumPrec==-9999.00),]

# Compute correlation between Yields and AveMAxTemp by grouping(FileName)
Corr_AccumPrecYield <- Merge_problem2Yield_AccumPrec %>% group_by(FileName) %>%  summarize(cor=cor(AccumPrec, Yield))

# Round correlation by 2 digits
Corr_AccumPrecYield$Corr_AccumPrecYield <- round(Corr_AccumPrecYield$cor, digits = 2)


############################ Create an output file
Merge_CorrAveMaxMinYield <- merge(x = Corr_AveMaxTempYield[,c(1,3)], y = Corr_AveMinTempYield[,c(1,3)],all.x = T)

# The Final file that merged all correlations together
Final_Correlations <- merge(x = Merge_CorrAveMaxMinYield, y = Corr_AccumPrecYield[,c(1,3)],all.x = T)


write.csv(Final_Correlations,file='C:/Users/fs0163/Documents/FaribaUNT/PhDSemester10/ApplyJob/CortenaTest/DataSciTest/answers/problem4.csv')





