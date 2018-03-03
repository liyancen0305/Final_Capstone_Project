############################## Capstone Project Milestone Report ####################################

##### Introduction

  # This project is customized for clients who are interested in investment property in both Round Rock, Texas and Los Angeles County. With respect to the data collected for the two Metropolitan Area, over the years, we present the client the comparison between two cities regarding to the investing purpose. I will analyze data on the “house price index”, "Population trend", "Demographics", and “unemployment rate” for both cities. 

  # While these factors are important to consider for investing purpose, investors may also think about other factors like operating expenses, property tax rates, financing conditions, etc., which are not covered in our datasets. 

##### Data Wrangling
  
  # Based on the data I collected for two cities on house price index, "Population trend", "Demographics", and an unemployment rate, I used several functions and commands to make the datasets more readable, which are all shown below under "Data Wrangling" section.

##### Explanatory Data analysis

  # # Unemployment rate

      # The unemployment rate is a measure of the prevalence of unemployment and it is calculated as a percentage by dividing the number of unemployed individuals by all individuals currently in the labor force. By checking out the line chart "Unemployment Rate Trend From 1990 To 2016", we can see that the rates are not stable over the years, expecially during 2003, and 2011. Comparatively, Round Rock is in a more stable condition, with a smaller scale. Also, based on both curves after 2010, we can tell that Round Rock recovered from "2008 economic depression" sooner than Los Angeles becuase the curve is steeper.

      # Unemployment is an important factor that the investor should consider becuase the healthier the job market is, the more prosperous the house market will be.Since Los Angeles County is a larger economy system than Round Rock, it is reasonable that the unemployment rate is less stable, which might be an indicator that houses in LA might encounter larger fluctuations than it would in Round Rock if economic recession or depression occurs in the future.


  # # Population Trend

      # According to the Bar plot of "Historical Population Trend", we can see that both populations are increasing, while Round Rock's trend is increasing in a larger proportion. In 2016, the population growth rate in Los Angeles County, and Round Rock are 3.25% and 21.03% respectively.

      # The population growth in Los Angeles is getting much slower that Round Rock, because Los Angeles county is a pretty developed city where growth potential has reached its limit.On the other side, Population in Round Rock keeps growing over the last couple of years. Also, unlike Los Angles, Round Rock is far from developed, it is still a growing city.

  # # House Price Index

      # The HPI is a broad measure of the movement of single-family house prices. The HPI is a weighted, repeat-sales index, meaning that it measures average price changes in repeat sales or refinancings on the same properties. In other words, it means that it measures average price changes in repeat sales or refinancing on the same properties. Based on the linear regression "HPI Trend", we can see that the both HPIs are on an increasing trend while Round Rock has a higher HPI with more stability and larger sale.
      
      # On the other hand, by comparing both " Los Angeles - Unemployment Rate & HPI" and " Round Rock - Unemployment Rate & HPI", there is a inverse relationship between unemployment rate and HPI. As the unemployment increases, the HPI decreaes, vice versa. This might illustrate an idea that while the job market is healthy, people are willing to buy houses.


  # # Demographics

      # client should also consider Demographics as an importnat factor if he or she in interested in buying a house for both investing and residential purpose. Demographis is a good reference for how the culture, diversity, and community life could be in the city they are intered in.


################################ Library ########################################
library (ggplot2)
library(plotly)
library(lubridate)
library(plotly)
################################ DATASET INPUT ###################################
datDir <- "C:\\Users\\Lishuang Cen\\Documents\\Sandbox\\Final_Capstone_Project\\Datasets"
#--- LA_Unemployment_Original
datFile1 <- paste(datDir, "Los_Angeles_Unemployment_Original.csv", sep = "/")
LA_Unemployment_Original <- read.table(datFile1, header = TRUE, sep= "")

#--- Round_Rock_Unemployment_Original
datFile2 <- paste(datDir, "Round_Rock_Unemployment_Original.csv", sep = "/")
Round_Rock_Unemployment_Original <- read.table(datFile2, header = TRUE, sep= "")

#--- LA_Demographics_Original
datFile3 <- paste(datDir, "Los_Angeles_Demographics_Original.csv", sep = "/")
LA_Demographics_Original <- read.table(datFile3, header = TRUE, sep = ",")

#--- Round_Rock_Demographics_Original
datFile4 <- paste(datDir, "Round_Rock_Demographics_Original.csv", sep = "/")
Round_Rock_Demographics_Original <- read.table(datFile4, header = TRUE, sep = ",")

#--- LA_Historical_Population_Original
datFile5 <- paste(datDir, "Los_Angeles_Historical_Population_Original.csv", sep = "/")
LA_Historical_Population_Original <- read.table(datFile5, header = TRUE, sep= ",")

#--- Round_Rock_Historical_Population_Original
datFile6 <- paste(datDir, "Round_Rock_Historical_Population_Original.csv", sep = "/")
Round_Rock_Historical_Population_Original <- read.table(datFile6, header = TRUE, sep= ",")

#--- LA_HPI_Original
datFile7 <- paste(datDir, "Los_Angeles_HPI_Original.csv", sep = "/")
LA_HPI_Original <- read.table(datFile7, header = TRUE, sep= "")

#--- Round_Rock_HPI_Original
datFile8 <- paste(datDir, "Round_Rock_HPI_Original.csv", sep = "/")
Round_Rock_HPI_Original <- read.table(datFile8, header = TRUE, sep= "")

############################## DATA WRANGLING ###########################################

# A. data wrangling
# 1. Los_Angles_unemployment_Original.csv vs. Round_Rock_Unemployment_Original.csv
  #a. rename columns
      names(LA_Unemployment_Original)[1] <- "Year"
      names(Round_Rock_Unemployment_Original)[1] <- "Year"
      names(LA_Unemployment_Original)[2] <- "Unemployment_Rate"
      names(Round_Rock_Unemployment_Original)[2] <- "Unemployment_Rate"
  #b. format the years
      #- only have the first month of every year
      a <- endsWith(as.character(LA_Unemployment_Original$Year), "£¯1£¯1")  # Which rows to keep (kp)?
      LA_Unemployment_Clean <- LA_Unemployment_Original[a, ]
      b <- endsWith(as.character(Round_Rock_Unemployment_Original$Year), "£¯1£¯1")  # Which rows to keep (kp)?
      Round_Rock_Unemployment_Clean <- Round_Rock_Unemployment_Original[b, ]
      #- format the "year"
      LA_Unemployment_Clean$Year <- year(as.Date(LA_Unemployment_Clean$Year, "%y"))
      Round_Rock_Unemployment_Clean$Year <- year(as.Date(Round_Rock_Unemployment_Clean$Year, "%y"))
  #c. save to a new file
      datFile9 <- paste(datDir, "Los_Angeles_Unemployment_Clean.csv", sep = "/")
      write.table(LA_Unemployment_Clean, datFile9)
      datFile10 <- paste(datDir, "Round_Rock_Unemployment_Clean.csv", sep = "/")
      write.table(Round_Rock_Unemployment_Clean, datFile10)
  #d. select 1990. 2000, 2010, 2016 for my dataset
      #Round_Rock_U <- Round_Rock_Unemployment_Clean[c(1,11,21,27), ]
      #LA_U <- LA_Unemployment_Clean[c(1,11,21,27), ]
      #names(Round_Rock_U)[2] <- "Unemployment_Rate"
      
# 2. Los_Angeles_Demographic_Original.csv vs. Round_Rock_Demographic_Original.csv.
  #a. rename columns
      names(LA_Demographics_Original)[2] <- "Ratio"
      names(Round_Rock_Demographics_Original)[2] <- "Ratio"
  #d. delete meaningless columns
      LA_Demographics_Original$X.1 <- NULL
      Round_Rock_Demographics_Original$X.1 <- NULL
  #c. delete meaningless rows
      LA_Demographics_Clean <- LA_Demographics_Original[1:7, ]
      Round_Rock_Demographics_Clean <- Round_Rock_Demographics_Original[1:7, ]
      Round_Rock_Demographics_Clean<- Round_Rock_Demographics_Clean[c(1:4,6,7), ]
  #c. save to a new file
      datFile11 <- paste(datDir, "Los_Angeles_Demographics_Clean.csv", sep = "/")
      write.table(LA_Demographics_Clean, datFile11)
      datFile12 <- paste(datDir, "Round_Rock_Demographics_Clean.csv", sep = "/")
      write.table(Round_Rock_Demographics_Clean, datFile12)

# 3. Los_Angeles_Historical_Population_Origianl vs Round_Rock_Historical_Population_Original.
  #a. rename columns
      names(LA_Historical_Population_Original)[2] <- "Population"
      names(Round_Rock_Historical_Population_Original)[2] <- "Population"
      names(LA_Historical_Population_Original)[3] <- "Percentage_Change"
      names(Round_Rock_Historical_Population_Original)[3] <- "Percentage_Change"
  #b. delete meaningless rows.
      Round_Rock_Historical_Population_Clean <- Round_Rock_Historical_Population_Original[1:15, ]
      LA_Historical_Population_Clean <- LA_Historical_Population_Original[1:18, ]
  #c. save to a new file
      datFile13 <- paste(datDir, "Los_Angeles_Historical_Population_Clean.csv", sep = "/")
      write.table(LA_Historical_Population_Clean, datFile13)
      datFile14 <- paste(datDir, "Round_Rock_Historical_Population_Clean.csv", sep = "/")
      write.table(Round_Rock_Historical_Population_Clean, datFile14)
  #d. select 1990. 2000, 2010, 2016 for my dataset
      #Round_Rock_P <- Round_Rock_Historical_Population_Original[c(12,13,14,15), ]
      #LA_P <- LA_Historical_Population_Original[c(15,16,17,18), ]
      #names(Round_Rock_P)[2:3] <- c("Population", "Percentage_Change")

# 4. Los_Angeles_HPI_Original.csv vs. Round_Rock_HPI_Original.csv.
    #a. rename columns
      names(LA_HPI_Original)[1] <- "Year"
      names(Round_Rock_HPI_Original)[1] <- "Year"
      names(LA_HPI_Original)[2] <- "HPI"
      names(Round_Rock_HPI_Original)[2] <- "HPI"
    #b. only remain the first quarter of every year
      d <- endsWith(as.character(Round_Rock_HPI_Original$Year), "£¯1£¯1")  # Which rows to keep (kp)?
      Round_Rock_HPI_Clean <- Round_Rock_HPI_Original[d,]
      g <- endsWith(as.character(LA_HPI_Original$Year), "£¯1£¯1")  # Which rows to keep (kp)?
      LA_HPI_Clean <- LA_HPI_Original[g,]
    #c. make two data comparable in the number of years
      #LA_HPI_Clean <- LA_HPI_Original[4:42, ]
      #Round_Rock_HPI_Clean <- Round_Rock_HPI_Clean[1:39, ]
    #d. format the year
      LA_HPI_Clean$Year <- year(as.Date(LA_HPI_Clean$Year, "%y"))
      Round_Rock_HPI_Clean$Year <- year(as.Date(Round_Rock_HPI_Clean$Year, "%y"))
    #e. save to a new file
      datFile15 <- paste(datDir, "Los_Angeles_HPI_Clean.csv", sep = "/")
      write.table(LA_HPI_Clean, datFile15)
      datFile16 <- paste(datDir, "Round_Rock_HPI_Clean.csv", sep = "/")
      write.table(Round_Rock_HPI_Clean, datFile16)
    #f. combine two dataset
      #LA_HPI <- LA_HPI_Clean[c(16,26,36,42), ]
      #Round_Rock_HPI <- Round_Rock_HPI_Clean[c(13,23,33,39), ]
      
#5. LA Dataset combined.
      #LA_Dataset <- cbind(LA_P, LA_HPI$HPI, LA_U$Unemployment_Rate)
#6. Round Rock Dataset combined.
      #Round_Rock_Dataset <- cbind(Round_Rock_P, Round_Rock_HPI$HPI, Round_Rock_U$Unemployment_Rate)
      
#7. Merge all dataset together for LA
      LA_Dataset <- merge(LA_HPI_Clean, LA_Historical_Population_Clean, all = TRUE, by.x = "Year", by.y = "Census")
      LA_Dataset <- merge(LA_Dataset, LA_Unemployment_Clean, all = TRUE, by.x = "Year", by.y = "Year")
      LA_Dataset[55, 3] <- as.numeric("10137915")
      LA_Dataset[55, 4] <- as.factor("3.25%")
      LA_Dataset <- LA_Dataset[1:56, ]
      #names(Dataset)[2:9] <- c("Population", "Percentage_Change", "HPI", "Unemployment_Rate", "Population_RK", "Percentage_Change_RK", "HPI_RK","Unemployment_Rate_RK")
      LA_Dataset$Year <- as.numeric(LA_Dataset$Year)
      
#8. Merge all dataset together for Round Rock
      RK_Dataset <- merge(Round_Rock_HPI_Clean, Round_Rock_Historical_Population_Clean, all = TRUE, by.x = "Year", by.y = "Census")
      RK_Dataset <- merge(RK_Dataset, Round_Rock_Unemployment_Clean, all = TRUE, by.x = "Year", by.y = "Year")
      names(RK_Dataset)[1:5] <- c("Year_RK", "HPI_RK", "Population_RK", "Percentage_Change_RK", "Unemployment_Rate_RK")
      RK_Dataset[49, 3] <- as.numeric("120892")
      RK_Dataset[49, 4] <- as.factor("21.03%")
      RK_Dataset <- RK_Dataset[1:50, ]
      RK_Dataset$Year <- as.numeric(RK_Dataset$Year)
#9. Merge all dataset together
      Dataset <- merge(LA_Dataset, RK_Dataset, all = TRUE, by.x = "Year", by.y = "Year_RK")
      Dataset[55, c(3,7)] <- as.numeric(c("10137915", "120892"))
      Dataset[55, c(4,8)] <- as.factor(c("3.25%", "21.03%"))
      Dataset <- Dataset[1:56, ]
      
      
################################## Explanatory Data Anlysis ################################################
# B. Explanatory Data Anlysis
      # 1. Los_Angles_unemployment_Original.csv vs. Round_Rock_Unemployment_Original.csv, Line graph
        Unemployment_line <- ggplot(Dataset[29:57, ], aes(x= Year)) +
                geom_line(aes( y = Unemployment_Rate, colour = "Los Angeles", group = 1))+
                geom_line(aes( y = Unemployment_Rate_RK, colour = "Round Rock", group = 1)) +
                ggtitle ("Unemployment Rate Trend")
     
        Unemployment_line

      # 2. Los_Angeles_Demographic_Original.csv vs. Round_Rock_Demographic_Original.csv., Pie chart
        # a. LA_Demographics
          names_LA <- as.character(LA_Demographics_Clean[,1])
          props_LA <- as.numeric(gsub('\\%', '', as.character(LA_Demographics_Clean[,2])))
          Label_LA <- paste(names_LA, props_LA, "%")
          pie(props_LA, labels = Label_LA, col = rainbow(length(props_LA)), main = "Los Angeles Demographics\n Year 2016")
        # b. Round_Rock_Demographics
          names_Round_Rock <- as.character(Round_Rock_Demographics_Clean[,1])
          props_Round_Rock <- as.numeric(gsub('\\%', '', as.character(Round_Rock_Demographics_Clean[,2])))
          Label_Round_Rock <- paste( names_Round_Rock, props_Round_Rock, "%")
          pie(props_Round_Rock, labels = Label_Round_Rock, col = rainbow(length(props_Round_Rock)), main = "Round Rock Demographics\n Year 2016")


      # 3. Los_Angeles_Historical_Population_Origianl vs Round_Rock_Historical_Population_Original, Bar plot
         
          ratio_LA <- as.numeric(gsub('\\%', '', as.character(Dataset[,4])))
          ratio_Round_Rock <-  as.numeric(gsub('\\%', '', as.character(Dataset[,8])))
          
          Historical_Population_bar <- plot_ly(Dataset, x = ~Year, y = ~ratio_LA, text = ~Percentage_Change, textposition = 'auto', type = 'bar', name = 'Los Angeles') %>%
            add_trace(y = ~ratio_Round_Rock, text = ~Percentage_Change_RK, name = 'Round Rock') %>%
            layout(title = "Historical Population Trend",
                   yaxis = list(title = "Population Trend Ratio"))
          Historical_Population_bar
          
      # 4. Los_Angeles_HPI_Original.csv vs. Round_Rock_HPI_Original.csv, Line graph

          HPI_line <- ggplot(Dataset[14:56, ], aes(x= Year, y = HPI)) +
            geom_line(aes( y = HPI, colour = "Los Angeles"), group = 1) +
            geom_line(aes( y = HPI_RK, colour = "Round Rock"), group = 1) +
            geom_smooth(method = lm) + ggtitle("HPI Trend")
          HPI_line
          

        
      # 5. Combine  LA's "Unemployment Rate" and " HPI" datasets together
          
          Dataset_line_LA <- ggplot(Dataset[13:57, ], aes(Year)) +
            geom_line(aes( y = Unemployment_Rate, colour = "Unemployment_Rate"), group = 1) +
            geom_line(aes( y = HPI, colour = "HPI"), group = 1) +
            ggtitle("Los Angeles - Unemployment Rate & HPI")
          Dataset_line_LA
          
          
      # 6. Combine Round Rock's "HPI" and "Unemployment" datasets together
          
          Dataset_line_Round_Rock <- ggplot(Dataset[15:57, ], aes(Year)) +
            geom_line(aes( y = Unemployment_Rate_RK, colour = "Unemployment Rate"), group = 1) +
            geom_line(aes( y = HPI_RK, colour = "HPI"), group = 1) +
            ggtitle("Round Rock - Unemployment Rate & HPI")
          Dataset_line_Round_Rock
   
################################################### Linear Regression Model  #########################################################
         #  use it to test if LA has a higher upward trend vs Round Rock?  
          
      # 1. Los Angeles County   
          LA.ds <- subset(LA_Dataset, select = c("Unemployment_Rate", "HPI", "Year", "Population"))
          summary(LA.ds)
          plot(RK.ds)
          
          LA.ds.mod <- lm(HPI ~ Unemployment_Rate +Year+ Population -1, data = na.omit(LA.ds))
          summary(LA.ds.mod)
          
      #2. Round Rock
          RK.ds <- subset(RK_Dataset, select = c("Unemployment_Rate_RK", "HPI_RK", "Year", "Population_RK"))
          summary(RK.ds)
          plot(RK.ds)
          
          RK.ds.mod <- lm(HPI_RK ~ Unemployment_Rate_RK+Year+ Population_RK -1, data = na.omit(RK.ds))
          summary(RK.ds.mod)
          
          