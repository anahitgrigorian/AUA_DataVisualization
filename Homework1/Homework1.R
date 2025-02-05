library(dplyr)
library(readr)
library(lubridate)

#Load the dataset. Check the first 5 rows.
file_path <- "/Users/agrigoryan/Desktop/Masters/AUA/Data Visualization/Homeworks/Homework1/crime_data.csv"   
df <- read_csv(file_path)
df %>% head(5)

#Identify columns with missing values and their respective counts. Drop columns where more than 50% of the data is missing (store this version as a new dataset).
threshold <- nrow(df) * 0.5
df_cleaned <- df %>% select(where(~ sum(is.na(.)) < threshold))
write_csv(df_cleaned, "/Users/agrigoryan/Desktop/Masters/AUA/Data Visualization/Homeworks/Homework1/crime_data_cleaned_r.csv")
df_cleaned %>% head(5)

#Convert the DATE OCC column to a datetime format. Extract the year, month, and day into separate columns. Create a new column for the hour using the TIME OCC column.
df_cleaned %>% select(`DATE OCC`) %>% head(10)
df_cleaned <- df_cleaned %>%
  mutate(`DATE OCC` = mdy_hms(`DATE OCC`, tz = "UTC"))
df_cleaned <- df_cleaned %>%
  mutate(Year = year(`DATE OCC`),
         Month = month(`DATE OCC`),
         Day = day(`DATE OCC`))
df_cleaned <- df_cleaned %>%
  mutate(`TIME OCC` = sprintf("%04d", as.integer(`TIME OCC`)),  
         Hour = as.integer(substr(`TIME OCC`, 1, 2)))  
df_cleaned %>% select(`DATE OCC`, Year, Month, Day, `TIME OCC`, Hour) %>% head(10)


#Filter the dataset for crimes that occurred in 2023. Further filter crimes with the description BURGLARY in the Crm Cd Desc column.
df_2023 <- df_cleaned %>%
  filter(Year == 2023)
df_burglary_2023 <- df_2023 %>%
  filter(toupper(`Crm Cd Desc`) == "BURGLARY")
df_burglary_2023 %>% head(10)


#Group the data by AREA NAME and calculate the total number of crimes and the average victim age. Sort the results by total crimes in descending order.
grouped_by_area <- df_cleaned %>%
  group_by(`AREA NAME`) %>%
  summarise(Total_Crimes = n(),Avg_Victim_Age = mean(`Vict Age`, na.rm = TRUE)   ) %>%
  arrange(desc(Total_Crimes))  
grouped_by_area %>% head(10)

#Group the data by Month and count the number of crimes
crimes_by_month <- df_cleaned %>%
  group_by(Month) %>%
  summarise(Total_Crimes = n()) %>%
  arrange(Month)
crimes_by_month %>% head(12)

#Count the number of crimes where a weapon was used
weapon_crimes_count <- df %>%
  filter(!is.na(`Weapon Used Cd`)) %>%
  summarise(Total_Crimes_With_Weapon = n())
weapon_crimes_count


#Group the data by Premis Desc and count the number of crimes
crimes_by_premis <- df_cleaned %>%
  group_by(`Premis Desc`) %>%
  summarise(Total_Crimes = n()) %>%
  arrange(desc(Total_Crimes))
crimes_by_premis %>% head(10)



df <- df %>%
  mutate(Severity_Score = 1)
df <- df %>%
  mutate(Severity_Score = ifelse(!is.na(`Weapon Used Cd`), Severity_Score + 5, Severity_Score))
df <- df %>%
  mutate(Severity_Score = ifelse(toupper(`Crm Cd Desc`) == "BURGLARY", Severity_Score + 3, Severity_Score))
grouped_severity <- df %>%
  group_by(`AREA NAME`) %>%
  summarise(Total_Severity_Score = sum(Severity_Score, na.rm = TRUE)) %>%
  arrange(desc(Total_Severity_Score))
head(grouped_severity, 10)




