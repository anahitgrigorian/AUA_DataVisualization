#Create a Boxplot of Lung Cancer Deaths Distribution
library(ggplot2)
library(readr)

df <-read_csv("lung_cancer_prediction_dataset.csv")

ggplot(df, aes(y = Annual_Lung_Cancer_Deaths)) +
  geom_boxplot(fill = "pink", color = "black", outlier.color = "red", outlier.size = 2) +
  labs(title = "Boxplot of Lung Cancer Deaths Distribution", y = "Annual Lung Cancer Deaths") +
  theme_minimal()


#Create a Histogram of PM2.5 AQI Values
df <- read_csv("global_air_pollution_dataset.csv")


ggplot(df, aes(x = PM2.5_AQI_Value)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.5, color = "black") +
  labs(title = "Histogram of PM2.5 AQI Values", x = "PM2.5 AQI Value", y = "Frequency") +
  theme_minimal()



#Create a Density Plot of the Lung Cancer Mortality Rate
df <-read_csv("lung_cancer_prediction_dataset.csv")


ggplot(df, aes(x = Mortality_Rate)) +
  geom_density(fill = "darkred", alpha = 0.4, color = "cyan") + 
  labs(title = "Density Plot of Lung Cancer Mortality Rate", x = "Lung Cancer Mortality Rate", y = "Density") +
  theme_minimal()



#Create a Scatter Plot by generating 100 random values from both the normal and logistic distributions. The points should be brown and use theme_solarized with argument light set to false
library(ggthemes)
set.seed(42)
df <- data.frame(
  Normal = rnorm(100),
  Logistic = rlogis(100)
)

ggplot(df, aes(x = Normal, y = Logistic)) +
  geom_point(color = "brown", alpha = 0.6) +  theme_minimal() +
  labs(title = "Scatter Plot of Normal vs. Logistic Distribution",x = "Normal Distribution Values", y = "Logistic Distribution Values") +
  theme_solarized(light = FALSE)





















#Use the gpplot2 package for this graph. (Hint: Aggregate the data then merge the two datasets. Use only the necessary columns.)
library(ggplot2)
library(dplyr)
library(readr)

lung_cancer <- read_csv("lung_cancer_prediction_dataset.csv")
air_pollution <- read_csv("global_air_pollution_dataset.csv")

#aggregate deaths
lung_cancer_agg <- lung_cancer %>%
  group_by(Country) %>%
  summarise(Annual_Lung_Cancer_Deaths = sum(Annual_Lung_Cancer_Deaths, na.rm = TRUE))

#aggregate aqi values
air_pollution_agg <- air_pollution %>%
  group_by(Country) %>%
  summarise(PM2.5_AQI = mean(PM2.5_AQI_Value, na.rm = TRUE))

merged_data <- inner_join(lung_cancer_agg, air_pollution_agg, by = "Country")
selected_countries <- c("Egypt", "Ethiopia", "France", "Germany", "India", 
                        "Indonesia", "Italy", "Japan", "Mexico", "Myanmar", 
                        "Nigeria", "Pakistan", "Philippines", "South Africa", 
                        "Thailand", "Turkey")

color_values <- setNames(
  rainbow(n = nrow(merged_data)),   
  merged_data$Country
)

ggplot(merged_data, aes(x = PM2.5_AQI, y = Annual_Lung_Cancer_Deaths, color = Country)) +
  geom_point(size = 3) +
  geom_text(aes(label = ifelse(Country %in% c("China", "India", "Japan"), Country, "")), 
            size = ifelse(merged_data$Country == "China", 6, 3), 
            fontface = "bold", color = "black") +  
  labs(title = "PM2.5 AQI vs. Annual Lung Cancer Deaths", 
       x = "PM2.5 AQI Value", 
       y = "Annual Lung Cancer Deaths", 
       color = "Annual_Lung_Cancer_Deaths") +
  scale_color_manual(values = color_values, breaks = selected_countries) +  
  theme_classic() +  
  theme(
    plot.title = element_text(face = "bold", color = "darkred", size = 20, hjust = 0.5),  
    axis.text.x = element_text(angle = 45, hjust = 1, color = "blue"),  
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "right",  
    legend.title = element_text(size = 13, margin = margin(t = 10)),  
    legend.text = element_text(size = 10),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_line(linetype = "dashed", color = "gray")   
  ) + guides(color = guide_legend(title.position = "bottom", title.hjust = 0.5))







#Use the ggplot2 package for this graph. (Hint: use geom_jitter since y axis contains categorical data, also use the following colors: #5469f1 , #d554f1)
library(ggplot2)
library(dplyr)
library(readr)
lung_cancer <- read_csv("lung_cancer_prediction_dataset.csv")
lung_cancer$Cancer_Stage <- as.factor(lung_cancer$Cancer_Stage)

#here I remove NONE and 0 from Cancer Stage since in the plot it is not present
lung_cancer_filtered <- lung_cancer %>%
  filter(Cancer_Stage != "None", Years_of_Smoking>0)

ggplot(lung_cancer_filtered, aes(x = Years_of_Smoking, y = Cancer_Stage, color = Gender, shape = Gender)) +
  geom_jitter(alpha = 0.7, size = 2.0) +  
  scale_color_manual(values = c("Female" = "#d554f1", "Male" = "#5469f1")) +  
  scale_shape_manual(values = c("Female" = 17, "Male" = 16)) +  
  facet_wrap(~Gender) +  
  labs(title = "Lung Cancer Stage vs. Smoking Years",
       subtitle = "Comparison by Gender", x = "Years of Smoking", y = "Cancer Stage", color = "Gender") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12, color = "black", face = "italic"),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 10),  
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )







#Use the ggplot2 package for this graph. (Hint: use scale_fill_viridis_d(option = "plasma" to get the same colors)
library(ggplot2)
library(dplyr)
library(readr)
library(viridis)  # i use this for color scale

air_pollution <- read_csv("global_air_pollution_dataset.csv")
selected_countries <- c("Brazil", "Germany", "India", "Italy", "Russian Federation", "United States of America")
air_pollution_filtered <- air_pollution %>%
  filter(Country %in% selected_countries)

ggplot(air_pollution_filtered, aes(x = PM2.5_AQI_Value, fill = Country)) +
  geom_histogram(bins = 50, alpha = 0.8, color = "black") +
  scale_fill_viridis_d(option = "plasma") +   
  facet_wrap(~Country, scales = "free_y") +  
  labs(title = "PM2.5 AQI Distribution Across Countries",
       subtitle = "Comparison of Air Pollution Levels", x = "PM2.5 AQI Value",y = "Frequency",fill = "Country") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    plot.subtitle = element_text(size = 12, color = "gray40", face = "italic"),
    strip.text = element_text(face = "bold", size = 12) 
  )




