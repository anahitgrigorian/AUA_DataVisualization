library(ggplot2)
library(dplyr)
library(tidyr)

df <- read.csv('mobiles_dataset.csv')

#onversion rates
conversion_rates <- c(PKR = 0.0036, INR = 0.011, CNY = 0.14, AED = 0.27, USD = 1.0)

#prices to USD
df <- df %>%
  mutate(
    Pakistan_USD = Launched.Price.Pakistan.PKR * conversion_rates["PKR"],
    India_USD = Launched.Price.India.INR * conversion_rates["INR"],
    China_USD = Launched.Price.China.CNY * conversion_rates["CNY"],
    Dubai_USD = Launched.Price.Dubai.AED * conversion_rates["AED"],
    USA_USD = Launched.Price.USA.USD * conversion_rates["USD"],
    Avg_Launch_Price_USD = (Pakistan_USD + India_USD + China_USD + Dubai_USD + USA_USD) / 5
  )

# ---- Question 1: Battery Capacity vs. Price ----
countries <- c("Pakistan_USD", "India_USD", "China_USD", "USA_USD", "Dubai_USD")
for (country in countries) {
  print(ggplot(df, aes(x = Battery.Capacity.mAh, y = !!sym(country))) +
          geom_point(alpha = 0.5) +
          geom_smooth(method = "lm", color = "red") +
          labs(title = paste("Battery Capacity vs. Price in", country), x = "Battery Capacity (mAh)", y = "Price (USD)"))
}
cor_results <- sapply(countries, function(country) cor(df$Battery.Capacity.mAh, df[[country]], use = "complete.obs"))
print(cor_results)

# ---- Question 2: RAM vs. Price ----
library(stringr)
df$RAM_GB <- as.numeric(str_extract(df$RAM, "\\d+"))
for (country in countries) {
  print(ggplot(df, aes(x = RAM_GB, y = !!sym(country))) +
          geom_point(alpha = 0.5) +
          geom_smooth(method = "lm", color = "red") +
          labs(title = paste("RAM vs. Price in", country), x = "RAM (GB)", y = "Price (USD)"))
}
cor_results_ram <- sapply(countries, function(country) cor(df$RAM_GB, df[[country]], use = "complete.obs"))
print(cor_results_ram)

# ---- Question 3: Apple Price Variation Across Regions ----
apple_df <- df %>% filter(tolower(Company.Name) == "apple")
apple_variation <- apple_df %>%
  select(Pakistan_USD, India_USD, China_USD, USA_USD, Dubai_USD) %>%
  summarise_all(sd, na.rm = TRUE)
apple_avg_prices <- apple_df %>%
  summarise(across(c(Pakistan_USD, India_USD, China_USD, USA_USD, Dubai_USD), mean, na.rm = TRUE))
highest_markup <- names(apple_avg_prices)[which.max(apple_avg_prices)]
print(apple_variation)
print(highest_markup)

# ---- Question 4: Brand Price Segments ----
df <- df %>% mutate(
  Price_Category = case_when(
    Avg_Launch_Price_USD < 300 ~ "Budget",
    Avg_Launch_Price_USD >= 300 & Avg_Launch_Price_USD <= 700 ~ "Mid-range",
    TRUE ~ "Premium"
  )
)
brand_segments <- df %>% group_by(Company.Name, Price_Category) %>%
  summarise(Model_Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = Price_Category, values_from = Model_Count, values_fill = 0)
brand_segments <- brand_segments %>% mutate(
  Covers_All_Segments = ifelse((Budget > 0 & `Mid-range` > 0 & Premium > 0), TRUE, FALSE)
)
print(brand_segments)

# ---- Question 5: Most Affordable Region & Price Variations ----
avg_price_per_region <- df %>% summarise(
  Pakistan = mean(Pakistan_USD, na.rm = TRUE),
  India = mean(India_USD, na.rm = TRUE),
  China = mean(China_USD, na.rm = TRUE),
  USA = mean(USA_USD, na.rm = TRUE),
  Dubai = mean(Dubai_USD, na.rm = TRUE)
)
most_affordable_region <- names(avg_price_per_region)[which.min(avg_price_per_region)]
print(avg_price_per_region)
print(most_affordable_region)
brand_price_variability <- df %>%
  group_by(Company.Name) %>%
  summarise(Max_Min_Diff_Percentage = ((max(c(Pakistan_USD, India_USD, China_USD, USA_USD, Dubai_USD), na.rm = TRUE) -
                                          min(c(Pakistan_USD, India_USD, China_USD, USA_USD, Dubai_USD), na.rm = TRUE)) /
                                         min(c(Pakistan_USD, India_USD, China_USD, USA_USD, Dubai_USD), na.rm = TRUE)) * 100,
            .groups = "drop") %>%
  filter(Max_Min_Diff_Percentage > 30)
print(brand_price_variability)



# ---- Part 2: Visualization ----
# 1. Bar chart for average price per region
ggplot(avg_price_per_region %>% gather(key = "Region", value = "Average_Price"), aes(x = Region, y = Average_Price, fill = Region)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Smartphone Price per Region", x = "Region", y = "Average Price (USD)") +
  theme_minimal()

# 2. Pie chart for market share of smartphone brands
brand_market_share <- df %>% count(Company.Name) %>%
  mutate(Percentage = n / sum(n) * 100)

ggplot(brand_market_share, aes(x = "", y = Percentage, fill = Company.Name)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Market Share of Smartphone Brands", x = NULL, y = NULL) +
  theme_void()

# ---- Part 3: Recreate ----
# ---- 1 ----

df_usa <- df %>% select(Company.Name, USA_USD) %>% na.omit()
ggplot(df_usa, aes(x = reorder(Company.Name, Company.Name), y = USA_USD, fill = Company.Name)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +  
  geom_jitter(width = 0.2, size = 1, alpha = 0.6) +   
  scale_fill_manual(values = rainbow(length(unique(df_usa$Company.Name)))) +   
  labs(title = "Price Distribution by Company in USA",
       subtitle = "A boxplot showing how the price varies by company, with individual data points overlaid",
       x = "Company",
       y = "Price in USD",
       fill = "Company Name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))   


# ---- 2 ----
df_usa <- df %>% select(Company.Name, Battery.Capacity.mAh, USA_USD) %>% na.omit()
ggplot(df_usa, aes(x = Battery.Capacity.mAh, y = USA_USD, color = Company.Name)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = rainbow(length(unique(df_usa$Company.Name)))) +  
  labs(title = "Battery Capacity vs. Price in USA",
       subtitle = "The relationship between battery capacity, price, and screen size across different smartphone brands",
       x = "Battery Capacity",
       y = "Price",
       color = "Brand") +
  theme_minimal()


# --- 3 ---
top_brands <- c("Apple", "Honor", "Oppo", "Samsung", "Vivo")
df_top5 <- df %>%
  filter(Company.Name %in% top_brands) %>%
  select(Company.Name, Battery.Capacity.mAh, USA_USD, Screen.Size.inches) %>%
  na.omit()
ggplot(df_top5, aes(x = Battery.Capacity.mAh, y = USA_USD, shape = Company.Name, color = Screen.Size.inches)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_shape_manual(values = c("Apple" = 16, "Honor" = 17, "Oppo" = 18, "Samsung" = 15, "Vivo" = 16)) +   
  scale_color_gradient(low = "dodgerblue4", high = "#7DF9FF", guide = "none") +  
  labs(title = "Battery Capacity vs. Price for Top 5 Brands",
       subtitle = "Different Shapes for Each Brand, Color by Screen Size, (USA)",
       x = "Battery Capacity (mAh)",
       y = "Price (USD)",
       shape = "Brand") +
  theme_minimal()
