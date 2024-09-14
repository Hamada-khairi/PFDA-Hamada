
# 🏡 House Rent Data Analysis and Prediction

## 👥 Team Members
- **TP065783**: Khaled Awad
- **TP064361**: Abdelrahman Mourad
- **TP066168**: Mohamed Khairy

## 📋 Overview

Welcome to the House Rent Data Analysis and Prediction project! This project delves into analyzing a comprehensive dataset of rental housing costs. With 4,746 observations across 12 columns, our goal is to identify patterns and relationships in the data, which include factors like rent, area type, city, size, and furnishing status. We also aim to use predictive analysis to provide insights into the rental market.

## 📚 Table of Contents
- [🔧 Installing Packages](#-installing-packages)
- [📦 Loading Libraries](#-loading-libraries)
- [📂 Data Loading and Pre-processing](#-data-loading-and-pre-processing)
  - [📋 Data Cleaning](#-data-cleaning)
- [📊 Analysis and Visualizations](#-analysis-and-visualizations)
  - [📍 Relationship Between Rent, Area Type, and Point of Contact](#-relationship-between-rent-area-type-and-point-of-contact)
  - [🏙️ Relationship Between Rent, City, and Size](#-relationship-between-rent-city-and-size)
  - [🛋️ Relationship Between Rent, City, and Furnished Status](#-relationship-between-rent-city-and-furnished-status)
  - [🏘️ Most Popular Houses per Category](#-most-popular-houses-per-category)
  - [🌆 Cities with Highest Amounts in Each Category](#-cities-with-highest-amounts-in-each-category)
- [✨ Additional Features](#-additional-features)
- [📌 Conclusion](#-conclusion)
- [📖 References](#-references)

## 🔧 Installing Packages

To perform the analysis and visualizations, you need to install the following R packages:

```r
install.packages("dplyr")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("plotly")
install.packages("tidyr")
install.packages("tidyverse")
install.packages("caTools")
```

## 📦 Loading Libraries

Load the necessary libraries to utilize various functions for data manipulation and visualization:

```r
library(dplyr)
library(ggplot2)
library(corrplot)
library(plotly)
library(tidyr)
library(tidyverse)
library(caTools)
```

## 📂 Data Loading and Pre-processing

### 🗂️ Loading the Dataset
We load the house rent dataset into R for our analysis:

```r
data <- read.csv("path_to_dataset/House_Rent_Dataset.csv")
head(data)
```

### 📋 Data Cleaning

#### Checking for Missing Values
Ensuring data quality is crucial, so we start by checking for missing values in the dataset:

```r
colSums(is.na(data))
```
In this dataset, there are no missing values.

#### Checking for Garbage Values
We look for inconsistencies in categorical columns to ensure data integrity:

```r
unique(data$Area.Type)
unique(data$City)
unique(data$Furnishing.Status)
unique(data$Tenant.Preferred)
unique(data$Point.of.Contact)
```
No garbage values were found in this dataset.

#### Summary Statistics
To understand the basic properties of the data, we generate summary statistics:

```r
summary(data)
```
- **Rent**: Average rent is `34,993`, with a maximum value of `3,500,000`.
- **Size**: Average size is `967 sq ft`, with a maximum size of `8,000 sq ft`.
- **Bathroom**: Average number of bathrooms is `1.9`, with a maximum of `10`.

#### Removing Outliers
Outliers are identified using the interquartile range method to maintain the integrity of our analysis:

```r
outliers <- function(x) {
  Q1 <- quantile(x, probs=.25)
  Q3 <- quantile(x, probs=.75)
  iqr = Q3-Q1
  upper_limit = Q3 + (iqr*1.5)
  lower_limit = Q1 - (iqr*1.5)
  x > upper_limit | x < lower_limit
}

remove_outliers <- function(df, cols = names(df)) {
  for (col in cols) {
    df <- df[!outliers(df[[col]]),]
  }
  df
}
data <- remove_outliers(data, c('Rent', 'Size', 'Bathroom'))
```

## 📊 Analysis and Visualizations

### 📍 Relationship Between Rent, Area Type, and Point of Contact

#### 🧐 Analysis 1.1: Houses with "Contact Owner" as Point of Contact
We explored properties rented directly through contact with the owner:

```r
data[which(data$Point.of.Contact == "Contact Owner"),]
```
Most properties rented through direct contact with the owner are suitable for singles and families.

#### 💰 Analysis 1.2: Average and Maximum Rent
Determine the average and maximum rent:

```r
mean(data$Rent)
max(data$Rent)
```

#### 📈 Analysis 1.3: Rent Distribution by Area Type
Examine how rent varies across different area types using a boxplot:

```r
ggplot(data = data, mapping = aes(x = Area.Type, y = Rent)) +
  geom_boxplot(col="orange") +
  labs(title = "Distribution of Rent By Area Type")
```
- **Carpet Area**: Highest average rent.
- **Super Area**: Moderately priced.
- **Built Area**: Lowest average rent.

#### 🏠 Analysis 1.4: Average House Rents and Sizes by Point of Contact
Determine average house sizes and rents by point of contact:

```r
ggplot(temp, aes(x = "", y = Avg_Rent, fill = Point.of.Contact)) +
  geom_col() +
  geom_text(aes(label = round(Avg_Rent, 2)), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "Average Rent By Point of Contact")
```
- **Agent Contact**: Highest average rent and size.
- **Builder Contact**: Lowest rent and size.

### 🏙️ Relationship Between Rent, City, and Size

#### 🌟 Analysis 2.1: Most and Least Preferred Cities
Identifying the most and least preferred cities based on rental properties:

```r
City_Count <- data %>% group_by(City) %>% summarise(count = length(BHK)) %>% arrange(desc(count))
ggplot(City_Count, mapping = aes(x= City, y= count, fill = count)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "City", y = "Count", title = "Houses Counted by Cities")
```
- **Most Preferred**: Chennai
- **Least Preferred**: Mumbai

#### 📏 Analysis 2.2: Rent Per Size
Calculating the rent per size for each property:

```r
data$Rent_per_size <- data$Rent / data$Size
```

#### 📐 Analysis 2.3: Relationship Between House Size and Rent
Exploring how house size impacts the rent:

```r
ggplot(data, aes(x=Size, y=Rent)) + 
  geom_point() + geom_smooth() +
  labs(title = "Relationship Between Size & Rent")
```
- Positive relationship: Larger size generally corresponds to higher rent.

#### 🏘️ Analysis 2.4: Average House Sizes by City
Identify the average house sizes for each city:

```r
temp <- data %>% group_by(City) %>% summarise(Avg_Size = mean(Size))
ggplot(data = temp, mapping = aes(x = City, y = Avg_Size, fill = City)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Average House Sizes By City")
```
- **Largest Average Size**: Hyderabad
- **Smallest Average Size**: Delhi

### 🛋️ Relationship Between Rent, City, and Furnished Status

#### 🛠️ Analysis 3.1: Preferred Furnishing Status
Finding the most and least preferred furnishing status:

```r
Furnished_Status <- data %>% group_by(Furnishing.Status) %>% summarise(count = length(BHK))
ggplot(Furnished_Status, mapping = aes(x= Furnishing.Status, y= count, fill = count)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Furnished Status", y = "Count", title = "Count By Facilities")
```
- **Most Preferred**: Semi-Furnished

#### 🏙️ Analysis 3.2: City Impact on Rent Prices
Examining how rent prices vary by city:

```r
ggplot(data = data, mapping = aes(x = City, y = Rent)) +
  geom_boxplot(col="black") +
  labs(title = "Distribution of Rent By City")
```
- **Highest Rent**: Mumbai
- **Lowest Rent**: Kolkata

### 🏘️ Most Popular Houses per Category

#### 🚿 Analysis 4.1: Most Popular Number of Bathrooms
Identify the most common number of bathrooms in rental properties:

```r
Bathroom_Count <- data %>% group_by(Bathroom) %>% summarise(count

 = length(BHK)) %>% top_n(5)
ggplot(Bathroom_Count, mapping = aes(x= Bathroom, y= count, fill = count)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Bathrooms", y = "Count", title = "Count Of Bathrooms in 1 House")
```
- **Most Common**: 2 Bathrooms

#### 🏡 Analysis 4.4: Most Popular House Sizes
Analyze the distribution of house sizes:

```r
Size_Count <- data %>% group_by(Size) %>% summarise(count = length(Size)) %>% top_n(8)
ggplot(Size_Count, mapping = aes(x= Size, y= count, fill = count)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Count By Size")
```
- **Most Popular Size**: 600 sq ft

### 🌆 Cities with Highest Amounts in Each Category

#### 🏙️ Analysis 5.1: City with Highest Total Amount of BHK
Identifying the city with the most BHK:

```r
Total_Amount_BHK_Per_city <- data %>% group_by(City) %>% summarise(Total_BHK = sum(BHK))
ggplot(Total_Amount_BHK_Per_city, mapping = aes(x= City, y= Total_BHK, fill = Total_BHK)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Amount Of BHK Per City")
```
- **Highest Total BHK**: Chennai

#### 💵 Analysis 5.3: City with Highest Total Rent
Finding which city has the highest total rent:

```r
Total_Amount_Rent_Per_city <- data %>% group_by(City) %>% summarise(Total_Rent = sum(Rent))
ggplot(Total_Amount_Rent_Per_city, mapping = aes(x= City, y= Total_Rent, fill = Total_Rent)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total amount Of rent per City")
```
- **Highest Total Rent**: Mumbai and Chennai are close.

## ✨ Additional Features

### 📊 Feature 1: Correlogram Matrix
Generating a correlogram to visualize the relationships between different variables:

```r
Correlogram_Matrix <- cor(data[,c(2,3,4,11)])
corrplot(Correlogram_Matrix, addCoef.col = TRUE)
```

### 📈 Feature 2: Scatter Plot with Regression Line
Create a scatter plot with a regression line for rent by house size:

```r
attach(data)
plot(Size, Rent, main = "Scatterplot of rent vs size", xlab = "House size", ylab ="House rent")
abline(lm(Rent ~ Size), col ="blue", lwd = 2)
```

### 🎻 Feature 3: Violin Plot for Rent by Size
Visualizing the distribution of rent with a violin plot:

```r
ggplot(data, aes(x = Rent, y = Size)) + geom_violin(trim = FALSE)
```
## 📷 ScreenShots
![R-9](https://github.com/user-attachments/assets/db6b3260-e6d1-410e-92a1-61cd5e2c24e9)
![R-8](https://github.com/user-attachments/assets/958d318a-7d1c-4b4e-ac34-5ff0942d72a5)
![R-7](https://github.com/user-attachments/assets/61da1f8b-b0d8-4430-9e4e-1cf8eaf5d1f2)
![R-6](https://github.com/user-attachments/assets/0c2fe23b-67d6-41e9-ab38-1b98eb80fedc)
![R-5](https://github.com/user-attachments/assets/6e95a893-0f6c-4d7c-8f88-60dcca3937d7)
![R-4](https://github.com/user-attachments/assets/fd5e036f-eb6d-4fc4-8cf8-6b0108ce145c)
![R-3](https://github.com/user-attachments/assets/8cec6caa-1ff0-4a60-bef0-36081456e51c)
![R-2](https://github.com/user-attachments/assets/eac34bbb-03df-489f-a89c-3c9c7923b62f)
![R-1](https://github.com/user-attachments/assets/99e9afaf-a45c-4e65-9d32-02a65af33e59)



## 📌 Conclusion

This analysis offers a comprehensive exploration of rental housing data, revealing key insights into factors affecting rent prices, area preferences, and housing features. By understanding these patterns, both renters and property managers can make more informed decisions in the rental market.

## 📖 References

1. [How to Remove Outliers in R](https://www.r-bloggers.com/2021/09/how-to-remove-outliers-in-r-3/)
2. [Box plot by group in ggplot2](https://r-charts.com/distribution/box-plot-group-ggplot2/)
3. [ggplot2 scatter plots: Quick start guide - R software and data visualization](http://www.sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization)
4. [8 Tips for Better Data Visualization - Towards Data Science](https://towardsdatascience.com/8-tips-for-better-data-visualization-2f7118e8a9f4)
5. [Tidyverse packages](https://www.tidyverse.org/packages/)
6. [Predicting House Prices using R](https://www.kaggle.com/code/pradeeptripathi/predicting-house-prices-using-r)

