
# 🏡 House Rent Data Analysis

## 👥 Team Members
- **TP065783**: Khaled Awad
- **TP064361**: Abdelrahman Mourad
- **TP066168**: Mohamed Khairy aka Hamada

## 📋 Overview

Welcome to the House Rent Data Analysis project! This project delves into the complexities of house rent data using R, exploring various factors such as rent, area type, city, size, and furnishing status. Through detailed data cleaning, visualization, and analysis, we aim to uncover insights into the rental market. 

## 📚 Table of Contents
- [🔧 Installing Packages](#-installing-packages)
- [📦 Loading Libraries](#-loading-libraries)
- [📂 Data Loading and Cleaning](#-data-loading-and-cleaning)
- [📊 Analysis and Visualizations](#-analysis-and-visualizations)
  - [📍 Relationship Between Rent, Area Type, and Point of Contact](#-relationship-between-rent-area-type-and-point-of-contact)
  - [🏙️ Relationship Between Rent, City, and Size](#-relationship-between-rent-city-and-size)
  - [🛋️ Relationship Between Rent, City, and Furnished Status](#-relationship-between-rent-city-and-furnished-status)
  - [🏘️ Most Popular Houses per Category](#-most-popular-houses-per-category)
  - [🌆 Cities with Highest Amounts in Each Category](#-cities-with-highest-amounts-in-each-category)
- [✨ Additional Features](#-additional-features)
- [📌 Conclusion](#-conclusion)

## 🔧 Installing Packages

To begin the analysis, you need to install the following R packages:

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

Load the necessary libraries to leverage data manipulation, visualization, and statistical functions:

```r
library(dplyr)
library(ggplot2)
library(corrplot)
library(plotly)
library(tidyr)
library(tidyverse)
library(caTools)
```

## 📂 Data Loading and Cleaning

### 🗂️ Loading the Dataset
We start by loading the house rent dataset into R for analysis:

```r
data <- read.csv("path_to_dataset/House_Rent_Dataset.csv")
head(data)
```

### 🧐 Checking for Missing Values
It's essential to check for any missing values to ensure data quality:

```r
colSums(is.na(data))
```

### 🧹 Data Cleaning
#### Removing Outliers
Outliers can skew analysis results. We identify and remove them to maintain accuracy:

```r
remove_outliers <- function(df, cols = names(df)) {
  for (col in cols) {
    df <- df[!outliers(df[[col]]),]
  }
  df
}
data <- remove_outliers(data, c('Rent', 'Size', 'Bathroom'))
```

### 📊 Summary Statistics
Generate summary statistics to understand the dataset better:

```r
summary(data)
```

## 📊 Analysis and Visualizations

### 📍 Relationship Between Rent, Area Type, and Point of Contact

#### 🔍 Analysis 1.1: Houses with "Contact Owner" as Point of Contact
Identify houses where the point of contact is directly the owner:

```r
data[which(data$Point.of.Contact == "Contact Owner"),]
```

#### 📈 Analysis 1.3: Relationship Between Rent and Area Type
Visualize how rent varies across different area types:

```r
ggplot(data = data, mapping = aes(x = Area.Type, y = Rent)) +
  geom_boxplot(col="orange") +
  labs(title = "Distribution of Rent By Area Type")
```

#### 🏠 Analysis 1.4: Average House Rents and Sizes by Point of Contact
Find average rents and house sizes for each point of contact:

```r
ggplot(temp, aes(x = "", y = Avg_Rent, fill = Point.of.Contact)) +
  geom_col() +
  geom_text(aes(label = round(Avg_Rent, 2)), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "Average Rent By Point of Contact")
```

### 🏙️ Relationship Between Rent, City, and Size

#### 🌟 Analysis 2.1: Preferred and Least Preferred Cities
Analyze which cities have the highest and lowest number of rental properties:

```r
City_Count <- data %>% group_by(City) %>% summarise(count = length(BHK)) %>% arrange(desc(count))
ggplot(City_Count, mapping = aes(x= City, y= count, fill = count)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "City", y = "Count", title = "Houses Counted by Cities")
```

#### 📏 Analysis 2.3: Relationship Between House Size and Rent
Explore how the size of the house impacts the rent price:

```r
ggplot(data, aes(x=Size, y=Rent)) + 
  geom_point() + geom_smooth() +
  labs(title = "Relationship Between Size & Rent")
```

### 🛋️ Relationship Between Rent, City, and Furnished Status

#### 🛠️ Analysis 3.1: Preferred Furnishing Status
Determine which furnishing status is most common:

```r
Furnished_Status <- data %>% group_by(Furnishing.Status) %>% summarise(count = length(BHK))
ggplot(Furnished_Status, mapping = aes(x= Furnishing.Status, y= count, fill = count)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Furnished Status", y = "Count", title = "Count By Facilities")
```

#### 💰 Analysis 3.3: City Impact on Rent Prices
Examine how rent prices vary across different cities:

```r
ggplot(data = data, mapping = aes(x = City, y = Rent)) +
  geom_boxplot(col="black") +
  labs(title = "Distribution of Rent By City")
```

### 🏘️ Most Popular Houses per Category

#### 🚿 Analysis 4.1: Most Popular Number of Bathrooms
Find out the most common number of bathrooms in rental properties:

```r
Bathroom_Count <- data %>% group_by(Bathroom) %>% summarise(count = length(BHK)) %>% top_n(5)
ggplot(Bathroom_Count, mapping = aes(x= Bathroom, y= count, fill = count)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Bathrooms", y = "Count", title = "Count Of Bathrooms in 1 House")
```

#### 🏡 Analysis 4.4: Most Popular House Sizes
Analyze the distribution of house sizes:

```r
Size_Count <- data %>% group_by(Size) %>% summarise(count = length(Size)) %>% top_n(8)
ggplot(Size_Count, mapping = aes(x= Size, y= count, fill = count)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Count By Size")
```

### 🌆 Cities with Highest Amounts in Each Category

#### 🏙️ Analysis 5.1: City with Highest Total Amount of BHK
Identify which city has the highest total number of BHK:

```r
Total_Amount_BHK_Per_city <- data %>% group_by(City) %>% summarise(Total_BHK = sum(BHK))
ggplot(Total_Amount_BHK_Per_city, mapping = aes(x= City, y= Total_BHK, fill = Total_BHK)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Amount Of BHK Per City")
```

#### 💵 Analysis 5.3: City with Highest Total Rent
Discover which city generates the highest total rent:

```r
Total_Amount_Rent_Per_city <- data %>% group_by(City) %>% summarise(Total_Rent = sum(Rent))
ggplot(Total_Amount_Rent_Per_city, mapping = aes(x= City, y= Total_Rent, fill = Total_Rent)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total amount Of rent per City")
```

## ✨ Additional Features

### 📊 Feature 1: Correlogram Matrix
Generate a correlogram to visualize correlations between variables:

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
Visualize the distribution of rent with a violin plot:

```r
ggplot(data, aes(x = Rent, y = Size

)) + geom_violin(trim = FALSE)
```

## 📌 Conclusion

This project provides an in-depth exploration of house rent data, uncovering valuable insights into how various factors like area type, city, size, and furnishing status impact rental prices. By utilizing R's powerful data manipulation and visualization capabilities, we gained a comprehensive understanding of the rental market dynamics.


