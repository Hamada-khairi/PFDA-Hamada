# TP065783 : KHALED AWAD 
# TP064361 : ABDELRAHMAN MOURAD
# TP066168 : MOHAMED KHAIRY


# Installing Packages.
install.packages("dplyr")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("plotly")
install.packages("tidyr")
install.packages("tidyverse")
install.packages("caTools")

# Loading libraries
library(dplyr)
library(ggplot2)
library(corrplot)
library(plotly)
library(tidyr)
library(tidyverse)
library(caTools)

## Loading the data set.

data <- read.csv("E:\\OneDrive - Asia Pacific University\\Degree 2\\Sem 1\\PFDA\\ASSIGHMENT\\House_Rent_Dataset.csv")
head(data)


## Checking for missing values

colSums(is.na(data))

dim(data)


## Checking for garbage values

unique(data$Area.Type)
unique(data$City)
unique(data$Furnishing.Status)
unique(data$Tenant.Preferred)
unique(data$Point.of.Contact)


summary(data)


## Remove Outlines

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


head(data)


# Q1 What is the relationship between Rent,Area type and point of contact?

# Analysis 1.1
# Q what are the houses that only have (Contact owner) point of contact?

data[,12]
data[which(data$Point.of.Contact=="Contact Owner"),]





# Analysis 1.2
# Find the average and max of the rent?

 mean(data$Rent)
 max(data$Rent)





# Analysis 1.3 
#Q Find the relationship between Rent and Area type?

ggplot(data = data, 
       mapping = aes(x = Area.Type, 
                     y = Rent)) +
  geom_boxplot(col="orange") +
  labs(title = "Distribution of Rent By Area Type")




# Analysis 1.4 
#Q Find average house rents and average house sizes for point of contact?

temp <- data %>%
  group_by(Point.of.Contact) %>%
  summarise(Avg_Size = mean(Size),
            Avg_Rent = mean(Rent))

ggplot(temp, aes(x = "",
                 y = Avg_Rent, 
                 fill = Point.of.Contact)) +
  geom_col() +
  geom_text(aes(label = round(Avg_Rent, 2)),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "Average Rent By Point of Contact")

ggplot(temp, aes(x = "",
                 y = Avg_Size, 
                 fill = Point.of.Contact)) +
  geom_col() +
  geom_text(aes(label = round(Avg_Size, 2)),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "Average House Sizes By Point of Contact")




#Q2 What is the relationship between Rent, city and size?

# Analysis 2.1
# What is the preferred city & What is the least Preferred City?

City_Count<-data%>%group_by(City)%>%summarise(count = length(BHK))%>%arrange(desc(count))
head(City_Count)
ggplot(City_Count, 
       mapping = aes(x= City,
                     y= count,
                     fill = count)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "City" ,y = "Count",title = "Houses Counted by Cities") + theme_bw()






# Analysis 2.2
# Q Find The Rent Per Size?
data$Rent_per_size<-data$Rent/data$Size
data






# Analysis 2.3 
#Q Find the relationship between house size and house rent?

ggplot(data, aes(x=Size, y=Rent)) + 
  geom_point()+
  geom_smooth() +
  labs(title = "Relationship Between Size & Rent")



# Analysis 2.4 
#Q Find The average house sizes for each city?

temp <- data %>%
  group_by(City) %>%
  summarise(Avg_Size = mean(Size))

ggplot(data = temp, 
       mapping = aes(x = City, 
                     y = Avg_Size, 
                     fill = City)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Average House Sizes By City")





#Q3 What is The Relationship  between Rent,City & Furnished Status?

# Analysis 3.1
# Q What is the preferred Furnished Status & What is the least Furnished Status?

Furnished_Status<-data%>%group_by(Furnishing.Status)%>%summarise(count = length(BHK))%>%top_n(5)
head(Furnished_Status)
ggplot(Furnished_Status, 
       mapping = aes(x= Furnishing.Status,
                     y= count,
                     fill = count)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Furnished Status" ,y = "Count",title = "Count By Facilities") + theme_bw()









# Analysis 3.2
# Q what are the houses that only have (Unfurnished) Furnishing Status in Mumbai City?

data[which(data$City=="Chennai" & data$Furnishing.Status=="Semi-Furnished"),]







# ## Analysis 3.3 
#Q Does the city affect on the rent price ?

ggplot(data = data, 
       mapping = aes(x = City, 
                     y = Rent)) +
  geom_boxplot(col="black") +
  labs(title = "Distribution of Rent By City")






# Analysis 3.4 
# Q Find the average house rent and house sizes are calculated by furnished status of a house?

temp <- data %>%
  group_by(Furnishing.Status) %>%
  summarise(Avg_Size = mean(Size),
            Avg_Rent = mean(Rent))

ggplot(temp, aes(x = "",
                 y = Avg_Rent, 
                 fill = Furnishing.Status)) +
  geom_col() +
  geom_text(aes(label = round(Avg_Rent, 2)),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "Average Rent By Furnishing Status")

ggplot(temp, aes(x = "",
                 y = Avg_Size, 
                 fill = Furnishing.Status)) +
  geom_col() +
  geom_text(aes(label = round(Avg_Size, 2)),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "Average House Sizes By Furnishing Status")








# Q4 What are the most popular houses per category?

# Analysis 4.1
# Q What are the most popular amount of bathrooms?
Area_Count<-data%>%group_by(Area.Type)%>%summarise(count = length(BHK))
head(Area_Count)
ggplot(Area_Count, 
       mapping = aes(x= Area.Type,
                     y= count,
                     fill = count)) +
  geom_bar(stat = "identity") +
  labs(x = "Area Type" ,y = "Count",title = "Count Of Houses By Area Type") + theme_bw()






# Analysis 4.2
# Q  What are the most popular area types ?

Bathroom_Count<-data%>%group_by(Bathroom)%>%summarise(count = length(BHK))%>%top_n(5)
head(Bathroom_Count)
ggplot(Bathroom_Count, 
       mapping = aes(x= Bathroom,
                     y= count,
                     fill = count)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Bathrooms" ,y = "Count",title = "Count Of Bathrooms in 1 House") + theme_bw()






# Analysis 4.3
# Q What are the most popular area of Facilities?
Facilities_Count<-data%>%group_by(Area.Locality)%>%summarise(count = length(BHK))%>%top_n(5)
head(Facilities_Count)
ggplot(Facilities_Count, 
       mapping = aes(x= Area.Locality,
                     y= count,
                     fill = count)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Area_Facilities" ,y = "Count",title = "Count By Facilities") + theme_bw()




# Analysis 4.4
# Q What are the most popular house sizes?
Size_Count<-data%>%group_by(Size)%>%summarise(count = length(Size))%>%top_n(8)
head(Size_Count)
ggplot(Size_Count, 
        mapping = aes(x= Size,
                      y= count,
                      fill = count)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Count By Size") + theme_bw()






# Q5 What are the cities witch has the highest amounts of each category?

# Analysis 5.1
# Q Which city has the highest total amount of BHK per city?

Total_Amount_BHK_Per_city<-data%>%group_by(City)%>%summarise(Total_BHK = sum(BHK))
head(Total_Amount_BHK_Per_city)
ggplot(Total_Amount_BHK_Per_city, 
       mapping = aes(x= City,
                     y= Total_BHK,
                     fill = Total_BHK)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Amount Of BHK Per City") + theme_bw()








# Analysis 5.2
# Q Which city has the highest total amount of each area type per city?

Amount_Areatype_Per_city<-data%>%group_by(Area.Type,City)%>%summarise(count = length(City))
head(Amount_Areatype_Per_city)
ggplot(Amount_Areatype_Per_city, 
       mapping = aes(x= Area.Type,
                     y= count,
                     fill = City)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Count Of Area Type Per City") + theme_bw()





# Analysis 5.3
# Q  Which city has the highest total amount of rent per city?

Total_Amount_Rent_Per_city<-data%>%group_by(City)%>%summarise(Total_Rent = sum(Rent))
head(Total_Amount_Rent_Per_city)
ggplot(Total_Amount_Rent_Per_city, 
       mapping = aes(x= City,
                     y= Total_Rent,
                     fill = Total_Rent)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total amount Of rent per City") + theme_bw()






# Analysis 5.4
# Q  Which city has the highest total amount of each furnishing status per city?

Total_Furnishing_Per_city<-data%>%group_by(Furnishing.Status,City)%>%summarise(count = length(City))
head(Total_Furnishing_Per_city)
ggplot(Total_Furnishing_Per_city, 
       mapping = aes(x= Furnishing.Status,
                     y= count,
                     fill = City)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Amount Of Each furnishing Status Per City") + theme_bw()











#Additional feature 1 Create Correlogram Matrix 

Correlogram_Matrix<-cor(data[,c(2,3,4,11)])
head(Correlogram_Matrix)
corrplot(Correlogram_Matrix,addCoef.col = TRUE)






#Additional feature 2 create a scatter plot graph with ab line for rent by size

attach(data)
plot (Size, Rent, main = "Scatterplot of rent vs size", xlab = "House size", ylab ="House rent")
abline(lm(Rent ~ Size), col ="blue", lwd =  2)






 
#Additional feature 3 create a violin graph for rent by size 

rent_size <- ggplot (data, aes (x = Rent, y = Size)) + geom_violin()
head(rent_size)
ggplot (data, aes (x = Rent, y = Size)) + geom_violin(trim = FALSE)





