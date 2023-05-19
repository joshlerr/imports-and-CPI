# Imports-and-CPI 🥝🥩 📈 📉
**Contributors**
  *Josh Yenesew, Rebecca Moges, Abdou Chaib*
  # Introduction 
  ![tin](https://github.com/joshlerr/imports-and-CPI/assets/118494139/4dea2f6c-6684-4b9f-870f-e3085bc333aa)


  In this project, we will be analyzing the price of imported goods and services and their consumption price index. we want to see if the price of goods
affected by the amount of imports. some questions we want to answer, are.... Does the price of goods in the US increase as import increase? or is it not affected at all? And if it is affected by import, how much? which products are heavily affected by price and what products are slightly affected?

# Data Dictionary 
1. Food Import: Food brought from abroad for sale
2. Consumer Price Index (CPI): The measure of average change overtime in the prices paid by typical consumers for retail goods and other items
# Data Cleaning  
cleaning the dataset was a little tricky. during the beginning of our data class, we talked about tidy data and we said that every column is a variable and every row is an observation. the data set we had involved columns with observations and rows with variables.  
1. we changed every column into a variable and every row into observation.  
2. we joined the two datasets we had using thier common key and left_joining them.  
```r
joined_table <- left_join(Foodimports, PPIforecast, by = "year")  
```  
# Visualizations and pivot tables  
To make different visualizations and analysis on the dataset, we had to create pivot tables which would arrange the dataset into different parts.  
```r
meat_dairy <- subset(joined_table, select=c("year", "Meats", "Dairy"))
veg_fruit <- subset(joined_table, select=c("year", "Vegetables", "Fruits"))
meat_fruit<-subset(joined_table, select =c("year","Meats","Fruits"))
meat_dairy_long <- gather(meat_dairy, key="category", value="cpi", Meats:Dairy)
veg_fruit_long <- gather(veg_fruit, key="category", value="cpi", Vegetables:Fruits)
meat_fruit_long<- gather(meat_fruit, key = "category", value = "cpi", Meats:Fruits)
``` 
as we can see above, we used a new function that we havent used before called "subset". it is used to extract subsets of data from a data frame or a vector based on specified conditions. The subset() function is a convenient way to filter or select specific rows or columns of a dataset based on logical expressions. so using these functions, we were able to organize the dataset into different groups(with their price and cpi). 


