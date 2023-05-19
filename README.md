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

