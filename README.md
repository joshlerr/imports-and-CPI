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
Foodimports<-read_excel("FoodImports main.xlsx")
PPIforecast<-read_excel("PPIForecast.xlsx")
joined_table <- left_join(Foodimports, PPIforecast, by = "year")  
```  
# Visualizations and pivot tables  
1 To make different visualizations and analysis on the dataset, we had to create pivot tables which would arrange the dataset into different parts.  
```r
meat_dairy <- subset(joined_table, select=c("year", "Meats", "Dairy"))
veg_fruit <- subset(joined_table, select=c("year", "Vegetables", "Fruits"))
meat_fruit<-subset(joined_table, select =c("year","Meats","Fruits"))
meat_dairy_long <- gather(meat_dairy, key="category", value="cpi", Meats:Dairy)
veg_fruit_long <- gather(veg_fruit, key="category", value="cpi", Vegetables:Fruits)
meat_fruit_long<- gather(meat_fruit, key = "category", value = "cpi", Meats:Fruits)
``` 
as we can see above, we used a new function that we havent used before called "subset". it is used to extract subsets of data from a data frame or a vector based on specified conditions. The subset() function is a convenient way to filter or select specific rows or columns of a dataset based on logical expressions. so using these functions, we were able to organize the dataset into different groups(with their price and cpi).  
2. visualize the pivot tables using ggplot functions. i used different ggplot functions, but the one that got my attention was charting two graphs side by side.  
# Line Charts 
```r
import_price_chart<-ggplot(data = joined_table, aes(x = year)) +
  geom_line(aes(y = Meats, color = "Meat"), size = 1) +
  geom_line(aes(y = Dairy, color = "Dairy"), size = 1) +
  geom_line(aes(y = Nuts, color = "Nuts"), size = 1) +
  geom_line(aes(y = Fruits, color = "Fruits"), size = 1) +
  labs(x = "Year", y = "Import Price", color = "") +
  scale_color_manual(values = c("red", "blue", "green", "purple"), name = "") +
  theme_classic() +
  labs(title = "Import Prices for Meat, Dairy, Nuts, and Fruit")
  coord_cartesian(ylim = c(0, max(joined_table[, c("Meats", "Dairy", 
                                              "Nuts", "Fruits")], na.rm = TRUE)*1.1))
                                             
cpi_chart <- ggplot(data = joined_table, aes(x = year)) +
  geom_line(aes(y = MeatsCPI, color = "Meat CPI"), size = 1) +
  geom_line(aes(y = DairyCPI, color = "Dairy CPI"), size = 1) +
  geom_line(aes(y = NutsCPI, color = "Nuts CPI"), size = 1) +
  geom_line(aes(y = FruitsCPI, color = "Fruits CPI"), size = 1) +
  labs(x = "Year", y = "CPI", color = "") +
  scale_color_manual(values = c("red", "blue", "green", "purple"), 
                     name = "", 
                     labels = c("Meat", "Dairy", "Nuts", "Fruits")) +
  theme_classic() +
  labs(title = "CPI Comparison") +
  coord_cartesian(ylim = c(0, max(joined_table[, c("MeatsCPI", "DairyCPI", "NutsCPI", "FruitsCPI")], na.rm = TRUE)*1.1))

# combine the two charts using gridExtra
grid.arrange(import_price_chart, cpi_chart, ncol = 2)
```  
the grid arrange function is a function used specifially to arrange the two line charts used above side by side. and the line chart used here is very important since it represents the project all in all. the chart in the left shows the increase in price while the chart in the right shows the change in cpi. from this, we can see that if the change in cpi (or the increase in price of food in the US from year to year) is because of imports of goods and services.  
# Prediction Model  
one of the major thing required for this project was making a model. to make this model, we also had to see researches that had been done in previous years to make model predictions about food imports and consumer price index. the model we used as a reference was called "Forecasting Import Prices of Basic Foodstuffs in the Caribbean Community (CARICOM) Using Univariate Time Series Models," published in the Journal of Economics and Sustainable Development in 2014. in this model, they used the price increase and food import amount to make a linear regression model. we also used regression models to make two models, one representing meat prodcuts, the other representing vegetable product.  
```r 
model <- lm(Meats ~ MeatsCPI, data = joined_table)
# make prediction on meat products
new_data <- data.frame(MeatsCPI = c(120, 125, 130)) # new data to predict on
predicted_import_price <- predict(model, newdata = new_data)
plot(joined_table$MeatsCPI, joined_table$Meats, main = "Import Price of Meat vs. CPI of Meat products", xlab = "CPI of Meat", ylab = "Import Price of Meat")
abline(model, col = "red")
#make prediction on vegetable products
models <- lm(Vegetables ~ VegetablesCPI, data = joined_table)
new_data <- data.frame(VegetablesCPI = c(120, 125, 130)) # new data to predict on
predicted_import_price <- predict(models, newdata = new_data)
plot(joined_table$VegetablesCPI, joined_table$Vegetables, main = "Import Price of vegetables vs. CPI of vegetable products", xlab = "CPI of Meat", ylab = "Import Price of Meat")
abline(models, col = "blue")  
```  
# Citation of source used 
Huang, K. S. (Year). Forecasting Consumer Price Indexes for Food: A Demand Model Approach. Technical Bulletin No. 1883. Food and Rural Economics Division,   Economic Research Service, U.S. Department of Agriculture.




