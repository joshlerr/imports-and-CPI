library(tidyverse)
library(rpart)
library(readxl)
library(rpart.plot)
library(leaflet)
library(shinyjs)
library(shiny)
library(ggplot2)
library(lubridate)
library(tidytext)
library(textdata)
library(dplyr)
library(gridExtra)
rm(list=ls())

#setwd("C:/slick project")
Foodimports<-read_excel("FoodImports main.xlsx")
PPIforecast<-read_excel("PPIForecast.xlsx")
joined_table <- left_join(Foodimports, PPIforecast, by = "year")


meat_dairy <- subset(joined_table, select=c("year", "Meats", "Dairy"))
veg_fruit <- subset(joined_table, select=c("year", "Vegetables", "Fruits"))
meat_fruit<-subset(joined_table, select =c("year","Meats","Fruits"))
meat_dairy_long <- gather(meat_dairy, key="category", value="cpi", Meats:Dairy)
veg_fruit_long <- gather(veg_fruit, key="category", value="cpi", Vegetables:Fruits)
meat_fruit_long<- gather(meat_fruit, key = "category", value = "cpi", Meats:Fruits)

ggplot(meat_dairy_long, aes(x=year, y=cpi, fill=category)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Meat and Dairy price by Year", x="Year", y="price in dollars")
your_colors<- c("blue", "grey")
ggplot(veg_fruit_long, aes(x=year, y=cpi, fill=category)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Vegetable and Fruit price by Year", x="Year", y="price in dollars")+
  scale_fill_manual(values=your_colors)
my_colors <- c("light blue", "red")
ggplot(meat_fruit_long, aes(x=year, y=cpi, fill=category)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Meat and Fruits imported price by Year", x="Year", y="price in dollars")+
  scale_fill_manual(values=my_colors)
#lets start comparing some of the products
ggplot(data = joined_table, aes(x = year)) +
  geom_bar(aes(y = Meats, fill = "Meat"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Fruits, fill = "Fruits"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Vegetables, fill = "Vegetables"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Nuts, fill = "Nuts"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Dairy, fill = "Dairy"), stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Import Price", fill = "") +
  scale_fill_manual(values = c("red", "green", "blue", "yellow", "purple"), name = "") +
  theme_classic()+
  labs(title = "Comparison of Import Prices for Meat, Fruits, Vegetables, Nuts, and Dairy")

ggplot(data = joined_table, aes(x = year, y = Fruits)) +
  geom_line() +
  labs(x = "Year", y = "Fruit Import Price")

#scatterplot
ggplot(data = joined_table, aes(x = Meats, y = MeatsCPI)) +
  geom_point() +
  labs(x = "Meats", y = "MeatsCPI")

#bar charts
ggplot(data = joined_table, aes(x = year, y = Beverages, fill = BeveragesCPI)) +
  geom_bar(stat = "identity") +
  labs(title = "Total beverages import price and their CPI over the years") +
  labs(x = "Year", y = "Beverages Import price", fill = "Beverages CPI")


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


# create CPI chart
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

#scatterplot
ggplot(data = joined_table, aes(x = year, y = MeatsCPI)) +
  geom_point() +
  labs(x = "year", y = "MeatsCPI")
long_data <- gather(joined_table, key = "category", value = "cpi", MeatsCPI:FruitsCPI)
# Create scatter plot
ggplot(long_data, aes(x=year, y=cpi, fill=category)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="The consumer price index of different products", x="Year", y="CPI")
import_price_chart<-ggplot(data = joined_table, aes(x = year)) +
  geom_line(aes(y = Meats, color = "Meat"), size = 1) +
  geom_line(aes(y = Dairy, color = "Dairy"), size = 1) +
  geom_line(aes(y = Nuts, color = "Nuts"), size = 1) +
  geom_line(aes(y = Fruits, color = "Fruits"), size = 1) +
  labs(x = "Year", y = "Import Price", color = "commodity") +
  scale_color_manual(values = c("red", "blue", "green", "purple"), name = "",
                                   labels = c("Meats", "Dairy","Nuts","Fruits")) +
  theme_classic() +
  labs(title = "Import Prices for Meat, Dairy, Nuts, and Fruit")
coord_cartesian(ylim = c(0, max(joined_table[, c("Meats", "Dairy", "Nuts", "Fruits")], na.rm = TRUE)*1.1))
# create CPI chart
  cpi_chart <- ggplot(data = joined_table, aes(x = year)) +
  geom_line(aes(y = MeatsCPI, color = "Meat CPI"), size = 1) +
  geom_line(aes(y = DairyCPI, color = "Dairy CPI"), size = 1) +
  geom_line(aes(y = NutsCPI, color = "Nuts CPI"), size = 1) +
  geom_line(aes(y = FruitsCPI, color = "Fruits CPI"), size = 1) +
  labs(x = "Year", y = "CPI", color = "commodity") +
  scale_color_manual(values = c("red", "blue", "green", "purple"), name = "", labels = c("Meat", "Dairy", "Nuts", "Fruits")) +
theme_classic() +
labs(title = "CPI Comparison") +
coord_cartesian(ylim = c(0, max(joined_table[, c("MeatsCPI", "DairyCPI", "NutsCPI", "FruitsCPI")], na.rm = TRUE)*1.1))
# combine the two charts using gridExtra
grid.arrange(import_price_chart, cpi_chart, ncol = 2)

ggplot(meat_fruit_long, aes(x=year, y=cpi, color=category)) +
  geom_line() +
  labs(title="Meat and Fruit CPI by Year", x="Year", y="price")
ggplot(data = joined_table, aes(x = year, y = Grains)) +
  geom_line() +
  labs(x = "Year", y = "Grain Import Price")
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


ui <- fluidPage(
  # Create three tabs
    titlePanel("The connection between food imports and their consumer price index"),
  tabsetPanel(
    tabPanel("Bar Charts",
                           # Add a description for each bar chart
     h4("Beverages which is mainly alcohol"),
      p("This is a description of the beverages imported by the united states and their prices. most of the beverages were alcohol and we can see that the united states has spent an increasing amount of money on importing. the only decrease we see is at 2009 and that was due to the effect of the great depression"),
      plotOutput("bar1"),
      h4("comparing different imported products relative to its price."),
      p("we can see that vegetables have the high mark in imports as the government spent a lot on importing vegetables. The next is fruits, and then goes dairy, then Nuts. the highest import is seen at 2021. this is mostly because of covid and most production in the united states was halted."),
     plotOutput("bar2"),
     h4("Meat and Dairy Price"),
     p("While we compare the price of Meat snd Dairy, we can see that there is a substancial difference since the price of meat is way bigger than the price of dairy.and we also can see that there is a fluctuation in the spendig on import of meat throught the two decades. 2022 has the highest import though."),
     plotOutput("bar3"),
     h4("vegetables and fruits"),
     p("The other thing that interested me was the comparison between fruits and vegetables. we can see that there is a constant increase in the price of Fruits and vegetables, fruits substantially being bigger than the price of vegetables."),
     plotOutput("bar4"),
     h4("Meat and fruit comparison"),
     p("then after we saw that fruits and meat products have the highest prices, we now compare fruits and meat products. then united states is known to have different types of meat products produced locally, so there is no need to import more. that is why there is more imports of fruuits than there is of Meat products."),
     plotOutput("bar5")
     ),
    tabPanel("Line Charts",
    # Add a description for each line chart
    h4("Meat cpi over the years"),
    p("the consumer price index of meat is very high for meat products that have been bought in 2005 and 2017, while comparatively lower in 2007 and 2021. what we can see here is, as there is a very low cpi in a current year, there will be a significant increase in CPI"),
   plotOutput("line1"),
   h4("the cpi of major products throughout the years"),
   p("Yes, this is not a line chart, but it does describe the cpi of different prodcuts over the years. some products like dairy had high cpi in early two thousands but seemed to be decreasing as the years go by, other products like vegetetables had very low cpi in the early two thousands but increased over time"),
   plotOutput("line2"),
   h4("import prices of different products with their respective CPI"),
   p("This is by the far the most important line chart. as seen from the line charts as some of the products price increase, their cpi increases. and as some of the prices of other prodcuts of other prodcuts increases, their cpi increases.these charts show if the prodcuts have direct or inverse relationship with CPI"),
  plotOutput("line3"),
   h4("meat and fruit price over the Years"),
   p("This is a simple line chart describing the products price and progress over the years."),
  plotOutput("line4")
  ),
  tabPanel("Model Chart",
  # Add a description for each model chart
  h4("Meat cpi linear regression model"),
  plotOutput("model1"),
  p("we chose to use this model because regression lines are very vital in predicting CPI. In this model, the meat CPI serves as the dependent variable, while other independent variables, such as time or economic factors, may be considered. The regression model estimates the relationship between these variables and predicts the impact on meat CPI. A downward trend in the regression line indicates that as the independent variable(s) increase, the meat CPI tends to decrease. This model helps to understand and quantify the relationship between the meat CPI and various factors, aiding in making informed decisions and predictions regarding the pricing dynamics of meat products."),
  h4("Model Chart 2"),
  plotOutput("model2"),
  p("The vegetable CPI is the dependent variable, while independent variables such as time or economic indicators are considered. The regression model estimates the relationship between these variables and predicts their impact on vegetable CPI. An upward trend in the regression line indicates that as the independent variable(s) increase, the vegetable CPI tends to increase as well. This model helps to analyze and quantify the relationship between the vegetable CPI and various factors, providing insights for decision-making and predictions related to the pricing dynamics of vegetable products. source used is Ankomah, M. K., Alavalapati, J. R., & Adamowski, J. (2014). Forecasting Import Prices of Basic Foodstuffs in the Caribbean Community (CARICOM) Using Univariate Time Series Models. Journal of Economics and Sustainable Development, 5(26), 46-61.")
  )
 )
)
server <- function(input, output) {
# Load your dataset here
# Generate the bar charts
output$bar1 <- renderPlot({
 #Create your first bar chart using ggplot2
  ggplot(data = joined_table, aes(x = year, y = Beverages, fill = Beverages)) +
        geom_bar(stat = "identity") +
        labs(title = "Total beverages import price by their year") +
        labs(x = "Year", y = "Beverages Import price", fill = "Beverages")
      })
    output$bar2 <- renderPlot({
      # Create your 2nd bar chart using ggplot2
        ggplot(data = joined_table, aes(x = year)) +
        geom_bar(aes(y = Fruits, fill = "Fruits"), stat = "identity", position = "dodge") +
        geom_bar(aes(y = Vegetables, fill = "Vegetables"), stat = "identity", position = "dodge") +
        geom_bar(aes(y = Nuts, fill = "Nuts"), stat = "identity", position = "dodge") +
        geom_bar(aes(y = Dairy, fill = "Dairy"), stat = "identity", position = "dodge") +
        labs(x = "Year", y = "Import Price", fill = "") +
        scale_fill_manual(values = c("red", "green", "yellow", "purple"), name = "") +
        theme_classic()+
        labs(title = "Comparison of Import Prices for Fruits, Vegetables, Nuts, and Dairy")
      })
    output$bar3 <- renderPlot({
      # Create your first bar chart using ggplot2
        ggplot(meat_dairy_long, aes(x=year, y=cpi, fill=category)) +
        geom_bar(stat="identity", position="dodge") +
        labs(title="Meat and Dairy price by Year", x="Year", y="price in dollars")
      })
    output$bar4 <- renderPlot({
      # Create your first bar chart using ggplot2
        ggplot(veg_fruit_long, aes(x=year, y=cpi, fill=category)) +
        geom_bar(stat="identity", position="dodge") +
        labs(title="Vegetable and Fruit price by Year", x="Year", y="price in dollars")+
        scale_fill_manual(values=your_colors)
      })
    output$bar5 <- renderPlot({
      # Create your first bar chart using ggplot2
        ggplot(meat_fruit_long, aes(x=year, y=cpi, fill=category)) +
        geom_bar(stat="identity", position="dodge") +
        labs(title="Meat and Fruits imported price by Year", x="Year", y="price in dollars")+
        scale_fill_manual(values=my_colors)
      })
    # Generate the line charts
      output$line1 <- renderPlot({
        # Create your first line chart using ggplot2
          ggplot(data = joined_table, aes(x = year, y = MeatsCPI)) +
          geom_point() +
          labs(x = "year", y = "MeatsCPI")
        })
    output$line2 <- renderPlot({
      # Create your first line chart using ggplot2
        ggplot(long_data, aes(x=year, y=cpi, fill=category)) +
        geom_bar(stat="identity", position="dodge") +
        labs(title="The consumer price index of different products", x="Year", y="CPI")
      })
    output$line3 <- renderPlot({
      # Create your first line chart using ggplot2
      import_price_chart<-ggplot(data = joined_table, aes(x = year)) +
        geom_line(aes(y = Meats, color = "Meat"), size = 1) +
        geom_line(aes(y = Dairy, color = "Dairy"), size = 1) +
        geom_line(aes(y = Nuts, color = "Nuts"), size = 1) +
        geom_line(aes(y = Fruits, color = "Fruits"), size = 1) +
        labs(x = "Year", y = "Import Price", color = "commodity") +
        scale_color_manual(values = c("red", "blue", "green", "purple"), name = "",
                           labels = c("Meats", "Dairy","Nuts","Fruits")) +
        theme_classic() +
        labs(title = "Import Prices for Meat, Dairy, Nuts, and Fruit")
      coord_cartesian(ylim = c(0, max(joined_table[, c("Meats", "Dairy", "Nuts", "Fruits")], na.rm = TRUE)*1.1))
      # create CPI chart
      cpi_chart <- ggplot(data = joined_table, aes(x = year)) +
        geom_line(aes(y = MeatsCPI, color = "Meat CPI"), size = 1) +
        geom_line(aes(y = DairyCPI, color = "Dairy CPI"), size = 1) +
        geom_line(aes(y = NutsCPI, color = "Nuts CPI"), size = 1) +
        geom_line(aes(y = FruitsCPI, color = "Fruits CPI"), size = 1) +
        labs(x = "Year", y = "CPI", color = "commodity") +
        scale_color_manual(values = c("red", "blue", "green", "purple"), name = "", labels = c("Meat", "Dairy", "Nuts", "Fruits")) +
        theme_classic() +
        labs(title = "CPI Comparison") +
        coord_cartesian(ylim = c(0, max(joined_table[, c("MeatsCPI", "DairyCPI", "NutsCPI", "FruitsCPI")], na.rm = TRUE)*1.1))
      # combine the two charts using gridExtra
      grid.arrange(import_price_chart, cpi_chart, ncol = 2)
        })
    output$line4 <- renderPlot({
      # Create your first line chart using ggplot2
        ggplot(meat_fruit_long, aes(x=year, y=cpi, color=category)) +
        geom_line() +
        labs(title="Meat and Fruit CPI by Year", x="Year", y="CPI")
      })
    # Generate the model charts
      output$model1 <- renderPlot({
        plot(joined_table$MeatsCPI, joined_table$Meats, main = "Import Price of Meat vs. CPI of Meat products", xlab = "CPI of Meat", ylab = "Import Price of Meat")
        abline(model, col = "red")
        })
   output$model2 <- renderPlot({
      plot(joined_table$VegetablesCPI, joined_table$Vegetables, main = "Import Price of vegetables vs. CPI of vegetable products", xlab = "CPI of Meat", ylab = "Import Price of Meat")
      abline(models, col = "blue")
      })
    }
shinyApp(ui, server)
