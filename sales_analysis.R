# sales performance analysis
Using R
# Author : Brian
# project: sales performance analysis

# 1. load and inspect the data 
str(R_sales)
summary(R_sales)

# 2. check for missing values
colSums(is.na(R_sales))

# 3. clean missing categorical values 
R_sales$Region[is.na(R_sales$Region)] <- "unknown"
R_sales$Salesperson[is.na(R_sales$Salesperson)] <- "unknown"
# check missing values again
colSums(is.na(R_sales))

# 4. analyze product performance 
# calculate total profit
aggregate(Profit_KES ~ Product, data = R_sales, FUN = sum)

# 5. compare average profit by product
aggregate(Profit_KES ~ Product, data = R_sales, FUN = mean)

# 6. Analyze regional performance 
# calculate total profit by region
aggregate(Profit_KES ~ Region, data = R_sales, FUN = sum)

# 7. Analyze salesperson performance
# calculate total profit by salesperson
aggregate(Profit_KES ~ Salesperson, data = R_sales, FUN = sum)

# 8. Analyze sales volume by salesperson
# calculate total units by salesperson
aggregate(Quantity ~ Salesperson, data = R_sales, FUN = sum)

# 9. Analyze average profit per unit
aggregate(Profit_KES / Quantity ~ Salesperson, data = R_sales, FUN = mean)

# 10. Analyze monthly profit trends
# Extract month from the sales date
R_sales$Month <- format(as.Date(R_sales$Date), "%Y-%m")
# Calculate total profit by month
aggregate(Profit_KES ~ Month, data = R_sales, FUN = sum)

# 11. Analyze profit by product and region
aggregate(Profit_KES ~ Product + Region, data = R_sales, FUN = sum)

# 12. Find the most profitable product in Nairobi 
nairobi_sales <- subset(R_sales, Region == "Nairobi")
aggregate(Profit_KES ~ Product, data = nairobi_sales, FUN = sum)

# 13. Explore the relationship between discount and profit
cor(R_sales$Discount_Rate, R_sales$Profit_KES, use = "complete.obs")

# 14. Visualize discount rate and profit
plot(R_sales$Discount_Rate, R_sales$Profit_KES, main = "Discount Rate vs Profit",
     xlab = "Discount Rate",
     ylab = "Profit (KES)",
     pch = 19)

# 15. Compare average profit by discount rate
aggregate(Profit_KES ~ Discount_Rate, data = R_sales, FUN = mean, na.action = na.omit)

# 16. Identify the highest-profit transaction
head(R_sales[order(-R_sales$Profit_KES),], 10)

# 17. Overall business summary
# total revenue
sum(R_sales$Revenue_KES)
# total cost
sum(R_sales$Cost_KES)
# total profit
sum(R_sales$Profit_KES)

# 18. Calculate overall profit margin
total_profit <- sum(R_sales$Profit_KES)
total_revenue <- sum(R_sales$Revenue_KES)
profit_margin <- (total_profit / total_revenue) * 100

profit_margin


# 19. Visualize total profit by product
product_profit <- aggregate(Profit_KES ~ Product, data = R_sales, FUN = sum)
barplot(product_profit$Profit_KES, names.arg = product_profit$Product,
        main = "total profit by product",
        xlab = "product",
        ylab = "total profit(KES)",
        las = 2)

# 20. Visualize total profit by region
region_profit <- aggregate(Profit_KES ~ Region, data = R_sales, FUN = sum)
barplot(region_profit$Profit_KES, names.arg = region_profit$Region,
        main = "Total profit by Region",
        xlab = "Region",
        ylab = "Total Profit (KES)",
        las = 2)

# 21. Visualize monthly profit
monthly_profit <- aggregate(Profit_KES ~ Month, data = R_sales, FUN = sum)
plot(seq_len(nrow(monthly_profit)),monthly_profit$Profit_KES,
     type = "b",
     xaxt = "n",
     main = "Monthly Profit Trend",
     xlab = "Month",
     ylab = "Total Profit (KES)",
     las = 2)
axis(1, 
     at = seq_len(nrow(monthly_profit)),
     labels = monthly_profit$Month)

# 22. Visualize salesperson performance
salesperson_profit <- aggregate(Profit_KES ~ Salesperson, data = R_sales, FUN = sum)
barplot(salesperson_profit$Profit_KES,
        names.arg = salesperson_profit$Salesperson,
        main = "Total Profit by Salesperson",
        xlab = "Salesperson",
        ylab = "Total Profit (KES)",
        las = 2)






