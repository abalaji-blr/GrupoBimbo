# perform data analysis on Grupo Bimbo data set

library("data.table")

# training data is 3GB, generate smaller train for early analysis.
#
# system(dt <- fread("./train.csv", header = TRUE))

# library("dplyr")
# new_train <- sample_frac(dt, 0.15)
# write.csv(new_train, file = "new_train.csv", row.names = FALSE, quote = FALSE)

system(dt <- fread("./new_train.csv", header = TRUE))

#load ggplot2
library("ggplot2")

# per week, h
ggplot(dt, aes(x = Semana)) + geom_histogram()

# returns per week

returns <- dt[, .( total_returns = sum(Dev_uni_proxima)), by = Semana]

ggplot(returns, aes(x=Semana, y = total_returns)) + geom_line()


# product wise sale (units) & return info.

prodReturn <- dt[, .(sales = sum(Venta_uni_hoy), returns = sum(Dev_uni_proxima)), by = Producto_ID]

# product wise returns
ggplot(prodReturn, aes(x = Producto_ID, y = returns, color = returns)) + geom_point() +
  scale_color_gradient(low="blue", high="red")

# product-wise sales (units)
ggplot(prodReturn, aes(x = Producto_ID, y = sales)) + geom_point()



# demand vs sales
# Demanda_uni_quil vs Venta_hoy
ggplot(dt %>% sample_frac(0.05),aes(x = Venta_uni_hoy , y = Demanda_uni_equil)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  scale_x_continuous(name = "Sales") +
  scale_y_continuous(name = "Demand") +
  ggtitle("Demand Vs Sales (in terms of units)")

# correlation info
cor(dt$Demanda_uni_equil, dt$Venta_hoy)
cor(dt$Demanda_uni_equil, dt$Venta_uni_hoy)
cor(dt$Demanda_uni_equil, dt$Dev_uni_proxima)
cor(dt$Demanda_uni_equil, dt$Dev_proxima)
