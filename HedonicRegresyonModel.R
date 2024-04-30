library(readr)
library(dplyr)

# Veriyi yükle
housePrice <- read_csv("C:/.../Desktop/housePrice.csv")

# Veri yapısı
str(housePrice)

# Özet istatistikler
summary(housePrice)


# Her sütundaki eksik değer sayısını hesaplayın
missing_values <- colSums(is.na(housePrice))

print(missing_values)

library(dplyr)

housePrice <- housePrice %>%
  select(-Address, -`Price(USD)`)

# Her sütundaki eksik değer sayısını hesaplayın
missing_values <- colSums(is.na(housePrice))

print(missing_values)

# Veri yapısı
str(housePrice)

# Parking dummy değişkeni oluşturma
housePrice$Parking_dummy <- ifelse(housePrice$Parking == TRUE, 1, 0)

# Warehouse dummy değişkeni oluşturma
housePrice$Warehouse_dummy <- ifelse(housePrice$Warehouse == TRUE, 1, 0)

# Elevator dummy değişkeni oluşturma
housePrice$Elevator_dummy <- ifelse(housePrice$Elevator == TRUE, 1, 0)

new_df <- housePrice %>%
  select(Area, Room, Price, Parking_dummy, Warehouse_dummy, Elevator_dummy)

# Lineer regresyon modeli oluşturma
model <- lm(Price ~ Area + Room + Parking_dummy + Warehouse_dummy + Elevator_dummy, data = new_df)

# Room dummy değişkenlerini oluşturma
for (i in 0:5) {
  new_df[paste0("Room_", i)] <- ifelse(new_df$Room == i, 1, 0)
}

# Modeli güncelleme
model <- lm(Price ~ Area + Room_1 + Room_2 + Room_3 + Room_4 + Room_5 +
              Parking_dummy + Warehouse_dummy + Elevator_dummy, data = new_df)

summary(model)
#library(ggplot2)
library(dplyr)
library(tidyr)

# Veri setinizdeki tüm değişkenlerin adlarını alın
vars <- names(housePrice)

# Veri setinizdeki tüm değişkenlerin dağılım grafiğini tek bir grafik içinde çizin
plots <- lapply(vars, function(var) {
  ggplot(housePrice, aes_string(x = var)) +
    geom_bar(fill = "skyblue", color = "black") +
    labs(title = paste(var, "Distribution"),
         x = var,
         y = "Frequency") +
    theme_minimal()
})

# Grafiği tek bir çerçevede düzenleme
multiplot <- do.call(gridExtra::grid.arrange, c(plots, ncol = 3))



library(corrplot)

correlation_matrix <- cor(new_df)
corrplot(correlation_matrix, method = "circle")


#ggplot(new_df, aes(x = Area, y = Price)) +
#  geom_point() +
#  labs(title = "Scatter Plot of Area vs. Price",
#       x = "Area",
#       y = "Price")



predicted_values <- predict(model, new_df)

# Gerçek ve öngörülen değerlerin plot'u
plot(new_df$Price, predicted_values,
     xlab = "Gerçek Değerler",
     ylab = "Modelin Öngörüsü",
     main = "Gerçek Değerler vs. Modelin Öngörüsü",
     col = "blue")  # Öngörülen değerleri mavi renkte göster

# Gerçek değerlerin ve öngörülen değerlerin birbirine eşit olduğu çizgiyi çiz
abline(0, 1, col = "red")  # Eşitlik çizgisi kırmızı renkte göster

