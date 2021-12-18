library(dplyr)
library(ggplot2)
mytheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15), hjust = (0.5)), 
                 legend.title = element_text(colour = "steelblue",  face = "bold.italic", family = "Helvetica"), 
                 legend.text = element_text(face = "italic", colour="steelblue4",family = "Helvetica", size = (10)), 
                 axis.title = element_text(face = "bold",family = "Helvetica", size = (12), colour = "steelblue4"),
                 axis.text = element_text(face = "bold",family = "Courier", colour = "cornflowerblue", size = (10)))

kolkata_air = city_day %>% filter(V1 == "Kolkata")
NO2= as.numeric(kolkata_air$V6)
NO2=na.omit(NO2)
hist(NO2, probability = TRUE)
lines(density(NO2,bw = 2.5, kernel = "gaussian"),col = 4, lwd=3)
lines(density(NO2, bw = 2.5, kernel = "epanechnikov"), col = 2, lwd=3)
rug(NO2)
CO= as.numeric(kolkata_air$V9)
CO=na.omit(CO)
hist(CO, probability = TRUE)
lines(density(CO, bw = 0.12, kernel = "gaussian"), col = 4, lwd=3)
lines(density(CO, bw = 0.12, kernel = "epanechnikov"), col = 2, lwd=3)
rug(CO)
ggplot(data = city_day, mapping = aes(x = V9, fill = V16)) +
         geom_boxplot() +
         ggtitle("Boxplot") +
         #labs(y = "Total Online Marks (%)") +
         theme_classic() + 
         mytheme 
