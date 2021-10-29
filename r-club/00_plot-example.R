
## Cargar nuestra base de datos
library(readxl) 


data <- read_excel("Data/ecological_monitoring.xlsx", sheet= "Data")

## Graficas

#La más simple con la función plot()
plot(data$Year, data$Month, col="red", bg="black" , pch= 21)
plot(data$Size, data$Quantity)

#Grafica de barras
library(tidyverse)
library(ggplot2)

#Grafica de barras donde no se 
#representa la Quantity real del Size
bar1 <- data %>% # %>% = pipe, se forma con  CTRL SHIFT M 
  ggplot(aes(x=Size))+
  geom_bar(fill="red")+
  labs(title= "Size: row count")

#Una forma de graficas Size y Quantity
bar2 <- data %>%
  group_by(Size) %>% 
  mutate(Quantity= sum(Quantity)) %>% 
  distinct(Size, .keep_all=TRUE) %>% 
  select(Size, Quantity) %>% 
  ggplot(aes(x=Size, y=Quantity))+
  geom_bar(stat='identity', fill="green")+
  labs(title="Size quantity")


bar3 <- data %>% 
  ggplot(aes(x=Size, y=Quantity))+
  geom_col(fill="blue")+
  geom_col(fill="red")

bar1
bar2
bar3

#Month vs Quantity
typeof(data$Month)

bar4 <- data %>%
  filter(Year==1998) %>% 
  group_by(Month) %>% 
  summarise(Quantity=sum(Quantity)) %>% 
  mutate(Month= factor(Month, levels= c(8,9), labels= c("Agosto", "Septiembre"))) %>% 
  ggplot(aes(x=Month, y=Quantity, fill=Month, col=Month ))+
  geom_col()+
  scale_fill_manual(values = c("orange", "pink"))+
  scale_color_manual(values = c("red", "blue"))+
  theme_minimal()




#Si quieren guardar directamente sus gráficas en R
#Pueden correr la siguientes líneas

bar4 #Es necesario indicar cuál gráfica queremos guardar
#Si no la especificamos, la función guardará la última almacenada en la ventana de "Plots" 

ggsave("figs/01_1998-Monthly-Observations.png", dpi= 500, widht=15, height=9)
#Lo primero que deben especificar es la ruta donde guardarán la gráfica
#Para el ejemplo es en la carpeta "figs", ponemos una diagonal
#y escribimos el nombre para nuestra figura; es importante especificar el tipo de imagen
# aquí la guardé como PNG: "figs/NOMBRE.png"
#dpi= es la densidad de pixeles o la resolución, entre mayor el número, mayor la resolución
# y mayor el tamaño
#widht= es el ancho de la imagen
#height= es la altura de la imagen


#Prueben hacer una gráfica del número de observaciones por año (Year y Quantity)
#o si se les ocurre un ejemplo más llamativo, adelante!


  






