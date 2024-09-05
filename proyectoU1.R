#Proyeco Unidad 1.
#Dulce María Medina Cruz
#LMA Análisis Multivariado

# Instalar y cargar el paquete necesario
install.packages("mlbench")
library(mlbench)

# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)

# Cargar la base de datos
data(BreastCancer, package = "mlbench")
head(BreastCancer)

# Eliminar filas con valores faltantes
bc_data <- na.omit(BreastCancer)

# Asegurarse de que la variable 'Class' esté como factor
bc_data$Class <- factor(bc_data$Class, levels = c("benign", "malignant"))

###### Gráfico 1 ######
# Gráfico de densidad para una variable continua (Bare Nuclei)
bc_data$Bare.nuclei <- as.numeric(bc_data$Bare.nuclei)

# Gráfico de densidad 
ggplot(bc_data, aes(x = Bare.nuclei, fill = Class)) +
  geom_density(alpha = 0.6) +
  labs(title = "Densidad de Núcleos Desnudos según su Diagnóstico", 
       x = "Núcleos desnudos presentes en las células (Bare Nuclei)", 
       y = "Densidad") +
  theme_minimal() +
  scale_fill_manual(name = "Diagnóstico", 
                    values = c("benign" = "#1f77b4", "malignant" = "#ff7f0e"),
                    labels = c("Benigno", "Maligno"))

###### Gráfico 2 ######
# Gráfico de dispersión para Uniformity of Cell Shape vs Uniformity of Cell Size según el Diagnóstico
bc_data$Cell.shape <- as.numeric(bc_data$Cell.shape)
bc_data$Cell.size <- as.numeric(bc_data$Cell.size)

# Gráfico de dispersión
ggplot(bc_data, aes(x = Cell.shape, y = Cell.size, color = Class)) +
  geom_point(alpha = 0.7, size = 2) +  # Ajuste de la transparencia y tamaño de los puntos
  geom_jitter(width = 0.5, height = 0.5, alpha = 0.7) +  # Añadir jitter para evitar el empalme de puntos
  labs(title = "Relación entre Uniformity of Cell Shape y Uniformity of Cell Size según el Diagnóstico", 
       x = "Uniformity of Cell Shape (Uniformidad de la Forma Celular)", 
       y = "Uniformity of Cell Size (Uniformidad del Tamaño Celular)") +
  theme_minimal() +
  scale_color_manual(name = "Diagnóstico", 
                     values = c("benign" = "#1f77b4", "malignant" = "#ff7f0e"),
                     labels = c("Benigno", "Maligno"))

###### Gráfico 3 ######
# Gráfico de Barras Apiladas. Composición de Diagnóstico según Categorías de Mitosis
bc_data$Mitoses <- as.numeric(bc_data$Mitoses)
bc_data$Mitoses_cat <- cut(bc_data$Mitoses, 
                           breaks = c(-Inf, 2, 5, 10, Inf), 
                           labels = c("0-2", "3-5", "6-10", ">10"))

# Gráfico de barras apiladas
ggplot(bc_data, aes(x = Mitoses_cat, fill = Class)) +
  geom_bar(position = "fill") +
  labs(title = "Composición de Diagnóstico según Categorías de Mitosis",
       x = "Número de Mitosis (Categorizado)",
       y = "Proporción",
       fill = "Diagnóstico") +
  scale_fill_manual(values = c("benign" = "#1f77b4", "malignant" = "#ff7f0e"),
                    labels = c("Benigno", "Maligno")) +
  theme_minimal()


