

calculate_adaptive_radius <- function(student_data, 
                                      min_radius = 200,    
                                      max_radius = 1000) {
  
  student_data %>%
    mutate(
      # Primero, creamos un ranking normalizado de densidad
      # rank() ordena de menor a mayor, así que las densidades más altas tendrán rankings más altos
      # Al dividir por n(), normalizamos el ranking entre 0 y 1
      density_rank = rank(density)/n(),
      
      # Ahora calculamos el radio de manera inversamente proporcional al ranking
      # Cuando density_rank es alto (alta densidad), el radio será más cercano a min_radius
      # Cuando density_rank es bajo (baja densidad), el radio será más cercano a max_radius
      adaptive_radius = max_radius - (max_radius - min_radius) * density_rank
    )
}



analyze_density_radius_relationship <- function(student_data) {
  # Agrupamos los datos en deciles de densidad para mejor visualización
  density_analysis <- student_data %>%
    mutate(density_decile = ntile(density, 10)) %>%
    group_by(density_decile) %>%
    summarise(
      mean_density = mean(density),
      mean_radius = mean(adaptive_radius),
      min_radius = min(adaptive_radius),
      max_radius = max(adaptive_radius),
      n_students = n()
    )
  
  # Creamos una visualización de la relación
  plot <- ggplot(density_analysis, aes(x = mean_density, y = mean_radius)) +
    geom_point(size = 3, color = "steelblue") +
    geom_line(color = "red", alpha = 0.5) +
    geom_errorbar(aes(ymin = min_radius, ymax = max_radius), alpha = 0.2) +
    labs(
      title = "Relación Empírica entre Densidad y Radio",
      subtitle = "Agrupado por deciles de densidad",
      x = "Densidad Poblacional Media",
      y = "Radio Adaptativo (metros)",
      caption = "Las barras muestran el rango de radios en cada decil"
    ) +
    theme_minimal()
  
  # Mostramos también las estadísticas numéricas
  print("Análisis por deciles de densidad:")
  print(density_analysis)
  
  return(plot)
}


# Aplicamos el cálculo empírico del radio
student_data_with_radius <- calculate_adaptive_radius(sample_2019)

# Analizamos la relación resultante
relationship_plot <- analyze_density_radius_relationship(student_data_with_radius)

# Visualizamos el resultado
print(relationship_plot)




model <- lm(adaptive_radius ~ density, data = student_data_with_radius)
summary(model)


glimpse(student_data_with_radius)
