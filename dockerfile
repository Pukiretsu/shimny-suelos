# Base R Shiny image
FROM rocker/shiny

# Crear directorio de la app
RUN mkdir -p /home/shiny-app

# Instalar dependencias
RUN R -e "install.packages(c('dplyr', 'ggplot2', 'gapminder'))"

# Copiar la aplicación al contenedor
COPY app.R /home/shiny-app/app.R

# Establecer el directorio de trabajo
WORKDIR /home/shiny-app

# Exponer el puerto 3838 (que es el puerto por defecto de Shiny)
EXPOSE 3838

# Ejecutar la aplicación
CMD ["R", "-e", "shiny::runApp('/home/shiny-app', host='0.0.0.0', port=3838)"]