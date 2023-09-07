# Use a Shiny base image
FROM rocker/shiny:latest

# Install additional dependencies
RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-gnutls-dev

# Install required R packages
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'ggplot2'), repos='http://cran.rstudio.com/')"

# Copy the Shiny app code into the container
COPY . /srv/shiny-server/my-app

# Set the working directory
WORKDIR /srv/shiny-server/my-app

# Expose the Shiny app port
EXPOSE 3838

# Run the Shiny app
CMD ["R", "-e", "shiny::runApp(port = 3838, host = '0.0.0.0')"]
