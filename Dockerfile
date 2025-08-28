FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    libudunits2-dev

# Install R packages
RUN R -e "install.packages(c('shiny', 'dplyr', 'ggplot2', 'leaflet', 'scales', 'qs', 'tidyr', 'stringr', 'gt', 'gtExtras', 'purrr', 'openxlsx', 'sf', 'bslib', 'mapboxapi'), repos='http://cran.rstudio.com/', dependencies=TRUE)"

# Copy app files
COPY . /srv/shiny-server/

WORKDIR /srv/shiny-server

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp(host='0.0.0.0', port=3838)"]