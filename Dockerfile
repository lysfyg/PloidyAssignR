FROM rocker/shiny

# Install system requirements
RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-gnutls-dev \
    lynx \
    xdg-utils \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# R dependencies
RUN cd /home && R -e "if (!requireNamespace('devtools')) { install.packages('devtools'); }; devtools::install_github('lysfyg/PloidyAssignR');"

# Workdir
WORKDIR /home

# App port
EXPOSE 8180

# Run the app
CMD R -e "library(shiny); library(PloidyAssignR); options(shiny.host='0.0.0.0'); options(shiny.port=8180); run_app();"
