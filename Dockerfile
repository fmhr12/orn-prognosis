FROM rocker/shiny:4.4.2

RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev && \
    rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages( \
  c('shinythemes','survival','riskRegression','fastshap','shapviz','ggplot2','prodlim','gower','plotly'), \
  repos='https://cloud.r-project.org', \
  lib='/usr/local/lib/R/site-library' \
); stopifnot(requireNamespace('riskRegression', quietly=TRUE))"

COPY . /app
WORKDIR /app

EXPOSE 10000
CMD R -e "shiny::runApp('/app', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT', 10000)))"
