FROM rocker/r-ver:4.0.2
RUN apt-get update && apt-get install -y  gdal-bin git-core libcurl4-openssl-dev libgdal-dev libgeos-dev libgeos++-dev libgit2-dev libglpk-dev libgmp-dev libpng-dev libssh2-1-dev libssl-dev libudunits2-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev r-cran-rcpp && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_github("r-lib/remotes", ref = "97bbf81")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.4.2")'
RUN Rscript -e 'remotes::install_version("magrittr",upgrade="never", version = "1.5")'
RUN Rscript -e 'remotes::install_version("processx",upgrade="never", version = "3.4.4")'
RUN Rscript -e 'remotes::install_version("pkgload",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "2.3.2")'
RUN Rscript -e 'remotes::install_version("ggplot2",upgrade="never", version = "3.3.2")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.0")'
RUN Rscript -e 'remotes::install_version("purrr",upgrade="never", version = "0.3.4")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.0.2")'
RUN Rscript -e 'remotes::install_version("tidyr",upgrade="never", version = "1.1.2")'
RUN Rscript -e 'remotes::install_version("attempt",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.5.0")'
RUN Rscript -e 'remotes::install_version("leaflet",upgrade="never", version = "2.0.3")'
RUN Rscript -e 'remotes::install_version("sf",upgrade="never", version = "0.9-6")'
RUN Rscript -e 'remotes::install_version("memoise",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("rnoaa",upgrade="never", version = "1.2.0")'
RUN Rscript -e 'remotes::install_version("countrycode",upgrade="never", version = "1.2.0")'
RUN Rscript -e 'remotes::install_version("future",upgrade="never", version = "1.19.1")'
RUN Rscript -e 'remotes::install_version("forcats",upgrade="never", version = "0.5.0")'
RUN Rscript -e 'remotes::install_version("shinyjqui",upgrade="never", version = "0.3.3")'
RUN Rscript -e 'remotes::install_version("santoku",upgrade="never", version = "0.5.0")'
RUN Rscript -e 'remotes::install_version("leaflet.extras",upgrade="never", version = "1.0.0")'
RUN Rscript -e 'remotes::install_version("rnaturalearth",upgrade="never", version = "0.1.0")'
RUN Rscript -e 'remotes::install_version("shinydashboard",upgrade="never", version = "0.7.1")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.5.4")'
RUN Rscript -e 'remotes::install_version("highcharter",upgrade="never", version = "0.8.2")'
RUN Rscript -e 'remotes::install_version("bs4Dash",upgrade="never", version = "0.5.0")'
RUN Rscript -e 'remotes::install_version("readr",upgrade="never", version = "1.4.0")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.16")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.2.1")'
RUN Rscript -e 'remotes::install_github("Nenuial/ggeo@862abe82e4932b986cf56816c4d89e1cc7f95851")'
RUN Rscript -e 'remotes::install_github("worldbank/wbgviz@3850d6e14542109550d61985a4b0a24a89be4005")'
RUN Rscript -e 'remotes::install_github("Nenuial/geographer@52867c473d5858a6a5df563c9ee28222789197e3")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
EXPOSE 3838
CMD  ["R", "-e", "options('shiny.port'=3838,shiny.host='0.0.0.0');GEOView::run_app()"]
