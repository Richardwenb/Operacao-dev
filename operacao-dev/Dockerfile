FROM openanalytics/r-base

MAINTAINER Geovana Veloso "geovana.lima@marilan.com"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libmariadb-dev \
    libmariadbclient-dev \
    libxml2-dev 
 
# basic shiny functionality
RUN R -e "install.packages(c('shinyWidgets', 'doParallel', 'foreach', 'RMariaDB', 'devtools', 'yaml', 'reshape',  'shiny', 'RSAP',  'dplyr', 'DT', 'foreach', 'stringr', 'lubridate', 'DBI', 'janitor', 'RDBC', 'varhandle', 'stringi', 'readxl', 'shinyalert', 'tdyr' ), repos='https://cloud.r-project.org/')"

# copy the app to the image
RUN mkdir /root/operacao
COPY app /root/operacao
COPY nwrfcsdk /root/operacao/nwrfcsdk

RUN echo "/root/operacao/nwrfcsdk/lib" > /etc/ld.so.conf.d/nwrfcsdk.conf && \
 ldconfig


ENV NWRFCSDK_INCLUDE=/root/operacao/nwrfcsdk/include/
ENV NWRFCSDK_LIBS=/root/operacao/nwrfcsdk/lib/
ENV LD_LIBRARY_PATH=/root/operacao/nwrfcsdk/lib/

RUN R -e "devtools::install_github('piersharding/RSAP')"



COPY Rprofile.site /usr/lib/R/etc/
EXPOSE 4040
CMD ["R", "-e", "shiny::runApp('/root/operacao')"]
