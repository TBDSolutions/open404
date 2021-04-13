# This can change with the whatever version of R the app was built
# sets the base image and OS on which the entire computer will be built
FROM rocker/shiny-verse:latest

# RUN commands are any shell commands needed on the base image layer (set above)
# Special installs for sf mapping package. Because the programs being installed will ask "do you want.." 
# the -y command is used to say 'yes'
RUN apt-get update
RUN apt-get install -y libudunits2-dev 
RUN apt-get install -y libgdal-dev

# Special install to allow B2C functionality for R inside a container. 
# Should be fixed in later Microsoft updates. 
RUN R -e 'install.packages("devtools");'
RUN R -e 'library("devtools"); install_github("LHaferkamp/httpuv")'

# Debian installs for host server/VM
RUN apt-get update && apt-get install -y \ 
    libssl-dev \
    ## clean up
    && apt-get clean \ 
    && rm -rf /var/lib/apt/lists/ \ 
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# R packages needed for Shiny app 
# (This will hopefully be the only change to the docker file, assuming the folder structure is set up right)
RUN install2.r tidyverse shiny DT shinythemes scales hrbrthemes stringi plotly viridis grid gtable shinycssloaders shinyWidgets\
    ## clean up
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# The . and /srv/shiny-server/ are two different arguments that relate to your local file and the 
# file path in the container; not to be read as one string. 
# Copy files from the root directory . into the docker image file location /srv/shiny-server/ 
#COPY . /srv/shiny-server/ 

# Copy files from the app folder /app into the docker image file location /srv/shiny-server/ 
COPY /app /srv/shiny-server/

# Exposing port to listen on. This was needed when running on an indivdual VM as opposed to an app service plan
# so when the docker starts port 3838 is exposed. 
EXPOSE 3838
# Required for shiny server 
#USER shiny
