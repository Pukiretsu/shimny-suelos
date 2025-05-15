# get shiny server plus tidyverse packages image

FROM rocker/shiny-verse:latest




# system libraries of general use

RUN apt-get update && apt-get install -y \

    sudo \

    pandoc \

    pandoc-citeproc \

# I didn’t need both libcurl4-gnutls-dev & libcurl4-openssl-dev so got rid of one

#    libcurl4-gnutls-dev \

    libcairo2-dev \

    libxt-dev \

    libssl-dev \

    libssh2-1-dev \

    curl \

    libcurl4-openssl-dev \

    libxml2-dev \

    libudunits2-0 \

    libudunits2-dev




# some of these are needed for the r sf package to run

RUN apt-get update && apt-get install -y \

#	libudunits2-dev \

	libgdal-dev \

	libgeos-dev \

	libproj-dev \

	libfontconfig1-dev





# Install R Dependencies - installs the r packages you need - if this step fails you’re likely

# missing system libraries that a package requires

RUN install2.r --error \

	rio \

	maps \

	sp \

	maptools \

	housingData \

	leaflet \

	DT\

	shinycustomloader




RUN install2.r --error \

	sf




### -----------------------------------




# copy shiny-server.sh to image

COPY shiny-server.sh /usr/bin/




# copy shiny server config to image

COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf




# copy the contents of app folder to image

COPY ./app /srv/shiny-server/app/




# select port

EXPOSE 80




# allow permission for user ‘shiny’ to run

RUN sudo chown -R shiny:shiny /srv/shiny-server




# install linux programs to enable conversion of ms dos file to unix file

RUN apt-get update && apt-get install -y dos2unix




# we do this so that the shiny-server.sh file is recognized by the linux machine

RUN dos2unix /usr/bin/shiny-server.sh && apt-get --purge remove -y dos2unix && rm -rf /var/lib/apt/lists/*




# Change access permissions to shiny-server.sh - did not need this for my purposes

 RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]




# run app

CMD ["/usr/bin/shiny-server.sh"]
