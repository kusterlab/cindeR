FROM r-base:3.4.2

RUN apt-get update -qq \
	&& apt-get install --no-install-recommends -y \
	libgfortran3 gfortran r-base-dev

MAINTAINER Tobias Schmidt "tobias.k.schmidt@tum.de"

EXPOSE 7677

WORKDIR /srv/shiny/

#COPY packrat /srv/shiny/packrat

#RUN Rscript -e "install.packages('https://cran.rstudio.com/src/contrib/packrat_0.4.8-1.tar.gz')"
#RUN Rscript -e "packrat::restore('/srv/shiny/')"

#ADD lib/selectivityCalculation_0.1.0.tar.gz /srv/shiny/localRepo/selectivityCalculation_0.1.0
#RUN Rscript -e "packrat::install_local('selectivityCalculation_1.1.0.tar.gz')"


RUN Rscript -e "install.packages('shiny')"
RUN Rscript -e "install.packages('shinyjs')"
RUN Rscript -e "install.packages('plotrix')"
RUN Rscript -e "install.packages('drc')"

COPY shiny.sh /usr/bin/shiny.sh
#COPY localRepo/selectivityCalculation /srv/shiny/localRepo/selectivityCalculation
#RUN Rscript -e "require('packrat'); packrat::on(); packrat::set_opts(local.repos = c('/srv/shiny/localRepo')); packrat::install_local('selectivityCalculation')"
COPY www /srv/shiny/www
COPY helpers /srv/shiny/helpers
COPY ui.R /srv/shiny/ui.R
COPY server.R /srv/shiny/server.R

CMD ["/usr/bin/shiny.sh"]

