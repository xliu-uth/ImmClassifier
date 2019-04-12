FROM rocker/tidyverse


RUN apt-get install -y net-tools
RUN apt-get update -qq && apt-get -y install libffi-dev

RUN Rscript -e "install.packages('synapser', repos=c('http://ran.synapse.org', 'http://cran.fhcrc.org'))"

COPY R_cmdline/* immclassifier
WORKDIR immclassifier


RUN mkdir -p inst/
COPY inst/integrated.cluster.names.rds /inst/

RUN RScript -e 'devtools::install_deps(pkg = ".", dependencies=TRUE,threads = getOption('Ncpus',1))'
RUN R CMD INSTALL .

COPY immClassifier.R /usr/local/bin
RUN chmod a+x /usr/local/bin/immClassifier.R
