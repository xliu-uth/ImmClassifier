FROM rocker/tidyverse


RUN apt-get install -y net-tools
RUN apt-get update -qq && apt-get -y install libffi-dev
RUN Rscript -e "install.packages('synapser', repos=c('http://ran.synapse.org', 'http://cran.fhcrc.org'))"
RUN Rscript -e "install.packages('BiocManager')"
RUN Rscript -e "BiocManager::install('sva')"

COPY bin/run-ic.R /usr/local/bin/run-ic.R
RUN chmod a+x /usr/local/bin/run-ic.R

WORKDIR ImmClassifier

COPY DESCRIPTION .
COPY NAMESPACE .

RUN mkdir -p R/
COPY R/* R/

RUN mkdir -p inst/
COPY inst/integrated.cluster.names.rds inst/

RUN Rscript -e 'devtools::install_deps(pkg = ".", dependencies=TRUE,threads = getOption("Ncpus",1))'
RUN R CMD INSTALL .

