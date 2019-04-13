FROM rocker/tidyverse


RUN apt-get install -y net-tools
RUN apt-get update -qq && apt-get -y install libffi-dev
RUN Rscript -e "install.packages('synapser', repos=c('http://ran.synapse.org', 'http://cran.fhcrc.org'))"
RUN Rscript -e "install.packages('BiocManager')"
RUN Rscript -e "BiocManager::install('sva')"

COPY bin/run-ic.R /usr/local/bin/
RUN chmod a+x /usr/local/bin/run-ic.R

COPY . ImmClassifier
WORKDIR ImmClassifier

RUN Rscript -e 'devtools::install_deps(pkg = ".", dependencies=TRUE,threads = getOption("Ncpus",1))'
RUN R CMD INSTALL .
