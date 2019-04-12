FROM rocker/tidyverse


RUN apt-get install -y net-tools
RUN apt-get update -qq && apt-get -y install libffi-dev

RUN Rscript -e "install.packages('mlr')"
RUN Rscript -e "install.packages('sva')"
RUN Rscript -e "install.packages('stringr')"
RUN Rscript -e "install.packages('randomForest')"
RUN Rscript -e "install.packages('synapser', repos=c('http://ran.synapse.org', 'http://cran.fhcrc.org'))"

COPY R_cmdline/assign_cell_identity.R  /
COPY R_cmdline/immClassifier.R /
COPY R_cmdline/mlr_train.R /
COPY R_cmdline/visualization.R/ /

RUN mkdir -p data/
COPY test/bulk.logrma.rds /data/

RUN chmod a+x /assign_cell_identity.R
RUN chmod a+x /immClassifier.R
RUN chmod a+x /mlr_train.R
RUN chmod a+x /visualization.R
