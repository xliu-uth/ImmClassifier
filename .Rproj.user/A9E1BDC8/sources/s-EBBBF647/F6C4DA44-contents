FROM rocker/tidyverse

RUN apt-get install -y net-tools

# Install the data files.
# Do this early so that changes to the installation procedure (or
# ImmClassifier package) downstream won't re-trigger download.
RUN apt-get update -qq && apt-get -y install wget

RUN mkdir data
RUN wget -O data/hca-bm-train-test-dat.rds --no-check-certificate https://ndownloader.figshare.com/files/20076257
RUN wget -O data/jci-bm-train-test-dat.rds --no-check-certificate https://ndownloader.figshare.com/files/20076248
RUN wget -O data/pbmc-train-test-dat.rds --no-check-certificate https://ndownloader.figshare.com/files/20076239
RUN wget -O data/liver-immune-train-test-dat.rds --no-check-certificate https://ndownloader.figshare.com/files/20076251
RUN wget -O data/nsclc-zilionis-tii-minor-train-test-dat.rds --no-check-certificate https://ndownloader.figshare.com/files/20076242
RUN wget -O data/nsclc-guo-train-test-dat.rds --no-check-certificate https://ndownloader.figshare.com/files/20076245
RUN wget -O data/brcatil-train-test-dat.rds --no-check-certificate https://ndownloader.figshare.com/files/20076260


# Install software.

RUN apt-get update -qq && apt-get -y install libffi-dev && apt-get -y install python3-pip && apt-get -y install wget
RUN Rscript -e "install.packages(c('optparse','BiocManager','mlr','stringr','randomForest','reticulate'))"
RUN Rscript -e "BiocManager::install('sva')"

RUN pip3 install pandas keras numpy sklearn tensorflow


COPY bin/runIC.R  /usr/local/bin/
RUN chmod a+x /usr/local/bin/runIC.R

COPY . ImmClassifier
WORKDIR ImmClassifier

RUN mv /data/*.rds data && \
  rmdir /data/

RUN R CMD INSTALL .

VOLUME ['/tmp']

ENTRYPOINT ["Rscript", "/ImmClassifier/bin/runIC.R"]
