FROM rocker/tidyverse

RUN apt-get install -y net-tools
RUN apt-get update -qq && apt-get -y install libffi-dev && apt-get -y install python3-pip
RUN Rscript -e "install.packages(c('BiocManager','mlr','stringr','randomForest','reticulate'))"
RUN Rscript -e "BiocManager::install('sva')"

RUN pip3 install pandas keras numpy sklearn

COPY bin/runIC.py  /usr/local/bin/
COPY bin/dnn.py /usr/local/bin/
RUN chmod a+x /usr/local/bin/runIC.py
RUN chmod a+x /usr/local/bin/dnn.py

COPY . ImmClassifier
WORKDIR ImmClassifier

RUN R CMD INSTALL .
