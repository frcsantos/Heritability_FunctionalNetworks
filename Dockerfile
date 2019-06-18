##################################################
#
# Dockerfile for R3.6 and UMX package
#
##################################################

FROM r-base:latest

RUN apt-get update
RUN apt-get upgrade -y

RUN apt-get install -y \
	openjdk-8-jdk \
	gnupg2 \
	libxml2-dev \
	libcurl4-openssl-dev \
	libssl-dev/unstable

RUN R CMD javareconf JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64/include/
RUN Rscript -e 'install.packages(c("xlsxjars","rJava","xlsx","tidyverse","OpenMx","umx"))'
