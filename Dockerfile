##################################################
#
# Dockerfile for R3.6 and UMX package
#
##################################################


FROM r-base:latest

#RUN apt-get install -y \
#	r-cran-littler

RUN Rscript -e 'install.packages(c("OpenMx","umx"))'
