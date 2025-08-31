FROM fredhutch/r-shiny-base:4.4.2
RUN apt-get update --allow-releaseinfo-change
# RUN apt-get install -y pandoc
RUN useradd -u 5555 -m -d /home/shiny -c "shiny user" shiny

RUN R -e "install.packages(c('survminer', 'exactci', 'mvtnorm', 'cobs'))"

ADD app/. /home/shiny/
RUN chown -R shiny:shiny /home/shiny 
WORKDIR /home/shiny
USER shiny
EXPOSE 7777
CMD Rscript start.R 
