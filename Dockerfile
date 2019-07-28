# Instrucciones:
# + Para crear la imagen 
#      docker build -t temas-i1m:19.07 .
# + Para ejecutarla
#      docker run --rm -p 8888:8888 -v $PWD/temas-i1m:/home/jovyan/pwd --name temas-i1m temas-i1m:19.07

FROM jaalonso/ihaskell-i1m:19.07

USER root

RUN mkdir /home/$NB_USER/temas-i1m
COPY temas-i1m/*.ipynb /home/$NB_USER/temas-i1m/
RUN chown --recursive $NB_UID:users /home/$NB_USER/temas-i1m

USER $NB_UID

ENV JUPYTER_TOKEN=x
ENV JUPYTER_ENABLE_LAB=yes
