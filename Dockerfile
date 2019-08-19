# Instrucciones:
# + Para crear la imagen 
#      docker build -t jaalonso/temas-de-pf-con-haskell:v2 .
# + Para ejecutarla
#      docker run --rm -p 8888:8888 -v $PWD/temas:/home/jovyan/pwd --name temas-de-pf-con-haskell jaalonso/temas-de-pf-con-haskell:v2

FROM jaalonso/ihaskell-i1m:v2

USER root

RUN mkdir /home/$NB_USER/temas
COPY temas/* /home/$NB_USER/temas/
RUN chown --recursive $NB_UID:users /home/$NB_USER/temas

USER $NB_UID

ENV JUPYTER_TOKEN=x
ENV JUPYTER_ENABLE_LAB=yes
