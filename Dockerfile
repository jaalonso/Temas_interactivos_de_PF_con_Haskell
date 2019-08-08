# Instrucciones:
# + Para crear la imagen 
#      docker build -t jaalonso/temas-de-pf-con-haskell:19.07 .
# + Para ejecutarla
#      docker run --rm -p 8888:8888 -v $PWD/temas:/home/jovyan/pwd --name temas-de-pf-con-haskell jaalonso/temas-de-pf-con-haskell:19.07

FROM jaalonso/ihaskell-i1m:19.07

USER root

RUN mkdir /home/$NB_USER/temas
COPY temas/*.ipynb /home/$NB_USER/temas/
COPY temas/fig /home/$NB_USER/temas/fig
COPY temas/ejemplos /home/$NB_USER/temas/ejemplos
COPY temas/Codigos/*.hs /home/$NB_USER/temas/Codigos
RUN chown --recursive $NB_UID:users /home/$NB_USER/temas

USER $NB_UID

ENV JUPYTER_TOKEN=x
ENV JUPYTER_ENABLE_LAB=yes
