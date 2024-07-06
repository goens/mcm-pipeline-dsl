
# Determine base image later...
# Ubuntu 22.04.4 LTS
FROM ubuntu:22.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt update -y
RUN apt upgrade -y
RUN apt install -y sudo vim wget curl git #bash
# software-properties-common

#RUN add-apt-repository ppa:deadsnakes/ppa

RUN apt install -y python3.10 python3-pandas python3-tabulate zsh

RUN python3.10 --version
# RUN python3.10 -m pip install pandas
# RUN python3.10 -m pip install tabulate

RUN useradd --user-group --system --create-home --no-log-init user
USER user

WORKDIR /home/user/
RUN wget http://mclab.di.uniroma1.it/site/softwareopensource/cmurphi5.5.0.tgz && tar -xvf cmurphi5.5.0.tgz
#RUN curl https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh -sSf | sh
RUN curl https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh -sSf > elan-init.sh
RUN chmod +x elan-init.sh
RUN ./elan-init.sh -y

#WORKDIR /home/user/
COPY --chown=user . mcm-pipeline-dsl
WORKDIR /home/user/mcm-pipeline-dsl/

RUN . /home/user/.elan/env
RUN /home/user/.elan/bin/elan self update

RUN /home/user/.elan/bin/lake update
RUN /home/user/.elan/bin/lake build

CMD ["/usr/bin/zsh"]
RUN mv zshrc ~/.zshrc

WORKDIR /home/user/mcm-pipeline-dsl/run-artifact

