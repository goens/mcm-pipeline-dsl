
# Determine base image later...
# Ubuntu 22.04.4 LTS
FROM ubuntu:20.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt update -y
RUN apt upgrade -y
RUN apt install -y python3 python3-pandas python3-tabulate zsh

#WORKDIR /usr/src/app

#COPY requirements.txt ./
#RUN pip install --no-cache-dir -r requirements.txt

#COPY . .

#CMD [ "python", "./your-daemon-or-script.py" ]

#RUN wget https://bootstrap.pypa.io/get-pip.py
#RUN python3 get-pip.py

# RUN python3 -m ensurepip --upgrade

RUN python3 --version
# RUN python3 -m pip install pandas
# RUN python3 -m pip install tabulate

RUN curl https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh -sSf | sh

CMD ["/usr/bin/zsh"]

