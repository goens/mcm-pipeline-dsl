
# Determine base image later...
FROM python:3.10

#WORKDIR /usr/src/app

#COPY requirements.txt ./
#RUN pip install --no-cache-dir -r requirements.txt

#COPY . .

#CMD [ "python", "./your-daemon-or-script.py" ]

#RUN wget https://bootstrap.pypa.io/get-pip.py
#RUN python3.10 get-pip.py

RUN python3.10 -m pip install pandas
RUN python3.10 -m pip install tabulate

RUN curl https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh -sSf | sh


