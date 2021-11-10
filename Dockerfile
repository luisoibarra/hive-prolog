FROM docker.uclv.cu/swipl:8.2.4

RUN mkdir app

WORKDIR /app

COPY . .

RUN swipl -o main.out -c main.pl