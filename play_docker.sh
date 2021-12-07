docker build -t swipl-hive:v1 .
docker run --rm -it --net=host swipl-hive:v1 ./main.out