port=9001
if [ -z $1 ]
then
  echo "Using port 9001"
else
  echo "Using port assigned port"
  port=$1
fi
echo $port

uvicorn server:app --port $port