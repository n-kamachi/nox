SECONDS=0

ls ./out_data -1 | while read line || [ -n "${line}" ]
do

echo ${line}

gzip ./out_data/${line}

done

echo "Script time is ${SECONDS}sec."