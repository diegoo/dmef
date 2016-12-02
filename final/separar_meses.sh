#!/bin/bash

DATASET="../producto_premium_2016.txt"

for foto_mes in 201404 201405 201406 201407 201408 201409 201410 201411 201412 201501 201502 201503 201504 201505 201506 201507 201508 201509 201510 201511 201512 201601 201602 201603 201604 201605 201606; do
    echo "processing ${foto_mes}"
    # cat $DATASET | awk -v fm="${foto_mes}" '$2 == fm { print $0 }' > "${foto_mes}.txt"
    cat ../titulos.txt "${foto_mes}.txt" > "${foto_mes}.with.header.txt"
done
