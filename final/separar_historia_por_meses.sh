#!/bin/bash

awk '(NR>1) && ($2 < 201511)' producto_premium_2016.txt > historia.txt
awk '(NR>1) && ($2 >= 201511) && ($2 <= 201604)' producto_premium_2016.txt > hasta_abril_inclusive.txt
awk '(NR>1) && ($2 >= 201601) && ($2 <= 201606)' producto_premium_2016.txt > hasta_junio_inclusive.txt
