#!/usr/bin/python3

import sys
from operator import itemgetter

lines = [line.strip().split('\t') for line in open('xgboost.tsv').readlines()]
titulos = lines[0]
results = lines[1:]
results = [[float(field.strip()) for field in line] for line in results]
ganancia_promedio_field = titulos.index('ganancia.promedio')
sorted_results = sorted(results, key=itemgetter(ganancia_promedio_field), reverse=True)

sys.stderr.write("%s: %f\n" % ("max", sorted_results[0][ganancia_promedio_field]))
print(','.join(titulos))
for sorted_result in sorted_results:
    print(','.join([str(x) for x in sorted_result]))
