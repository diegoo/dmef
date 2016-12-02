#!/usr/bin/python3

## ./tendencia.py extra_abril_3_tendencias.export > extra_abril_3_tendencias.tsv

import sys, csv, pprint
from math import floor
from statistics import mean, median, stdev

def mean_trend(n):
    mid = floor(len(n) / 2)
    first_half, second_half = n[0:mid], n[mid:]
    if mean(first_half) < mean(second_half):
        return 1 # creciente
    elif mean(first_half) == mean(second_half):
        return 0
    else:
        return -1 # decreciente

# def start_end_trend(n):
#     if n[0] < n[-1]:
#         return 1 # creciente
#     elif n[0] == n[-1]:
#         return 0
#     else:
#         return -1 # decreciente

# def stdev_trend(n):
#     if len(n) < 2: return 'NULL'
#     m = mean(n)
#     up, down = m + (1*stdev(n)), m - (1*stdev(n))
#     def is_inside(x):
#         return x > down and x < up
#     return int(all([is_inside(x) for x in n]))

# def oscillate_trend(n):
#     one_half, other_half = n[::2], n[1::2]
#     return mean_trend(one_half + other_half)


def test():
    for numbers in ([3,2.5,2.5,4,6,5],
                    [3,1,2,4,6,5],
                    [4,6,5,3,1,2],
                    [4,6,0,4,1,5],
                    [2,-2,3,-3,4,-4],
                    [7063.39, 28154.35, 19111.49, 18842.88, 16725.10, 14781.37],
                    [1371.80, 1371.75, 1371.75, 1584.67, 1371.75, 1603.16],
                    [3041.76, 12815.64, 21331.16, 20453.15, 12922.40, 31721.87],
                    [42233.17, 42335.01, 15495.56, 13099.92, 8824.66, 8819.21],
                    [1206.20, 1137.25, 1778.29, 1134.59, 1251.29, 477.05],
                    [5511.73, 4111.92, 7243.67, 4635.47, 24970.60, 4848.45]):
        print(numbers, "|", mean_trend(numbers), start_end_trend(numbers), stdev_trend(numbers), oscillate_trend(numbers))
        print("*"*80)

def process_field(field):
    if field == 'NULL': return ['NULL'] ## ,'NULL'] ##,'NULL','NULL']
    values = [float(x) for x in field.split(',') if x != '' and x != 'NULL']
    if values == []: return ['NULL'] ## ,'NULL'] ## ,'NULL','NULL']
    if len(values) == 1: return [2] ## ,2] ##,2,2]
    return [mean_trend(values)] ## , start_end_trend(values)] ##, stdev_trend(values), oscillate_trend(values)]


def run():

    names = [x for x in open(sys.argv[1],'r').readlines()[0].strip().split('\t') if x.startswith('tendencia')]

    with open(sys.argv[1], newline='') as csvfile:
        r = csv.DictReader(csvfile, delimiter='\t', quotechar="""'""")
        for row in r:
            results = []
            for values in [process_field(row[name]) for name in names]:
                for v in values: results.append(str(v)+'\t')
            print(''.join(results).strip())
            # input()
            
if __name__ == '__main__':
    # test()
    run()
