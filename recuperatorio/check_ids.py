#!/usr/bin/env python3
import sys
futuro_ids = [x.strip() for x in open('futuro.ids').readlines()]
predictions = [x.strip() for x in open(sys.argv[1]).readlines()]
condition = all([x in futuro_ids for x in predictions])
print(condition)
