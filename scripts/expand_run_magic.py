#!/usr/bin/env python3
import sys
import json

input_path = sys.argv[1]
output_path = sys.argv[2]

exapnded = 0

with open(input_path) as f:
    nb_in = json.load(f)

    new_cells = []

    for cell in nb_in['cells']:
        if cell['cell_type'] != 'code':
            new_cells.append(cell)
            continue
        code = ''.join(cell['source'])
        if code.startswith('%run'):
            to_include = code[5:].strip()
            with open(to_include) as o:
                nb_run = json.load(o)
            new_cells.extend(nb_run['cells'])
            expanded = 1
        else:
            new_cells.append(cell)

assert expanded
print(f'Expanded {expanded} run magics')

with open(output_path, 'w') as f:
    nb_in['cells'] = new_cells
    json.dump(nb_in, f)

print(f'Saved as {output_path}')
