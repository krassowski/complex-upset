#!/usr/bin/env python3
import sys
import json
import re

python_input_path = sys.argv[1]
r_output_path = sys.argv[2]

prepare_changed = False

print(f'Will translate Python exampels from {python_input_path} into R examples')

with open(python_input_path) as f:
    nb_in = json.load(f)

    new_cells = []

    for cell in nb_in['cells']:
        if cell['cell_type'] == 'code':
            first = cell['source'][0].strip()
            if first.strip() == '%run Prepare_Python.ipynb':
                cell['source'] = [
                    '%run Prepare_R.ipynb'
                ]
                prepare_changed = True
            elif first.startswith('%%R'):
                first = first[4:].strip()
                if first:
                    args = first.split(' ')
                    args = {
                        args[i*2]: args[i*2 + 1]
                        for i in range(len(args)//2)
                    }
                    assert args
                    w = int(args["-w"]) / 100
                    h = int(args["-h"]) / 100
                    if round(w) == w:
                        w = int(w)
                    else:
                        w = f'{w:.1f}'
                    if round(h) == h:
                        h = int(h)
                    else:
                        h = f'{h:.1f}'
                    first = f'set_size({w}, {h})\n'
                cell['source'][0] = first
            elif first.startswith('%R'): 
                first = first[2:].strip()
                cell['source'][0] = first
        new_cells.append(cell)

assert prepare_changed

with open(r_output_path, 'w') as f:
    nb_in['metadata'] = {
      "kernelspec": {
       "display_name": "R",
       "language": "R",
       "name": "ir"
      },
      "language_info": {
       "codemirror_mode": "r",
       "file_extension": ".r",
       "mimetype": "text/x-r-source",
       "name": "R",
       "pygments_lexer": "r",
       "version": "3.6.1"
      }
     }
    nb_in['cells'] = new_cells
    json.dump(nb_in, f)

print(f'Saved as {r_output_path}')
