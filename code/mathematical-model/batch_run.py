"""
Auxiliary script to run the model for multiple input files.

Usage: Just run this program without arguments.
> python batch_run.py
"""

import os
from pathlib import Path

# String representing the reproduction number
r_str = "1p5"

input_files = list()

# -()- By race and ethnicity
input_files += [
    f"inputs/usa_matrices_same_repnum/r0_{r_str}/epistorm_race-ethnicity_in.yaml",
    f"inputs/countries/r0_{r_str}_in/UK_race-ethnicity_in.yaml"
]


# -()- USA, different surveys
input_files += [
    f'inputs/usa_matrices_same_repnum/r0_{r_str}/mistry_18_in.yaml',
    f'inputs/usa_matrices_same_repnum/r0_{r_str}/epistorm_ageby5_80_in.yaml',
    f'inputs/usa_matrices_same_repnum/r0_{r_str}/prem-2017-pcb_in.yaml',
]

# -()- Other countries
input_files += [
    f'inputs/countries/r0_{r_str}_in/Italy_in.yaml',
    f'inputs/countries/r0_{r_str}_in/China-Hunan_in.yaml',
    f'inputs/countries/r0_{r_str}_in/Switzerland_in.yaml',
    f'inputs/countries/r0_{r_str}_in/United-Kingdom_in.yaml',
    f'inputs/countries/r0_{r_str}_in/Netherlands_in.yaml',
    f'inputs/countries/r0_{r_str}_in/China-Jiangsu_in.yaml',
    f'inputs/countries/r0_{r_str}_in/Belgium_in.yaml'
]

# -()- UK Alternative matrix
input_files += [
    f"inputs/countries/r0_{r_str}_in/UK_goodfellow_in.yaml",  # Goodfellow et al 2025
]

general_out_dir = Path(f"outputs/r0_{r_str}_out")


# ================================

# --- Construct output file names
output_dirs = [
    general_out_dir / Path(f.split("_in.yaml")[0]).name for f in input_files
]

print(output_dirs)


for input_file, output_dir in zip(input_files, output_dirs):
    cmd = f"python run_model.py {input_file} {output_dir}"

    print(cmd)
    os.system(cmd)

# ================================
