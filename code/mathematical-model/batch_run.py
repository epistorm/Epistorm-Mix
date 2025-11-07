"""
Auxiliary script to run the model for multiple input files.

To use, uncomment the `-()-` block for the desired set of model runs,
then call this script with no arguments: `python batch_run.py`
"""

import os
import sys
import subprocess
from pathlib import Path

# # -()- Same beta, USA
# input_files = [
#     'inputs/usa_matrices_same_beta/beta_02/mistry_18_in.yaml',
#     'inputs/usa_matrices_same_beta/beta_02/epistorm_ageby5_80_in.yaml',
#     'inputs/usa_matrices_same_beta/beta_02/prem-2017-pcb_in.yaml',
# ]
# general_out_dir = Path("outputs/same_beta_outs/beta_02")

# -()- Same reproduction number, USA, r0 = 1.3 or 1.5
r_str = "1p5"
input_files = [
    f'inputs/usa_matrices_same_repnum/r0_{r_str}/mistry_18_in.yaml',
    f'inputs/usa_matrices_same_repnum/r0_{r_str}/epistorm_ageby5_80_in.yaml',
    # f'inputs/usa_matrices_same_repnum/r0_{r_str}/epistorm_g1-matrix_in.yaml',
    # f'inputs/usa_matrices_same_repnum/r0_{r_str}/epistorm_g2-matrix_in.yaml',
    # f'inputs/usa_matrices_same_repnum/r0_{r_str}/epistorm_g3-matrix_in.yaml',
    # f'inputs/usa_matrices_same_repnum/r0_{r_str}/epistorm_g4-matrix_in.yaml',
    # f'inputs/usa_matrices_same_repnum/r0_{r_str}/epistorm_g5-matrix_in.yaml',
    f'inputs/usa_matrices_same_repnum/r0_{r_str}/prem-2017-pcb_in.yaml',
]
general_out_dir = Path(f"outputs/same_repnum/r0_{r_str}_out/")

# # -()- Countries
# r_str = "1p5"
# input_files = [
#     f'inputs/countries/r0_{r_str}_in/Italy_in.yaml',
#     f'inputs/countries/r0_{r_str}_in/China-Hunan_in.yaml',
#     f'inputs/countries/r0_{r_str}_in/Switzerland_in.yaml',
#     f'inputs/countries/r0_{r_str}_in/United-Kingdom_in.yaml',
#     f'inputs/countries/r0_{r_str}_in/Netherlands_in.yaml',
#     f'inputs/countries/r0_{r_str}_in/China-Jiangsu_in.yaml',
#     f'inputs/countries/r0_{r_str}_in/Belgium_in.yaml'
# ]
# general_out_dir = Path(f"outputs/countries_out/r0_{r_str}_out")

# # -()- Epistorm – In person or not, R0 = 1.5
# # Compare against epistorm matrices, fixed beta
# input_files = [
#     'inputs/usa_matrices_remote_work/compare_epistorm_beta/epistorm_ageby5_80_in.yaml',
#     'inputs/usa_matrices_remote_work/compare_epistorm_beta/epistorm_in-person_in.yaml',
#     'inputs/usa_matrices_remote_work/compare_epistorm_beta/epistorm_not-in-person_in.yaml',
# ]
# general_out_dir = Path("outputs/remote_work_outs/compare_epistorm_beta_out")

# # -()- Compare in person only with other USA matrices (Mistry and Prem), fixed r0
# input_files = [
#     'inputs/usa_matrices_remote_work/compare_us_r0/epistorm_in-person_in.yaml',
#     'inputs/usa_matrices_remote_work/compare_us_r0/epistorm_not-in-person_in.yaml',
#     'inputs/usa_matrices_remote_work/compare_us_r0/epistorm_75_in-person_in.yaml',
#     'inputs/usa_matrices_remote_work/compare_us_r0/epistorm_75_not-in-person_in.yaml',
# ]
# general_out_dir = Path("outputs/remote_work_outs/compare_us_r0_1p5")


# =========================

# --- Construct output file names
output_dirs = [
    general_out_dir / Path(f.split("_in.yaml")[0]).name for f in input_files
]

print(output_dirs)


for input_file, output_dir in zip(input_files, output_dirs):
    cmd = f"python run_model.py {input_file} {output_dir}"

    print(cmd)
    os.system(cmd)

# =========================
