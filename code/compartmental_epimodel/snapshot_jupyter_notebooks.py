"""
A simple script to save the relevant jupyter notebooks into a zip file
that can be commited to the repository.

This is helpful to keep jupyter notebooks away from the GitHub repo,
since they "contaminate" the Languages counter - everything becomes
jupyter notebooks.

The program chooses among an "include" and an "exclude" list of Notebooks.

- Output file: jupyter_notebooks.zip
"""
from glob import glob
import os
from pathlib import Path
import zipfile

# --- Select here the notebooks to include and exclude
include_notebooks = [
    *glob("*.ipynb"),  # Include all notebooks in the dedicated directory
    # "jupyter_notebooks/prepare_cell_data.ipynb",
]

exclude_notebooks = [
    "anything.ipynb",
]


# ------------------
use_notebooks = [n for n in include_notebooks if n not in exclude_notebooks]

print(f"Notebooks to save: {use_notebooks}")

pass
# Output file path
output_file = Path('jupyter_notebooks.zip')
# output_file.parent.mkdir(parents=True, exist_ok=True)

# Create a zip file
with zipfile.ZipFile(output_file, 'w') as zip_file:
    for notebook in use_notebooks:
        try:
            zip_file.write(notebook, os.path.basename(notebook))
        except FileNotFoundError:
            print(f"[WARNING] Notebook not found: {notebook}. Skipped.")
        else:
            print(f"[SUCCESS] Saved: {notebook}")

print(f"Notebooks saved to {output_file}")
