# Compartmental Age-structured Transmission Model using Contact Matrices

This directory contains the code and data files for the stochastic 
age-structured 
transmission model developed to test the contact matrices produced with 
the contact patterns data.

# Instructions to reproduce the methodology

## Create and activate a python environment

```shell
python -m venv env
source env/bin/activate
pip install -r requirements.txt
```

If you have any errors while executing the code, there may be a problem with 
the versions of python and its libraries. You can try to solve this by 
deleting the `env`, then creating it again with the same library versions
used for development. These are available in `requirements_with_versions.txt`:

```shell
python -m venv env
source env/bin/activate
pip install -r requirements_with_versions.txt
```

You can further ensure compatibility by using [**_python 3.9.6_**](https://www.python.org/downloads/release/python-396/), which was used for development of this model.
First make sure you have python 3.9 in your system. Then, run:

```shell
python3.9 -m venv env
source env/bin/activate
pip install -r requirements_with_versions.txt
```

## Run the model script
Lorem ipsum

```shell
python run_model.py inputs/
```

# Tutorials

Lorem ipsum

