# Compartmental Age-structured Transmission Model using Contact Matrices

This directory contains the code and data files for the stochastic 
age-structured 
transmission model developed to test the contact matrices produced with 
the contact patterns data.

# Instructions to reproduce the methodology

## Create and activate a python environment

> NOTICE: These instructions are valid for a Unix system (Linux, MacOS) 
> and might need to be adapted for use with Windows.

If you have this repository cloned, open a terminal and move to this 
directory (this is, the subdirectory `code/compartmental_epimodel`).
If you are familiar with creating environments for running python, you can use your 
preferred method, observing the required libraries in `requirements.txt`. 
Otherwise, type the following commands in the terminal:

```shell
python -m venv env
source env/bin/activate
pip install -r requirements.txt
```

If you have any errors while executing the code, there may be a problem with 
the versions of the python interpreter and its libraries. You can try to solve this by 
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

You can run a simple test to check if the code works in your machine. 
For this, make sure you have a terminal open in the `code/compartmental_epimodel` 
subdirectory, then run:

```shell
python run_model.py inputs/input_example.yaml outputs/tests/example/01
```

The first argument, `inputs/input_example.yaml`, is the path to the input
file, which contains program parameters and other required file paths. The 
second argument, `outputs/tests/example/01`, is an example of an output
directory where the model files are exported to. You can use any path you 
want as the second argument; the directory and its parents will be created 
automatically if they do not exist.

For reproducing the results in the paper, you will need to get additional 
data files from their respective sources.  

Open `batch_run.py`, select the set of simulations you want to use, 
then run it without arguments:

```shell
python batch_run.py
```


# Tutorials

Lorem ipsum

