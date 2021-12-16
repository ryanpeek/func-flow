
# Installing FFC via python

## Download

download and cd into dir

```
git clone https://github.com/leogoesger/func-flow.git
cd func-flow/
```

## Python: Activate/Create Virtual Environment

activate environ via Python

```
python3 -m venv py-virtenv
source my-virtualenv/bin/activate

# to get out of environ use 
deactivate
```

## Installing with Pip

Next we install all the package requirements with `pip`:

```
pip install -r requirements_revised.txt
```

## `conda`: Activate/Create Virtual Environment

To save environment in conda from current:

```
conda env export > environment.yml
```

To build or create the environment:

```
conda env create --file environment.yml

# and remove
conda env list
conda env remove -n NAME
```

For this repo, I used this:

An example of what this `yml` looks like:

```
name: ffc
channels:
    - conda-forge
    - bioconda
    - defaults
dependencies:
  - python=3.7
  - pip
# this breaks for me
#  - pip: 
#    - packagename
#    - -git+https://github.com/ffc-calc/ffc@branch
```

Next we install all the package requirements with `pip`:

```
pip install -r requirements_revised.txt
```

## Dependencies

For functional flows calculator this is what is required:

```
absl-py
astor
astroid
autopep8
Click
cvxpy
cycler
dill
ecos
fancyimpute
fastcache
Flask
future
gast
grpcio
gunicorn
h5py
pyyaml
isort
itsdangerous
Jinja2
joblib
Keras
Keras-Applications
Keras-Preprocessing
knnimpute
lazy-object-proxy
Markdown
MarkupSafe
matplotlib
mccabe
multiprocess
np-utils
numpy
osqp
pandas
protobuf
pycodestyle
pydot
pylint
pyparsing
python-dateutil
python-dotenv
pytz
scikit-learn
scipy
scs
simplejson
six
sklearn
tensorboard
tensorflow
termcolor
Werkzeug
wrapt
```


