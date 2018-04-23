# Data Mining and Matrices Assignments

This repository contains the assignments for the course [IE 673 Data Mining and Matrices](http://dws.informatik.uni-mannheim.de/en/teaching/courses-for-master-candidates/ie-673-data-mining-and-matrices/)
at the University of Mannheim in the spring semester 2018.

## Assignments

- Singular Value Decomposition (Feb 22 - Mar 25): [dmm18-a1-stefschm](https://s3.eu-central-1.amazonaws.com/steffen911-papers/dmm18-a1-stefschm.pdf)
- Matrix Completion (Mar 25 - Apr 22): [dmm18-a2-stefschm](https://s3.eu-central-1.amazonaws.com/steffen911-papers/dmm18-a2-stefschm.pdf)
- Non-Negative Matrix Factorization (Apr 11 - May 06): [dmm18-a3-stefschm](https://s3.eu-central-1.amazonaws.com/steffen911-papers/dmm18-a3-stefschm.pdf)

## Building the documents

Go into the respective folder and run `xelatex FILENAME.tex` or `/bin/bash build.sh` in the
corresponding subdirectory.

## Running the notebooks

The notebooks are build using the [jupyter/jupyter-notebook](https://hub.docker.com/r/jupyter/r-notebook/) docker images.

Start the docker image with `docker run -dp 8888:8888 --name notebook jupyter/r-notebook` to run jupyter locally with all dependencies.
It might be necessary to install further packages. Stick to the references in the notebooks.

Open Jupyter in the browser and import the .ipynb file and the input data from the UI.

Use `docker logs notebook` to get the browser URL with auth token.
