# Web of Life

The <a href="http://www.web-of-life.es" target="_blank">Web of Life</a> provides a graphical user interface, based on Google Maps, for visualizing and downloading data on species interaction networks. It was designed and implemented in a relational database management system, allowing sophisticated and user-friendly cross-search across networks. Users can easily access to the database through any web browser. Data can be downloaded in many file formats, and a data transmission webservice in JavaScript Object Notation is also provided.

### Jupyter Notebook:
<a href="http://jupyter.org/" target="_blank">Jupyter Notebook</a> is an open-source web application for interactive computing across many programming languages. The notebook provided <a href="https://github.com/miguelfortuna/WebOfLife/blob/master/WebOfLife.ipynb" target="_blank">here</a> uses the <a href="https://irkernel.github.io/" target="_blank">IR kernel</a>: it executes R code which the front-end (Jupyter Notebook) submits to the kernel. It contains code writen in R to access, download, manipulate, analyze, and plot data on the species interaction networks stored in the Web Of Life database (a kind of API). It is like the recipe to generate a specific result.

<a href="https://mybinder.org/" target="_blank">Binder</a> can be used to make science reproducible. We can give people our data and our data analysis scripts, but not our computing environment - or at least not in a way that was easy to run. With Binder, you can add a badge that opens a Jupyter notebook---stored in github---in a executable environment, making your code immediately reproducible by anyone, anywhere. <a href="https://mybinder.org/v2/gh/miguelfortuna/WebOfLife/master" target="_blank">Click here to run this notebook remotely</a>, without installing anything in your computer. In this way, we can share Jupyter notebooks with the scientific community and the public. We must follow standard practices by making our script files and main data files freely available online so anyone can inspect the details of our analysis, or explore new ideas using our data.

In order to execute Jupyter notebooks containing code writen in R, the following <a href="https://github.com/binder-examples/dockerfile-r" target="_blank">Dockerfile</a> was used to create a Jupyter R kernel in Binder. <a href="https://github.com/choldgraf" target="_blank">Thanks a lot!</a>

### R script:
The code is also available as an R script <a href="https://github.com/miguelfortuna/WebOfLife/blob/master/WebOfLife.r" target="_blank">(WebOfLife.r)</a>.



[![DOI](https://zenodo.org/badge/119046336.svg)](https://zenodo.org/badge/latestdoi/119046336)
