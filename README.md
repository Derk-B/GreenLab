# GreenLab - BrewCrashEffect

## Install instructions
The python requirements can be installed by running the following command in the root of the repo:
```
pip install -r requirements.txt
```
This will install all the required python packages.  
  
The required R packages are listed on the top of the [R file](./DataManipulation.R) and will be installed by running the file.


To install energiBridge you can follow the install instructions from their [repository](https://github.com/tdurieux/EnergiBridge)

## Dataset generation
To generate new datasets, add and or check sparsity the [dataset_generation](./dataset_generation.ipynb) notebook can be used. In this notebook you can confige which shape of data you want, how much sparsity you want and test what the approximate run time of the dataset will be, for both CPU and GPU runs.

## Running experiments
For running the experiment both the [experiment runner config](./RunnerConfig.py) and [bioscience_experiment](./bioscience_experiment.py) are used. It is required to change the config so that the runner knows where the experiment script ([bioscience_experiment](./bioscience_experiment.py)) and where the [datasets](./data/) are located, this location string may differ per operating system so it is configurable. To run the experiment, make sure the [configuration](./RunnerConfig.py) is correct and run the following command from the root folder:

```
python experiment-runner/ <path-to-runner-config>
```

After this command has been run the experiment runner will start running the experiments and the output of the first should soon be visible in the [experiments folder](./experiments/) where also our previous experiments are visible.

## Dataplotting
The [R file](./DataManipulation.R) contains all code for making the plots in the paper. You can link the final runtable and all packages will be installed and the output will be all the plots we used. 