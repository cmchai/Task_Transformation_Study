# This is the offcial repository of the Task Transformation Study

In this study, we aimed to investigate whether people can dynamically modulate their task preparation based on the level of task uncertainty.

citation: Chai, M., Holroyd, C. B., Brass, M., & Braem, S. (2024). Dynamic changes in task preparation in a multi-task environment: The task transformation paradigm. *Cognition*, 247, 105784. 

DOI: <https://doi.org/10.1016/j.cognition.2024.105784>

## Aim of this repository

This repository was created to openly share the scripts that I created for the computer-based experiment, as well as the scripts used in data analysis. Thus, I only included scripts, and not stimuli used in the experiment. For the image stimuli used in our experiments, please go to [our repository](https://osf.io/zyfb9/) on Open Science Framework (OSF).

## Structure of this repository

The experiment scripts \(mostly in Javascript and html\) were organized based on the experiment number \(1-4, and a pilot\). Since they are online experiments, thus they have to be hosted on a webserver.
the analyis scripts were mainly in R and in need of R and RStudio installed on your local computer. we have also conducted analysis after combining datasets across experiments \(Btn_Exps\).

In addition to the analysis scripts for each experiment, I include the scripts created for computational modelling on the behavioral data. We employed drift diffusion models \(DDM\) and used Bayesian hierarchical modeling to exmaine the underlying evidence accumulation process. This part of script was in Python. the results from the DDM was further analysed using R. 

## Bugs and improvement

if you run into issues when running this script, or your think something can be improved, please contact the author \(chaimengqiao@\[gmail\].com\)
