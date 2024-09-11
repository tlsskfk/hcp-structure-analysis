# HCP Structure Analysis
## Built by Yi Wei @ University of Maryland
#### Maintained by Stephen Shin @ University of Maryland
#### Maintained by Nate Mearns-Escobar @ University of Maryland

This is a code repository for ongoing projects for bayesian and mediation analysis.

Still in progress (particularly with organization and documentation)

# Code
[Code](./Code)
>>>>>>> main
This repository contains R code designed to run bayesian inference models using brms and (for the time being) log transformed volumes.  Weights are given to categories to be used in each model (currently using 1 for each postiive weight).  Refer to [Description of Tasks](#Description-of-tasks) to understand the correlational points in the models.

## TODO
Adjust direcories to use tmp directory and reorgnaize zaratan components
Need to further document portions of each part of the repository and the script elements.

## Description of Tasks

The Rey Auditory Verbal Learning Test (RAVLT) measures how well subjects can remember and learn words. It looks at different aspects of memory, like short-term memory, learning strategies, and how well people can recall information after a delay. The RAVLT uses two unrelated world lists (List A and B). The first 5 trials prompt the subject with words from List A, with the subject then being tasked with recalling as many words from the list as possible. The 6th trial prompts the subject with words from List B, with the subject then tasked with recalling as many words from the list as possible. Lastly, in the 7th trial, the subject is asked to recall as many words from list A without hearing the list again. The interfering list (List A for trial 6, List B for trial 7) is tracked and scored alongside the primary list of interest, in case the subject recalls any interfering words. This test was administered through RAVLT using an iPad via the Q-Interactive platform.

The Flanker task measures the subject’s attentive ability and inhibitory control. The subject is tasked with focusing on a primary stimuli while simultaneously ignoring additional flanking stimuli. Ages 8-85 are given 20 trials with arrows, while ages 3-7 receive 20 additional trials with arrows if they score ≥90% on the fish stimuli (with no more than one congruent and one incongruent trial incorrect). The Flanker is a good measure of executive function. The skills assessed in this task are considered to be “fluid abilities,” with performance peaking in early adulthood and declining throughout the rest of the lifespan. Higher scores indicate higher levels of ability to pay attention while omitting distractions.

# Resources
[Bids Documentation](https://bids-specification.readthedocs.io/en/stable/introduction.html)

[Bids Overview](https://oesteban.github.io/ohbm19/#1)
[Bids Checker](https://bids-standard.github.io/bids-validator/)

BIDS was originally designed to describe and apply consistent naming conventions to raw (unprocessed or minimally processed due to file format conversion) data.
