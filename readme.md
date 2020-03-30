# LAX county CHIME models

This repositoy contains various versions of the [CHIME model](https://github.com/CodeForPhilly/chime) re-written in R / Shiny. 

Basic functionality is demonstrated in the demos/ folder.


- basicSIR_demo.R contains a minimum working example of the deterministic chime model
- stochasticSIR_demo.R contains a minimum working example of the chime model where SIR parameters are vectors of parameters
- bayesSEIR_demo.R contains a minimum working example of the Bayesian SEIR model

The repository contains two shiny apps.

- rChime: a replication of the deterministic chime model.  No longer developing this, just a proof of concept.
- bayesChime: the bayesian version of the chime model, incorporating a time-lag in data collection and social distancing


## Getting Started

This project was developed in R version 3.5.1 (Feather Spray).  To install the primary dependencies run source/setup.R

## Next steps / todo

- Implementing the time lag in a more intuitive/elegant way
- Writing an R version of the negative binomial sampler
- Cleaning up the labeling
- Implementing data-driven methods for predicting demand.


## Contributing

## Versioning

## Authors

## License

## Acknowledgments

