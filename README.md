# A Bayesian approach to clustering categorical data through mixture models ​
*(Bayesian Clustering of Categorical Data with Latent Class Models)*

A comprehensive simulation study comparing classical and Bayesian approaches for clustering categorical data, with a focus on Latent Class Models.

## Abstract

The growing availability of categorical data, especially in fields such as market research, bioinformatics, and the social sciences, has rendered many traditional clustering approaches, such as k-means or classic Gaussian mixture models, inadequate. These methods are primarily based on assumptions or distance metrics unsuitable for discrete data. This thesis focuses on proposing a technique for categorical data by leveraging multinomial mixture models. Specifically, Latent Class Models are studied in depth, first in a classical context and then focusing on the Bayesian framework.

The main frequentist inferential tools for estimating these models are presented, such as the Expectation-Maximization (EM) algorithm and its direct evolution for clustering, CEM. Within the Bayesian framework, the Data Augmentation technique is proposed to sample from the posterior distribution after defining the model's prior distributions, both of which are Dirichlet.

The core of the thesis is in the third chapter, which features an in-depth simulation study to compare these models in different application contexts. In addition to the comparison between the classical and Bayesian approaches, in this chapter, the models were compared with others developed or adapted for clustering categorical data, such as k-modes or the generalized Bayes product partition model.

The results highlight the superior robustness and accuracy of the Bayesian approach, especially in complex contexts characterized by high overlap between groups, unbalanced cluster proportions, and small sample sizes. Although computationally more expensive, the Bayesian framework proved to be more reliable in identifying the correct latent partitions.

Within the same simulation study, different techniques for mitigating the label switching problem, a well-known issue in the context of Bayesian mixture models, are compared. Furthermore, an in-depth analysis of the elicitation of hyperparameters for the prior distributions is included, proposing an empirical Bayes approach based on estimates obtained from classical methods.

To verify the reliability of the proposed methods, an applied study is conducted by studying the underlying latent classes in a dataset provided by the University of Northern Ireland concerning the correlation between social media use and sleep-related problems during the last phase of the COVID-19 pandemic. The results obtained are compared with a causality study conducted on the same data, noting similarities and differences.

## Repository Structure

* **/Scenari**: Contains the R scripts for the different simulation scenarios studied in the thesis.
* **/Funzioni**: Includes custom R functions used across the different simulations.
* **/Grafici**: Scripts used to generate the plots and graphics for the results.
* **/Risultati**: Contains saved results from the simulations, such as tables or RData files.

