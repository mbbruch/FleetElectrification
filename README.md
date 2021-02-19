# Optimization of Ridesourcing Fleet Electrification

This project contains the code used to run the experiments presented in the paper [Effects of Air Emission Externalities on Optimal Ridesourcing Fleet Electrification and Operations](https://pubs.acs.org/doi/pdf/10.1021/acs.est.0c05141).^1

It explores how a centrally owned and operated fleet would optimally purchase and dispatch its vehicles. It also considers how those optimal choices change if the fleet pays the external costs of its life cycle air emissions. *Does a Pigovian tax on pollution and climate change impacts encourage electric vehicle usage? How much do emissions fall?*

The code uses a series of functions to:
1. Import the RideAustin ridesourcing trip dataset from 2016-2017
2. Construct a representative sample of ~5000 trips
3. Read in or estimate the private costs, and air emission external costs, of:
    + Vehicle purchases, including a discounted cash flow for each vehicle's future resale value that depends on age, usage rate, and powertrain
    + Gasoline combustion
    + Battery charging from the grid's marginal generator
    + Vehicle manufacturing + disposal + recycling
    + Maintenance
    + Per-hour driver costs
4. Use Gurobi and heuristics to solve a mixed-integer linear program that minimizes costs by optimizing:
    + Purchases: number of combustion, hybrid electric, and battery electric vehicles in the fleet
    + Operations: vehicle routing and battery charging
5. Summarize results

1: If this project is useful for any publications, please consider citing it:
```
@article{doi:10.1021/acs.est.0c05141,
author = {Bruchon, Matthew B. and Michalek, Jeremy J. and Azevedo, Inês L.},
title = {Effects of Air Emission Externalities on Optimal Ridesourcing Fleet Electrification and Operations},
journal = {Environmental Science \& Technology},
volume = {0},
number = {0},
year = {2021},
doi = {10.1021/acs.est.0c05141}
}
```

