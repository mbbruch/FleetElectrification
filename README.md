# RideHailElectrification

This project explores how a centrally owned and operated fleet would optimally purchase and dispatch its vehicles. 

It also considers how those optimal choices change if the fleet pays the external costs of its life cycle air emissions: *Does a Pigovian tax on the impacts of pollution and climate change encourage electric vehicle usage? How much are those impacts reduced?*

The code uses a series of functions to:
1. Import the RideAustin ridesourcing trip dataset from 2016-2017
2. Construct a representative sample of ~5000 trips
3. Read in estimates of private costs, and air emission external costs, of:
    +Vehicle purchases, including a discounted future resale value that depends on usage rate
    +Gasoline combustion
    +Battery charging from the grid's marginal generator
    +Vehicle manufacturing + disposal + recycling
    +Maintenance
    +Per-hour labor
4. Solve a mixed-integer linear program, via Gurobi, to minimize private (+ external) costs:
  +Fleet vehicle mix (number of combustion, hybrid electric, and battery electric vehicles) 
  +Fleet operations (vehicle routing and battery charging) 
5. Summarize results
