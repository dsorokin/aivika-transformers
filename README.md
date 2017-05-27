Transformers for the Aivika simulation library

[This package](http://hackage.haskell.org/package/aivika-transformers) is a generalization of 
the [aivika](http://hackage.haskell.org/package/aivika) simulation library with extensive use 
of monad transformers and type families. It can be applied for 
[nested simulation](http://hackage.haskell.org/package/aivika-branches) and 
[parallel distributed simulation](http://hackage.haskell.org/package/aivika-distributed).

Unlike sequential simulation, the distribution simulation is more difficult to implement for
providing the modeler with the simulation experiments by the Monte-Carlo method. Therefore, 
[Aivika Exension Pack](http://www.aivikasoft.com/en/products/aivika-extension-pack.html)
saves the results of distribution simulation in SQL databases
and only then the simulation reports are generated. These reports are HTML pages
with charts, histograms, links to CSV tables, statistics summary and so on.

Moreover, Aivika Exension Pack allows the modeler to run the simulation experiments 
not only for the parallel distribution simulation, but for other simulation models
created with help of the generalized version of the Aivika simulation library.
