Transformers for the Aivika simulation library

The represented [aivika-transformers](http://hackage.haskell.org/package/aivika-transformers) package 
is a generalization of the [aivika](http://hackage.haskell.org/package/aivika) simulation library with 
extensive use of monad transformers and type families. It can be applied for nested simulation,
package [aivika-branches](http://hackage.haskell.org/package/aivika-branches), and
parallel distributed simulation, package [aivika-distributed](http://hackage.haskell.org/package/aivika-distributed).

Unlike sequential simulation, the distribution simulation is more difficult for implementing
the simulation experiments by the Monte-Carlo method. Therefore, there are additional packages
that allow saving the results of distributed simulation in SQL databases
and only then the simulation reports are generated. These reports are HTML pages
with charts, histograms, links to CSV tables, statistics summary and so on.

This method can be used not only for the parallel distributed simulation, but also for other 
simulation models created with help of the generalized version of the Aivika simulation library.
Please consult the [AivikaSoft](http://www.aivikasoft.com) website for more details.
