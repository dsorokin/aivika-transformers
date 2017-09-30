
Version 5.3
-----

* Introduced the result source titles, which can be useful when plotting the charts.

* Added functions newSignalInGridTimes and gridTimes.

Version 5.2
-----

* Using the mwc-random package for generating random numbers by default.

Version 5.1
-----

* Includes changes destined for Aivika Exension Pack.

* Minor changes in the resource preemption statistics.

* Added the statistics reset.

Version 5.0
-----

* Added the Composite monad transformer.

* Added the Channel computation.

* Breaking change: modified signatures of functions signalStream and streamSignal.

* Breaking change: the signalProcessor function is replaced with channelProcessor.

* Breaking change: the processorSignaling function is replaced with processorChannel.

* Added module Signal.Random.

* Added functions arrivalTimerSignal and arrivalTimerChannel.

* Added functions queuedSignalStream, queuedProcessorChannel and queuedChannelProcessor.

Version 4.6
-----

* Removed the MonadTemplate type class as it often caused overlapping family data
  instances.

* Updated module DoubleLinkedList.

* Breaking change: arrows Net and Processor are trying to perform computations
  in parallel as possible, when using the proc notation. Earlier they executed
  sequentially.
