
Version 4.7
-----

* Added the Composite monad transformer.

* Added the Channel computation.

* Breaking change: modified signatures of functions signalStream and streamSignal.

* Breaking change: the signalProcessor function is replaced with channelProcessor.

* Breaking change: the processorSignaling function is replaced with processorChannel.

* Added module Signal.Random.

Version 4.6
-----

* Removed the MonadTemplate type class as it often caused overlapping family data
  instances.

* Updated module DoubleLinkedList.

* Breaking change: arrows Net and Processor are trying to perform computations
  in parallel as possible, when using the proc notation. Earlier they executed
  sequentially.
