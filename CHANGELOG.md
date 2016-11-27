
Version 4.6
-----

* Removed the MonadTemplate type class as it often caused overlapping family data
  instances.

* Updated module DoubleLinkedList.

* Breaking change: arrows Net and Processor are trying to perform computations
  in parallel as possible, when using the proc notation. Earlier they executed
  sequentially.
