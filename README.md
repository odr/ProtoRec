# ProtoRec
Ideas for [record](https://github.com/nikita-volkov/record) library.

* [Record](https://github.com/nikita-volkov/record) generates many FieldOwner instances but five instances is enough.  
Instead of direct (O(1)) field access we will have logarithmic (O(log n)) coplexity though.

Unfortunately, it doesn't work...
