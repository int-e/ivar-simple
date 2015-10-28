## What is this?

ivar-simple provides immutable, write-once variables (`IVar`s) in the
`Data.IVar.Simple` module.

It also provides two more experimental channel implementations built on
top of `IVar`s,

* `Data.IVar.Simple.IChan`:
  multi-cast channels with write-once semantics.
* `Data.IVar.Simple.MIChan`:
  channels with write semantics similar to Control.Concurrent.Chan

## Comparison to data-ivar

Both data-ivar and ivar-simple provide a write-once variable. That's where
the similarities end:

* Reading an `IVar` with data-ivar is an `IO` operation. In ivar-simple it's
  a pure function.
* data-ivar provides a `Reader` monoid, monad, etc. for reading from one of
  several `IVar`s. ivar-simple has no such functionality.
* The data-ivar implementation can, in principle, add arbitrary `IO` actions
  that are called when an `IVar` is written. (This detail is not exposed,
  however)
* Technically, ivar-simple tries for efficiency by exploiting the existing
  locking structures in the RTS; in particular, reading a full `IVar` for
  a second time is as cheap as evaluating a record selector.
  (I have no performance numbers for this.)
