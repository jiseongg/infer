(* Abstract state with heap, store together *)

include module type of AbstractDomain.Pair (AbsHeap) (AbsStore)
