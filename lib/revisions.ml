module IntMap = Map.Make(struct type t = int let compare = compare end)
type 'a int_map = 'a IntMap.t

type action = unit -> unit

type 'a versioned = 'a int_map
and 'a segment = Root | Parent of 'a segment * int * int ref * 'a versioned list
and 'a revision = 'a segment (* root *) * 'a segment (* current *) * 'a Lwt.t (* thread and task*)

module type Versioned = sig
  type 'a t

  val get: 'a t -> 'a revision -> 'a
  val set: 'a t -> 'a revision -> 'a -> unit
    
  val get_current: 'a t -> 'a
  val set_current: 'a t -> 'a -> unit

  val release: 'a t -> 'a segment -> unit
  val collapse: 'a t -> 'a revision -> 'a segment -> unit
  val merge: 'a t -> 'a revision -> 'a revision -> 'a segment -> unit
end 

module type Revision = sig
  type 'a t

  val create: 'a segment -> 'a segment -> 'a t
  val fork: 'a t -> action -> 'a t
  val join: 'a t -> 'a t -> unit
end

module type Segment = sig
  type 'a t
    
  val root: 'a t
  val create: 'a t -> unit
  val release: 'a t -> unit
  val collapse: 'a t -> 'a revision -> unit
end
