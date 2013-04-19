module IntMap = Map.Make(struct type t = int let compare = compare end)
type 'a int_map = 'a IntMap.t

type action = unit -> unit

type 'a versioned = 'a int_map

and 'a parent = Root | Parent of 'a segment
and 'a segment = { parent: 'a parent;
                   version: int;
                   mutable refcount: int;
                   written: 'a versioned list
                 }

and 'a revision = 'a segment * 'a segment

module Revision : sig
  type 'a t

  val create: 'a segment -> 'a segment -> 'a t
  val fork: 'a t -> action -> 'a t
  val join: 'a t -> 'a t -> unit
end = struct
  type 'a t = 'a revision

  let create root current = (root, current)
    
  let fork (root, current) action =
    
    
end


module Versioned : sig
  type 'a t

  val empty: 'a t

  val get: 'a t -> 'a revision -> 'a
  val get_current: 'a t -> 'a

  val set: 'a t -> 'a revision -> 'a -> unit
  val set_current: 'a t -> 'a -> unit

  val release: 'a t -> 'a segment -> 'a t
  val collapse: 'a t -> 'a revision -> 'a segment -> unit
  val merge: 'a t -> 'a revision -> 'a revision -> 'a segment -> unit
end = struct
  type 'a t = 'a versioned
      
  let empty = IntMap.empty

  let get vs r = ()
  let get_current vs = () 

  let set vs (mainrev, currrev) v =
    IntMap.add v (currrev.version) vs
  let set_current vs v = ()

  let release vs release = 
    IntMap.remove release.version vs
    
  let collapse vs mainrev parent = ()

  let merge mainrev joinrev join = ()

end



module Segment : sig
  type 'a t
    
  val root: 'a t
  val create: 'a t -> 'a t
  val release: 'a t -> 'a t
  val collapse: 'a t -> 'a revision -> unit
end = struct
  type 'a t = 'a segment

  let version_count = ref 0

  let root = { parent=Root; 
               version=0;
               refcount=0;
               written=[];
             }

  let create parent =
    (match parent with
      | Root -> ()
      | Parent s -> s.refcount <- s.refcount + 1
    );

    let version = !version_count in
    incr version_count;

    { parent; version; refcount=1; written=[] }
    
  let release segment =
    segment.refcount <- segment.refcount - 1;
    if segment.refcount == 0 then (
      List.map (fun v -> Versioned.release v) segment.written
    );
    segment

end
