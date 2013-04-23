(*
    Copyright (c) 2013, Richard Mortier <mort@cantab.net> 
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are
    met:

    + Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.

    + Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
    IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
    THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
    PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
    CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
    EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
    PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
    PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
    NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*)

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
