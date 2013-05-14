(*
    Copyright (c) 2013, Richard Mortier <mort@cantab.net>.
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

(** Revisions and versioned types, from "{i {{:
    http://dx.doi.org/10.1145/1869459.1869515}Concurrent Programming with
    Revisions and Isolation Types}}", Burckhardt, Baldassin, Leijen. OOPSLA
    2010.

    The OOPSLA'10 paper describes a mechanism for simplyfying parallel
    execution of different application tasks. From the abstract:

    "Programmers declare what data they wish to share between tasks by using
    isolation types, and execute tasks concurrently by forking and joining
    revisions. These revisions are isolated: they read and modify their own
    private copy of the shared data only. A runtime creates and merges copies
    automatically, and resolves conflicts deterministically, in a manner
    declared by the chosen isolation type."

    @author Richard Mortier <mort\@cantab.net>
    @version 0.0.1

*)

type 'a versioned
type 'a revision
type 'a segment
type task


(** Implements the {e isolation type} referred to above. Used to declare data
    shared between concurrently executing [Revision]s. 
 
 *)
module Versioned : sig

  (** [get vt r] returns the value of the versioned type [vt] at revision [r].

      @param vt the concrete versioned data
      @param r the revision of the type being fetched
   *)
  val get: vt:'a versioned -> r:'a revision -> 'a

  (** [curr vt] returns the value in [vt] at the current revision.

      @param vt the concrete versioned data
   *)
  val curr: vt:'a versioned -> 'a
  
  (** [set vt r v] sets the value of the versioned type [vt] at revision [r]
      to [v], returning the type at revision [r].
      
      @param vt the concrete versioned data
      @param r the revision of the type being set
      @param v the value being set
   *)
  val set: vt:'a versioned -> r:'a revision -> v:'a -> unit

  (** [release vt release] marks the entry in [vt] for the [release] segment
      version as null.

      @param vt the concrete versioned data
      @param release the segment to release
   *)
  val release: vt: 'a versioned -> release:'a segment -> unit

  (** [collapse main parent] XXX
   *)
  val collapse: main:'a revision -> parent:'a segment -> unit

  (** [merge main join_rev joiner] 
   *)
  val merge: main:'a revision -> join_rev:'a revision -> joiner:'a segment -> unit

end


(** Implements the {! Segment}, the container for shared state between
    revisions. Essentially contains its own version number, a reference
    counter for its child segments, a reference to its parent, plus a list of
    the shared ([Versioned]) data. Currently this list is an ['a versioned
    list], i.e., restricted to a particular ['a] -- ideally this would just be
    a [versioned list].

 *)
module Segment : sig

  (** [create parent] generates a new ['a segment] with the specified parent.
   *)
  val create: parent:'a segment -> 'a segment

  (** [release target] is called to decrement [target]'s refcount and
      potentially garbage collect it by releasing all its [Versioned]
      variables, and its parent.

   *)
  val release: target:'a segment -> unit

  (** [collapse main] collapses the graph if it consists of a single thread of
      segments up to [main]. This entails in turn collapsing all the versioned
      data in each immediate parent and removing that parent.
      
   *)
  val collapse: main:'a revision -> unit

end


(** Implements the [Revision], the basic unit of concurrency. Essentially
    contains references to the current and root {! Segment}s, plus some [task]
    structure (for now, a {! Thread.t}).

    [Revision]s essentially function as asynchronous tasks that are forked and
    joined, and may fork and join other revisions. Conceptually similar to
    revisions (branches) in revision control systems, and so (logically)
    records HEAD and ROOT.

    Note that the main thread is itself considered a revision, and all forked
    revisions {b must} be explicitly joined. Represented as a vertical line
    joining two {! Segment}s in a Revision Diagram.

 *)
module Revision : sig

  (** [create root curr]
   *)
  val create: root:'a segment -> curr:'a segment -> 'a revision

  (** [fork action] returns a new revision based off the current revision,
      running the action [action] within.

      @param action concurrent task started by fork in new revision
   *)
  val fork: action:(unit -> unit) -> 'a revision
  
  (** [join joiner] waits until the concurrent task association with revision
      [joiner] is done.

      @param joiner revision being joined
   *)
  val join: joiner:'a revision -> unit

end


(*
  module type Cumulative = sig

  type 'a t

(** [merge original master revised] called when merging results of a
  versioned type.
  
  @param original original value at the time the revision forked
  @param master current value in the revision performing the join
  @param revised current value in the revision joining
 *)
  val merge: original:'a t -> master:'a t -> revised:'a t -> 'a t

end
 *)
