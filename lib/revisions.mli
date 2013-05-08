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

(**
*)
module type Revision = sig

  type 'a t

  (** [fork action] returns a new revision based off the current revision,
      running the action [action] within.

      @param action concurrent task started by fork in new revision
   *)
  val fork: action:(unit -> unit) -> 'a t
  
  (** [join join] waits until the concurrent task association with revision
      [join] is done.

      @param join revision being joined (the {e joiner})
   *)
  val join: join:'a t -> unit

end

(** Implements the {e isolation type} referred to above. Used to declare data
    shared between concurrently executing {e revisions}. 
 *)
module type Versioned = sig

  module R : Revision

  type 'a t

  (** [curr ()] returns the value at the current version. *)
  val curr: unit -> 'a t      

  (** [get r] returns the versioned type at revision [r].

      @param r the revision of the type being fetched
   *)
  val get: r:'a R.t -> 'a t

  (** [set r v] sets the value of the versioned type at revision [r] to [v],
      returning the type at revision [r].
      
      @param r the revision of the type being set
      @param v the value being set
   *)
  val set: r:'a R.t -> v:'a t -> 'a t

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
