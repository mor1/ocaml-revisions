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

open Printf

type 'a versioned = (int, 'a) Hashtbl.t
and 'a revision = {
  root: 'a segment;
  curr: 'a segment;
  task: task;
}
and 'a segment = {
  parent: 'a segment;
  version: int;
  refcount: int;
  written: 'a versioned list
}
and task = Thread.t

type 'a st = {
  mutable current_rev: 'a revision
}

let tls:(int, 'a st) Hashtbl.t = Hashtbl.create 8

module Versioned = struct

  let lookup t k = Hashtbl.(if mem t k then Some (find t k) else None)

  let get ~vt ~r =
    let rec follow t s = 
      match lookup t s.version with
        | None -> follow t s.parent (* assumes there's a parent *)
        | Some v -> v
    in
    follow vt r.curr

  let curr ~(vt:'a versioned) = 
    let tid = Thread.(id (self ())) in
    match lookup tls tid with
      | None 
        -> failwith (sprintf "thread has no state! tid=%d" tid)
      | Some v
        -> get vt v.current_rev

  let set vt r v =
    ()

  let merge main join_rev joiner = 
    ()

  let collapse main parent =
    ()

  let release r = 
    ()

end

module Revision  = struct

    

end


module Segment = struct

end
