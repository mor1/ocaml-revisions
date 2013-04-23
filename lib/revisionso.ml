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

let version_count = ref 0

class versioned = object (self : 'self)
  val versions = IntMap.empty

  method release (release:segment) = 
    let v = 
      try Some (IntMap.find (release#version) versions)
      with Not_found -> None
    in
    match v with
      | None -> assert false
      | Some v -> IntMap.add 
end

and revision (root:segment) (current:segment) = object (self : 'self)
  val mutable root = root
  val mutable current = current
  val mutable task : Thread.t option = None
  val mutable current_revision = None

  method fork (action: action) = 
    let r = (new revision) current ((new segment) (Some current)) in
    current#release;
    let current = new segment (Some current) in
    task <- Some (Thread.create (fun () -> 
      let previous = current_revision in
      current_revision <- Some r;
      action ();
      current_revision <- previous
    ) ());
    r

  method join (join:revision) =
    Thread.join join#task; (* join.task.Wait(); *)
    let s = ref (join#current) in
    while s <> join#root do
      List.iter (fun v -> v#merge self (!s)) (!s)#written;
      s := (!s)#parent
    done;
    join#current#release ();
    current#collapse self
end

and segment (parent:segment) = object (self : 'self)
  val mutable version = let vc = !version_count in (incr version_count; vc)
  val mutable refcount = 1
  val mutable written = ([]:versioned list)
    
  val mutable parent = None

  initializer begin
    (match parent with
      | None -> ()
      | Some s -> refcount <- refcount + 1
    );
    self.parent <- parent
  end

  method release = 
    refcount <- refcount - 1;
    if refcount = 0 then 
      List.iter (fun v -> v#release self) written;
    match parent with
      | None -> ()
      | Some s -> s#release

end

