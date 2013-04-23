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

