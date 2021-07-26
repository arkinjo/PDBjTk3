module type POINT = 
  sig
    type t
    val coord : t -> (float * float * float)
  end

module type S =
  sig
    type point
    type t
    val default_size : float
    val neighbors_coord : t -> float * float * float -> point list
    val neighbors : t -> point -> point list
    val iter_neighbors : (point -> unit) -> t -> point -> unit
    val fold_neighbors : (point -> 'a -> 'a) -> t -> point -> 'a -> 'a
    val make : ?size:float -> point list -> t
  end

module Make (P: POINT) : (S with type point = P.t) =
  struct
    type point = P.t
	  (** Record for IntCell. *)
    type t = {
	size: float;
	cell: point list array array array;
	xmin: float; ymin: float; zmin: float;
	xmax: float; ymax: float; zmax: float;
	nx: int; ny: int; nz: int;
      }

    let default_size = 15.0
    let _nn = [ (0,0,0); 
		(1,0,0); (0,1,0); (0,0,1); 
		(-1,0,0); (0,-1,0); (0,0,-1);
		(1,1,0); (-1,1,0); (1,-1,0); (-1,-1,0); 
		(1,0,1); (-1,0,1); (1,0,-1); (-1,0,-1);
		(0,1,1); (0,-1,1); (0,1,-1); (0,-1,-1);
		(1,1,1); (-1,1,1); (1,-1,1); (-1,-1,1); 
		(1,1,-1); (-1,1,-1); (1,-1,-1); (-1,-1,-1)]

    (** [locate_coord intcell (x,y,z)] returns the grid point in which the point (x,y,z) is located. *)
    let locate_coord cell (x,y,z) = 
      (int_of_float ((x -. cell.xmin) /. cell.size)),
      (int_of_float ((y -. cell.ymin) /. cell.size)),
      (int_of_float ((z -. cell.zmin) /. cell.size)) 
    (** [locate intcell atom] returns the grid point in which the atom is located. *)
    let locate cell point = locate_coord cell (P.coord point)

    (** Returns a list of atoms near a point po=(x,y,z) *)
    let neighbors_coord cell coord = 
      let i,j,k = locate_coord cell coord in
      List.fold_left (fun nnp (ii,jj,kk) ->
	let i',j',k' = i + ii, j + jj, k + kk in
	if i' < 0 || i' >= cell.nx || 
	j' < 0 || j' >= cell.ny || k' < 0 || k' >= cell.nz then
	  nnp
	else 
	  List.fold_left (fun aa a ->
	    a :: aa) nnp cell.cell.(i').(j').(k'))
	[] _nn

    (** iterate function over neighbors *)
    let iter_neighbors f cell point = 
      let i,j,k = locate cell point in
      List.iter (fun (ii,jj,kk) ->
	let i',j',k' = i + ii, j + jj, k + kk in
	if i' < 0 || i' >= cell.nx || 
	j' < 0 || j' >= cell.ny || k' < 0 || k' >= cell.nz then
	  ()
	else 
	  List.iter (fun a -> f a) cell.cell.(i').(j').(k'))
	_nn

    (** fold function over neighbors *)
    let fold_neighbors (f: point -> 'a -> 'a) cell point init = 
      let i,j,k = locate cell point in
      List.fold_left (fun acc (ii,jj,kk) ->
	let i',j',k' = i + ii, j + jj, k + kk in
	if i' < 0 || i' >= cell.nx || 
	j' < 0 || j' >= cell.ny || k' < 0 || k' >= cell.nz then
	  acc
	else 
	  List.fold_right f cell.cell.(i').(j').(k') acc)
	init _nn

    (** Returns a list of atoms near the atom. *)
    let neighbors cell point = 
      neighbors_coord cell (P.coord point)

    let make ?(size = default_size) points = 
      let (xmin,xmax,ymin,ymax,zmin,zmax) = List.fold_left 
	  (fun (xmin,xmax,ymin,ymax,zmin,zmax) point ->
	    let x,y,z = P.coord point in
	    ((min x xmin),(max x xmax),
	     (min y ymin),(max y ymax),
	     (min z zmin),(max z zmax)))
	  (infinity,neg_infinity,infinity,neg_infinity,infinity,neg_infinity)
      points 
      in
      let xmin = xmin -. size and xmax = xmax +. size 
      and ymin = ymin -. size and ymax = ymax +. size 
      and zmin = zmin -. size and zmax = zmax +. size in
      let nx = 1 + int_of_float ((xmax -. xmin) /. size)
      and ny = 1 + int_of_float ((ymax -. ymin) /. size) 
      and nz = 1 + int_of_float ((zmax -. zmin) /. size) in
      let cell = Array.init nx (fun i -> Array.make_matrix ny nz []) in
      let icell = {size=size; cell = cell; nx=nx; ny=ny; nz=nz; 
		   xmin = xmin; ymin = ymin; zmin = zmin;
		   xmax = xmax; ymax = ymax; zmax = zmax;}
      in
      List.iter
	(fun p ->
	  let (i,j,k) = locate icell p in
	  icell.cell.(i).(j).(k) <- p :: icell.cell.(i).(j).(k)) 
	points;
      icell
  end

module Atom = Make(Atom)
