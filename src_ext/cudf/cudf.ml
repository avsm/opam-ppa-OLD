(*****************************************************************************)
(*  libCUDF - CUDF (Common Upgrade Description Format) manipulation library  *)
(*  Copyright (C) 2009-2012  Stefano Zacchiroli <zack@upsilon.cc>            *)
(*                                                                           *)
(*  This library is free software: you can redistribute it and/or modify     *)
(*  it under the terms of the GNU Lesser General Public License as           *)
(*  published by the Free Software Foundation, either version 3 of the       *)
(*  License, or (at your option) any later version.  A special linking       *)
(*  exception to the GNU Lesser General Public License applies to this       *)
(*  library, see the COPYING file for more information.                      *)
(*****************************************************************************)

open ExtLib
open Printf

open Cudf_types
open Cudf_types_pp

exception Constraint_violation of string

type package = {
  package : pkgname ;
  version : version ;
  depends : vpkgformula ;
  conflicts : vpkglist ;
  provides : veqpkglist ;
  installed : bool ;
  was_installed : bool ;
  keep : enum_keep ;
  pkg_extra : typed_value stanza ;
}

type request = {
  request_id : string ;
  install : vpkglist ;
  remove : vpkglist ;
  upgrade : vpkglist ;
  req_extra : typed_value stanza ;
}
type preamble = {
  preamble_id : string ;
  property : typedecl ;
  univ_checksum: string ;
  status_checksum: string ;
  req_checksum: string ;
}
type cudf_doc = preamble option * package list * request
type cudf_item =
    [ `Preamble of preamble | `Package of package | `Request of request ]
type universe = {
  id2pkg: ((string * int), package) Hashtbl.t;	(** <name, ver> -> pkg *)
  name2pkgs: (string, package) Hashtbl.t; (** name -> pkg (multi-bindings) *)
  uid2pkgs: (int, package) Hashtbl.t; (** int uid -> pkg *)
  id2uid: ((pkgname * version), int) Hashtbl.t; (** <name, ver> -> int uid *)
  features: (string, (package * version option)) Hashtbl.t;
  (** feature -> avail feature versions (multi-bindings) 
      Each available feature is reported as a pair 
      <owner, provided version>, where owner is the package
      providing it. Provided version "None" means "all possible
      versions" *)
  mutable univ_size : int;
  mutable inst_size : int;
}
type cudf = preamble * universe * request
type solution = preamble * universe

let universe_size univ = univ.univ_size
let installed_size univ = univ.inst_size

let (=%) pkg1 pkg2 =
  pkg1.package = pkg2.package && pkg1.version = pkg2.version

let (<%) pkg1 pkg2 =
  Pervasives.compare (pkg1.package, pkg1.version) (pkg2.package, pkg2.version)

let (>%) pkg1 pkg2 =
  Pervasives.compare (pkg2.package, pkg2.version) (pkg1.package, pkg1.version)

let default_preamble = {
  preamble_id = "" ;
  property = [] ;
  univ_checksum = "" ;
  status_checksum = "" ;
  req_checksum = "" ;
}

let default_package = {
  package = "" ;
  version = 0 ;
  depends = [] ;
  conflicts = [] ;
  provides = [] ;
  installed = false ;
  was_installed = false ;
  keep = `Keep_none ;
  pkg_extra = [] ;
}

let default_request = {
  request_id = "" ;
  install = [] ;
  remove = [] ;
  upgrade = [] ;
  req_extra = [] ;
}

let empty_universe () =
  { id2pkg = Hashtbl.create 1023 ;
    uid2pkgs = Hashtbl.create 1023;
    id2uid = Hashtbl.create 1023;
    name2pkgs = Hashtbl.create 1023 ;
    features = Hashtbl.create 1023 ;
    univ_size = 0 ; inst_size = 0 ;
  }

(** process all features (i.e., Provides) provided by a given package
    and fill with them a given feature table *)
let expand_features pkg features =
    List.iter
      (function
        | name, None -> Hashtbl.add features name (pkg, None)
        | name, Some (_, ver) -> Hashtbl.add features name (pkg, (Some ver)))
      pkg.provides

let load_universe pkgs =
  let univ = empty_universe () in
  let uid = ref 0 in
  List.iter
    (fun pkg ->
      let id = pkg.package, pkg.version in
      Hashtbl.add univ.uid2pkgs !uid pkg;
      Hashtbl.add univ.id2uid id !uid;
      incr uid;
      if Hashtbl.mem univ.id2pkg id then
	raise (Constraint_violation
		 (sprintf "duplicate package: <%s, %d>"
		    pkg.package pkg.version));
      Hashtbl.add univ.id2pkg id pkg;
      Hashtbl.add univ.name2pkgs pkg.package pkg;
      expand_features pkg univ.features;
      univ.univ_size <- univ.univ_size + 1;
      if pkg.installed then
	univ.inst_size <- univ.inst_size + 1)
    pkgs;
  univ

let package_by_uid univ = Hashtbl.find univ.uid2pkgs
let uid_by_package univ pkg =
  Hashtbl.find univ.id2uid (pkg.package, pkg.version)

let lookup_package univ = Hashtbl.find univ.id2pkg
let mem_package univ = Hashtbl.mem univ.id2pkg

let iter_packages f univ = Hashtbl.iter (fun _id pkg -> f pkg) univ.id2pkg
let iteri_packages f univ = Hashtbl.iter (fun _id pkg -> f _id pkg) univ.uid2pkgs

let fold_packages f init univ =
  Hashtbl.fold (fun _id pkg acc -> f acc pkg) univ.id2pkg init

let get_packages ?filter univ =
  match filter with
    | None -> fold_packages (fun acc pkg -> pkg :: acc) [] univ
    | Some test ->
	fold_packages
	  (fun acc pkg -> if test pkg then pkg :: acc else acc)
	  [] univ

let (|=) v = function
  | None -> true
  | Some (`Eq, v') -> v = v'
  | Some (`Neq, v') -> v <> v'
  | Some (`Geq, v') -> v >= v'
  | Some (`Gt, v') -> v > v'
  | Some (`Leq, v') -> v <= v'
  | Some (`Lt, v') -> v < v'

let version_matches = (|=)

let status univ =
  let univ' = empty_universe () in
  Hashtbl.iter
    (fun id pkg -> match pkg with
    | { installed = true } ->
      Hashtbl.add univ'.id2pkg id pkg;
      Hashtbl.add univ'.name2pkgs pkg.package pkg;
      expand_features pkg univ'.features
    | _ -> ())
    univ.id2pkg;
  univ'.inst_size <- univ.inst_size;
  univ'.univ_size <- univ.inst_size; (* as we filtered on installed pkgs *)
  univ'

let lookup_packages ?(filter=None) univ pkgname = 
  let packages = Hashtbl.find_all univ.name2pkgs pkgname in
    match filter with
	None -> packages
      | Some _ as pred -> List.filter (fun p -> p.version |= pred) packages

let get_installed univ pkgname =
  List.filter (fun { installed = i } -> i) (lookup_packages univ pkgname)

let mem_installed ?(include_features = true) ?(ignore = fun _ -> false)
    univ (name, constr) =
  let pkg_filter = fun pkg -> not (ignore pkg) in
  let mem_feature constr =
    let feats = Hashtbl.find_all univ.features name in
      List.exists
	(function
           | owner_pkg, _ when not owner_pkg.installed -> false
	   | owner_pkg, None -> pkg_filter owner_pkg
	   | owner_pkg, Some v -> pkg_filter owner_pkg && v |= constr)
	feats in
  let pkgs = List.filter pkg_filter (get_installed univ name) in
    List.exists (fun pkg -> pkg.version |= constr) pkgs
    || (include_features && mem_feature constr)

let who_provides ?(installed=true) univ (pkgname, constr) =
  List.filter
    (function 
      |pkg , _ when not pkg.installed && installed -> false
      |_, None -> true 
      | _, Some v -> v |= constr
    )
    (Hashtbl.find_all univ.features pkgname)


let lookup_typed_package_property pkg = function
  | "package" -> `Pkgname pkg.package
  | "version" -> `Posint pkg.version
  | "depends" -> `Vpkgformula pkg.depends
  | "conflicts" -> `Vpkglist pkg.conflicts
  | "provides" -> `Veqpkglist pkg.provides
  | "installed" -> `Bool pkg.installed
  | "keep" -> `Enum (keep_enums, string_of_keep pkg.keep)
  | prop_name -> List.assoc prop_name pkg.pkg_extra

let lookup_typed_request_property req = function
  | "request" -> `String req.request_id
  | "install" -> `Vpkglist req.install
  | "remove" -> `Vpkglist req.remove
  | "upgrade" -> `Vpkglist req.upgrade
  | prop_name -> List.assoc prop_name req.req_extra

let lookup_typed_preamble_property pre = function
  | "preamble" -> `String pre.preamble_id
  | "property" -> `Typedecl pre.property
  | "univ-checksum" -> `String pre.univ_checksum
  | "status-checksum" -> `String pre.status_checksum
  | "req-checksum" -> `String pre.req_checksum
  | _ -> raise Not_found


let lookup_package_property pkg prop =
  string_of_value (lookup_typed_package_property pkg prop)

let lookup_request_property req prop =
  string_of_value (lookup_typed_request_property req prop)

let lookup_preamble_property pre prop =
  string_of_value (lookup_typed_preamble_property pre prop)


let lookup_package_typedecl ?(extra = []) prop =
  List.assoc prop (Cudf_conf.package_typedecl @ extra)
