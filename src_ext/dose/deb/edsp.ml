(**************************************************************************************)
(*  Copyright (C) 2011 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Copyright (C) 2011 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

(** Representation of a apt-get <-> solvers protocol edsp 0.3 *)

open ExtLib
open Common

include Util.Logging(struct let label = __FILE__ end) ;;

type request = {
  request : string;
  install : Format822.vpkg list;
  remove : Format822.vpkg list;
  autoremove : bool;
  upgrade : bool;
  distupgrade : bool;
  strict_pin : bool;
  preferences: string;
}

let default_request = {
  request = "";
  install = [];
  remove = [];
  autoremove = false;
  upgrade = false;
  distupgrade = false;
  strict_pin = false;
  preferences = ""
}

(* convert a apt command line request to edsp request *)
let from_apt_request request = function
  |Apt.Install vpkgreqlist ->
      List.fold_left (fun acc -> function
        |(Some Format822.I, vpkg, _) -> {acc with install = vpkg :: acc.install}
        |(Some Format822.R, vpkg, _) -> {acc with remove = vpkg :: acc.remove}
        |(None, vpkg, _) -> {acc with install = vpkg :: acc.install}
      ) request vpkgreqlist
  |Apt.Remove vpkgreqlist ->
      List.fold_left (fun acc -> function
        |(Some Format822.I, vpkg, _) -> {acc with install = vpkg :: acc.install}
        |(Some Format822.R, vpkg, _) -> {acc with remove = vpkg :: acc.remove}
        |(None, vpkg, _) -> {acc with remove = vpkg :: acc.remove}
      ) request vpkgreqlist
  |Apt.Upgrade _ -> {request with upgrade = true }
  |Apt.DistUpgrade _ -> {request with distupgrade = true}
;;

let parse_s = Packages.parse_s
let parse_string (_,s) = s
let parse_int_s (_,s) = string_of_int(int_of_string s)
let parse_req (loc,s) = 
  let aux = Packages.lexbuf_wrapper Packages_parser.vpkg_top in
  let l = Re_pcre.split ~rex:Apt.blank_regexp s in
  List.map (fun s -> aux (loc,s)) l

let parse_request_stanza par =
  {
    request = parse_s ~err:"(Empty REQUEST)" parse_string "Request" par;
    install = parse_s ~opt:[] parse_req "Install" par;
    remove = parse_s ~opt:[] parse_req "Remove" par;
    upgrade = parse_s ~opt:false Packages.parse_bool "Upgrade" par;
    distupgrade = parse_s ~opt:false Packages.parse_bool "Dist-Upgrade" par;
    autoremove = parse_s ~opt:false Packages.parse_bool "Autoremove" par;
    strict_pin = parse_s ~opt:true Packages.parse_bool "Strict-Pinning" par;
    preferences = parse_s ~opt:"" Packages.parse_string "Preferences" par;
  }
;;

(* parse and return a string -> for extra fields *)
let parse_bool_s = function
  |(_,("Yes"|"yes"|"true" |"True")) -> "true"
  |(_,("No" |"no" |"false"|"False")) -> "false" (* this one usually is not there *)
  |(_,s) -> raise (Format822.Type_error ("wrong value : "^ s))

let parse_installed = parse_s parse_bool_s "Installed"
let parse_hold = parse_s parse_bool_s "Hold"
let parse_apt_id = parse_s ~err:"(MISSING APT-ID)" parse_string "APT-ID"
let parse_apt_pin = parse_s ~err:"(MISSING APT-Pin)" parse_int_s "APT-Pin"
let parse_automatic = parse_s parse_bool_s "APT-Automatic"
let parse_candidate = parse_s parse_bool_s "APT-Candidate"
let parse_section = parse_s parse_string "Section"

(* (field,opt,err,multi,parsing function) *)
let extras = [
  ("Installed", Some parse_installed);
  ("Hold", Some parse_hold);
  ("APT-ID", Some parse_apt_id);
  ("APT-Pin", Some parse_apt_pin);
  ("APT-Candidate", Some parse_candidate);
  ("APT-Automatic", Some parse_automatic);
  ("Section", Some parse_section);
  ]

(* parse the entire file while filtering out unwanted stanzas *)
let rec packages_parser ?(request=false) archs (req,acc) p =
  let filter par = 
    let match_field f p =
      try 
        begin match Packages.assoc f p with
        |(_,("Yes"|"yes"|"True" |"true")) -> true
        |(_,("No" |"no" |"False"|"false")) -> false
        |_ -> false
        end
      with Not_found -> false
    in
    let inst () = match_field "installed" par in 
    let candidate () = match_field "apt-candidate" par in
    ((inst ()) || (candidate ()))
  in
  match Format822_parser.stanza_822 Format822_lexer.token_822 p.Format822.lexbuf with
  |None -> (req,acc) (* end of file *)
  |Some stanza when request = true -> 
      let req = parse_request_stanza stanza in
      packages_parser archs (req,acc) p
  |Some stanza when req.strict_pin = true -> begin
    match (Packages.parse_package_stanza (Some(filter)) archs extras stanza) with
    |None -> packages_parser archs (req,acc) p
    |Some st -> packages_parser archs (req,st::acc) p
  end
  |Some stanza when req.strict_pin = false -> begin
    match (Packages.parse_package_stanza None archs extras stanza) with
    |None -> assert false (* this is not possible in this branch *)
    |Some st -> packages_parser archs (req,st::acc) p
  end
  |_ -> assert false
;;

let input_raw_ch ?(archs=[]) ic =
  Format822.parse_from_ch (
    packages_parser ~request:true archs (default_request,[])
  ) ic
;;

let input_raw ?(archs=[]) file =
  let ch =
    match file with
    |"-" -> IO.input_channel stdin
    |_   -> Input.open_file file
  in
  let l = input_raw_ch ~archs ch in
  let _ = Input.close_ch ch in
  l
;;

let extras_tocudf =
  [
  ("Hold", ("hold", `Bool (Some false)));
  ("APT-Pin", ("apt-pin", `Int None));
  ("APT-ID", ("apt-id", `String None));
  ("APT-Candidate", ("apt-candidate", `Bool (Some false)));
  ("APT-Automatic", ("apt-automatic", `Bool (Some false)));
  ("Section", ("section", `String (Some ""))); 
  ]
;;

let is_installed pkg =
  try
    let _loc = Format822.dummy_loc in
    let v = Packages.assoc "installed" pkg.Packages.extras in
    Packages.parse_bool (_loc,v)
  with Not_found -> false

let is_on_hold pkg =
  try
    let _loc = Format822.dummy_loc in
    let v = Packages.assoc "hold" pkg.Packages.extras in
    (Packages.parse_bool (_loc,v))
  with Not_found -> false

let tocudf tables ?(options=Debcudf.default_options) ?(inst=false) pkg =
  let options = { options with Debcudf.extras_opt = extras_tocudf } in
  let pkg = 
    if is_installed pkg then
      let s = 
        if is_on_hold pkg then "hold ok installed" 
        else "install ok installed"
      in
      { pkg with Packages.extras = ("status",s)::pkg.Packages.extras }
    else pkg
  in
  Debcudf.tocudf tables ~options (* ~inst:(is_installed pkg) *) pkg
;;

(* Only one version of a package can be installed at a given time.
   Hence, when a remove request is issued without version constraint,
   we return (candidate.Cudf.package,None) that designates the only
   package installed.
 *)
let requesttocudf tables universe request =
  let to_cudf (p,v) = (p,Debcudf.get_cudf_version tables (p,v)) in
  let get_candidate (name,constr) =
    try
      List.find
        (fun pkg ->
          try (Cudf.lookup_package_property pkg "apt-candidate") = "true"
          with Not_found -> false)
        (CudfAdd.who_provides universe (name,constr))
    with Not_found ->
      fatal "Package %s does not have a suitable candidate" name
  in
  let select_packages ?(remove=false) l =
    List.map (fun ((n,a),c) ->
      let (name,constr) = Debutil.debvpkg to_cudf ((n,a),c) in
      if remove then (name,None)
      else begin
        match constr, request.strict_pin with
        |None, false -> (name, None)
        |_, _ -> (name,Some(`Eq,(get_candidate (name,constr)).Cudf.version))
        (* FIXME: when apt will accept version constraints different from `Eq,
           we will need to pass them through. *)
      end
    ) l
  in
  if request.upgrade || request.distupgrade then
    let to_upgrade = function
      |[] ->
        let filter pkg = pkg.Cudf.installed in
        let l = Cudf.get_packages ~filter universe in
        List.map (fun pkg -> (pkg.Cudf.package,None)) l
      |l -> select_packages l
    in
    {Cudf.default_request with
    Cudf.request_id = request.request;
    Cudf.upgrade = to_upgrade request.install;
    Cudf.remove = select_packages ~remove:true request.remove;
    }
  else
    {Cudf.default_request with
    Cudf.request_id = request.request;
    Cudf.install = select_packages request.install;
    Cudf.remove = select_packages ~remove:true request.remove;
    }
;;

