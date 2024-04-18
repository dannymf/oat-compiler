open Oat
open Ll
open Datastructures

let do_live =     ref false
let do_cp =       ref false
let do_alias =    ref false

let do_escape =   ref false 

let print_live tdecls (cfg:Cfg.t) : string =
    Liveness.Graph.to_string tdecls @@ Liveness.analyze cfg

let print_cp tdecls (cfg:Cfg.t) : string =
    Constprop.Graph.to_string tdecls @@ Constprop.analyze cfg

let print_alias tdecls (cfg:Cfg.t) : string =
    Alias.Graph.to_string tdecls @@ Alias.analyze cfg

let print_escape tdecls (cfg:Cfg.t) : string =
  Escape.Graph.to_string tdecls @@ Escape.analyze cfg

let files = ref []

let args = let open Arg in
           [ "-live",  Set do_live,  "print liveness"
           ; "-cp",    Set do_cp,    "print constant prop"
           ; "-alias", Set do_alias, "print alias"
           ; "-escape", Set do_escape, "print escape"
           ]



let do_file fname print_fn =
  let ll_prog = Driver.parse_ll_file fname in
  ll_prog.fdecls
  |> List.iter @@ fun (g,f) ->
                  let string_of_arg (t,u) = Printf.sprintf "%s %%%s" (Llutil.sot t) u in
                  let ts, t = f.f_ty in
                  Printf.printf "define %s @%s(%s) {\n%s\n}\n" 
                                (Llutil.sot t) g 
                                (String.concat ", " @@ List.map string_of_arg List.(combine ts f.f_param))
                                (print_fn ll_prog.tdecls (Cfg.of_ast f))

let opt_file opt fname =
  let opt_fdecl (gid,fdecl) = 
    let og = opt (Cfg.of_ast fdecl) in
    gid, Cfg.to_ast og
  in

  let p = Driver.parse_ll_file fname in

  let op = { p with fdecls = List.map opt_fdecl p.fdecls } in

  print_endline @@ Llutil.string_of_prog op

  

let () = 
  if not !Sys.interactive then begin
      Arg.parse args (fun f -> files := f::!files) "Usage";
      (if !do_live  then List.iter (fun f -> do_file f print_live)  !files);
      (if !do_cp    then List.iter (fun f -> do_file f print_cp)    !files);
      (if !do_alias then List.iter (fun f -> do_file f print_alias) !files);      
      (if !do_escape then List.iter (fun f -> do_file f print_escape) !files);  
    end
  


    

