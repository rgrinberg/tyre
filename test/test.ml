[@@@ocaml.warning "-44"]

module A = struct
  include Alcotest
  let choice
      (type a) (type b)
      (module M1 : TESTABLE with type t = a)
      (module M2 : TESTABLE with type t = b)
    : (module TESTABLE with type t = [`Left of M1.t | `Right of M2.t])
    = (module struct
    type t = [`Left of M1.t | `Right of M2.t]
    let pp ppf = function
      | `Left x -> M1.pp ppf x
      | `Right x -> M2.pp ppf x
    let equal x y = match x, y with
      | `Left x, `Left y -> M1.equal x y
      | `Right x, `Right y -> M2.equal x y
      | _ -> false
  end)
end

open Tyre

let t' title desc re v s =
  let cre = Tyre.(compile (start *> re <* stop)) in
  A.(check (result desc reject))
    (title^" exec") (Tyre.exec cre s) (Result.Ok v) ;
  A.(check bool) (title^" execp") (Tyre.execp cre s) true ;
  A.(check string) (title^" eval") s (Tyre.eval re v)

let t title desc re v s =
  title, `Quick, fun () -> t' title desc re v s

let topt title desc re v s s' =
  title, `Quick,
  fun () ->
    t' (title ^" some") (A.option desc) re (Some v) s ;
    t' (title ^" none") (A.option desc) re None s'

let basics = [
  t "int" A.int int 42 "42" ;
  t "int pos" A.int pos_int 549085 "549085" ;
  t "int neg" A.int int (-54) "-54" ;

  t "float int" A.float float 3. "3." ;
  (* t "float_int2" A.float float 3. "3" ; *)
  t "float" A.float float 4.2 "4.2" ;
  t "float neg" A.float float (-4.2) "-4.2" ;

  t "bool true" A.bool bool true "true" ;
  t "bool false" A.bool bool false "false" ;

  topt "int option" A.int (opt int) 3 "3" "" ;
  t "int seq" A.(pair int bool) (int <&> bool) (3,true) "3true" ;
]

let prefix_suffix = [
  t "prefix" A.int (bool *> int) 3 "true3" ;
  t "prefix" A.int (int *> bool *> int) (-2) "0true-2" ;

  t "prefixstr" A.int (str "foo" *> int) 3 "foo3" ;
  t "suffixstr" A.int (int <* str "foo") 3 "3foo" ;

  t "prefix seq" A.(pair int int) (int <&> str "foo" *> int) (3,4) "3foo4" ;
  t "prefix seq" A.(pair int bool) (int <&> bool <* str "foo") (3,true) "3truefoo" ;
  t "suffix seq" A.(pair int int) (int <* str "foo" <&> int) (3,4) "3foo4" ;
  t "suffix seq" A.(pair bool int) (str "foo" *> bool <&> int) (true,4) "footrue4" ;
]

let composed = [
  topt "option prefix" A.int (opt int <* str "foo") 3 "3foo" "foo" ;
  t "terminated list" A.(list int) (terminated_list ~sep:(char ';') int)
    [1;254;3;54;] "1;254;3;54;" ;
  t "separated list" A.(list int) (separated_list ~sep:(char ';') int)
    [1;254;3;54] "1;254;3;54" ;
  t "alt list" A.(list @@ choice string string) (list (regex Re.digit <|> regex Re.alpha))
    [`Left "1";`Right "a"; `Left "2"; `Left "5"; `Right "c"] "1a25c" ;
  t "list of list"
    A.(list @@ list @@ choice int string)
    (list @@ str"@" *> list (pos_int <|> regex Re.alpha))
    [[`Left 1;`Right "a"]; [`Right "c"] ; [`Right "d";`Left 33]] "@1a@c@d33"
]

let routes =
  let fixed n = regex Re.(repn any n (Some n)) in
  let f n x = n, x in
  route [
    (str"foo" *> fixed 3 <* str"xx") --> f 1 ;
    (str"foo" *> fixed 5) --> f 2 ;
    (str"bar" *> fixed 5) --> f 3 ;
    (fixed 2 <* str"blob") --> f 4 ;
  ]

let troute title s n res =
  title, `Quick,
  fun () ->
    A.(check @@ result (pair int string) reject)
      title (exec routes s) (Result.Ok (n,res))

let route_test = [
  troute "route 1" "foo123xx" 1 "123" ;
  troute "route 2" "foo12345" 2 "12345" ;
  troute "route 3" "bar12345" 3 "12345" ;
  troute "route 4" "xxblob" 4 "xx" ;
]


let () = Alcotest.run "tyre" [
    "basics", basics ;
    "prefix suffix", prefix_suffix ;
    "composed", composed ;
    "routes", route_test ;
  ]
