[@@@ocaml.warning "-44-48"]

let oc = open_in "benchmark/data/http-requests.txt.100"
let s = CCIO.read_all oc

let re = Tyre.compile (Tyre.list RFC2616.request)
let tyre s =
  match Tyre.exec re s with
  | Result.Ok l -> assert (List.length l = 55 * 100)
  | Result.Error _ -> failwith "oups"

let re2 = Tyre.compile ~whole:false RFC2616.request
let tyre_all s =
  match Tyre.all re2 s with
  | Result.Ok l -> assert (List.length l = 55 * 100)
  | Result.Error _ -> failwith "oups"

let angstrom s =
  match Angstrom.(parse_only (many Angstrom_rFC2616.request)) (`String s) with
  | Result.Ok l -> assert (List.length l = 55 * 100)
  | Result.Error _ -> failwith "oups"

let () =
  let open Benchmark in
  let n = 100L in
  let bench_tyre = lazy (Benchmark.latency1 n tyre s) in
  let bench_tyre_all = lazy (Benchmark.latency1 n tyre_all s) in
  let bench_angstrom = lazy (Benchmark.latency1 n angstrom s) in
  let tree = Tree.("http" @>>> [
      "tyre" @> bench_tyre ;
      "tyre.all" @> bench_tyre_all ;
      "angstrom" @> bench_angstrom ;
    ])
  in
  Tree.register tree ;
  Tree.run_global ()
