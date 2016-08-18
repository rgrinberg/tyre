
let oc = open_in "benchmark/data/http-requests.txt.100"
let s = CCIO.read_all oc

let re = Tyre.compile (Tyre.list RFC2616.request)
let tyre s =
  match Tyre.exec re s with
  | Result.Ok _ -> ()
  | Result.Error _ -> failwith "oups"

let angstrom s =
  match Angstrom.(parse_only (many Angstrom_rFC2616.request)) (`String s) with
  | Result.Ok _ -> ()
  | Result.Error _ -> failwith "oups"

let l = [
  "tyre", tyre, s ;
  "angstrom", angstrom, s ;
]

let () =
  let samples = Benchmark.latencyN 100L l in
  Benchmark.tabulate samples
