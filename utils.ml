(* Code to smaple from normal distribution is based on Box-Muller method: *)
(* https://en.wikipedia.org/wiki/Box–Muller_transform *)
let random_normal () =
  sqrt (-2. *. log (Random.float 1.)) *. cos (2. *. Float.pi *. Random.float 1.)

let argmax nums =
  let rec aux idx idx_max =
    match idx with
    | -1 -> if idx_max >= 0 then Some idx_max else None
    | _ -> aux (idx - 1) (if nums.(idx) >= nums.(idx_max) then idx else idx_max)
  in
  let idx = Array.length nums - 1 in
  aux idx idx
