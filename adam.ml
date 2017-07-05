open Lacaml.D


let min ?(eta=0.002) ?(epsilon=10E-8) ?(beta1=0.9) ?(beta2=0.999) ~stop f_df x =
  let n = Vec.dim x in
  let g = Vec.make0 n in
  let g2 = Vec.make0 n in
  let m = Vec.make0 n in
  let v = Vec.make0 n in
  let mhat = Vec.make0 n in
  let vhat = Vec.make0 n in
  let tmp1 = Vec.make0 n in
  let tmp2 = Vec.make0 n in

  let rec iterate t cost =
    Vec.mul ~z:g2 g g |> ignore;
    (* update m *)
    scal beta1 m;
    axpy ~alpha:(1. -. beta1) g m;
    (* update v *)
    scal beta2 v;
    axpy ~alpha:(1. -. beta2) g2 v;
    (* bias correction factors *)
    Vec.fill mhat 0.;
    axpy ~alpha:(1. /. (1. -. (beta1 ** float t))) m mhat;
    Vec.fill vhat 0.;
    axpy ~alpha:(1. /. (1. -. (beta2 ** float t))) v vhat;
    (* compute update *)
    Vec.sqrt ~y:tmp1 vhat |> ignore;
    Vec.add_const epsilon ~y:tmp2 tmp1 |> ignore;
    Vec.div ~z:tmp1 mhat tmp2 |> ignore;
    axpy ~alpha:(-. eta) tmp1 x;
    if not (stop t cost) then (
      let cost = f_df x g in
      iterate (t+1) cost) else cost in

  iterate 1 (f_df x g)


