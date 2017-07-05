open Lacaml.D

(** minimize a differentiable function using ADAM
    @param eta learning rate (default=0.002)
    @param epsilon (default=10E-8)
    @param beta1 (default=0.9)
    @param beta2 (default=0.999) *)
val min: ?eta:float -> ?epsilon:float -> ?beta1:float -> ?beta2:float 
  -> ?lb:vec -> ?ub:vec -> ?clip:float
  -> stop:(int -> float -> bool) 
  -> (vec -> vec -> float) -> vec -> float


