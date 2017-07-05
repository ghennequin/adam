open Lacaml.D

(** minimize a differentiable function using ADAM *)
val min: eta:float -> epsilon:float -> beta1:float -> beta2:float 
  -> stop:(int -> float -> bool) 
  -> (vec -> vec -> float) -> vec -> float


