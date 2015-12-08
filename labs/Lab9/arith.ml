module type Ring = sig
  type t
  val zero : t
  val one  : t
  val add  : t -> t -> t
  val mult : t -> t -> t
end

module type Field = sig
  include Ring
  val neg : t -> t
  val div : t -> t -> t
end

module FloatRing = struct
  type t = float
  let zero = 0.
  let one = 1.
  let add = (+.)
  let mult = ( *. )
end

module FloatField = struct
  include FloatRing
  let neg = (~-.)
  let div = (/.)
end

module FR : Ring = FloatRing 
module FF : Field = FloatField
  

  
