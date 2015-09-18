(* [cipher refl rotors notches starts s] computes the Enigma cipher, where
 *  - [refl] is the wiring of the reflector,
 *  - [rotors] is a list of the rotors (which must contain at least
 *      one element), as they are installed from left to right on 
 *      the spindle,
 *  - [notches] is a list of where the notch is on each rotor,
 *      where the nth character of [notches] is the location of
 *      the notch on the nth rotor in [rotors],
 *  - [starts] is a list of the starting character for each rotor
 *      as positioned on the spindle, where the nth character of
 *      [starts] is the starting character for the nth rotor in
 *      [rotors].
 *  - [s] is the string to be ciphered.
 *)
let cipher (refl:string) (rotors:string list) (notches:char list)
           (starts:char list) (s:string) : string
=
  "Unimplemented"

(* [simulate] takes the same inputs as [cipher] but prints
 * a simulation of the Enigma machine at each step of the
 * computation.
 *)
let simulate (refl:string) (rotors:string list) (notches:char list)
             (starts:char list) (s:string) : unit
=
  print_string "Unimplemented\n"
