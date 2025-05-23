(***********************************************************************)
(*                                                                     *)
(*  Based on the music example from the book                           *)
(*  ``Apprendre Ã  programmer avec OCaml'' by                           *)
(*                                                                     *)
(*  Sylvain Conchon and Jean-Christophe FilliÃ¢tre                      *)
(*  UniversitÃ© Paris Sud                                               *)
(*                                                                     *)
(*  Copyright 2014 UniversitÃ© Paris Sud.  All rights reserved. This    *)
(*  file is distributed under the terms of the GNU Library General     *)
(*  Public License, with the same special exception on linking as the  *)
(*  OCaml library. See http://caml.inria.fr/ocaml/license.fr.html      *)
(*                                                                     *)
(***********************************************************************)

(* -------------------------------------------------------------------------- *)
(* ----------------------- DEFINITIONS OF MUSIC SCORES ---------------------- *)
(* -------------------------------------------------------------------------- *)

type note = C | D | E | F | G | A | H
type pitch = { note : note; octave : int }
type duration = Full | Half | Quarter | Eighth | Sixteenth
type symbol = Note of duration * pitch | Rest of duration
type score = { symbols : symbol list; tempo : int }

let o1_frequency t =
  match t with
  | C -> 32.70
  | D -> 36.71
  | E -> 41.20
  | F -> 43.65
  | G -> 49.
  | A -> 55.
  | H -> 61.74

let frequency p =
  let f0 = o1_frequency p.note in
  f0 *. (2. ** float p.octave)

let duration d t =
  let t = 60. /. float_of_int t in
  match d with
  | Sixteenth -> t /. 4.
  | Eighth -> t /. 2.
  | Quarter -> t +. 0.0001
  | Half -> t *. 2.
  | Full -> t *. 4.

let df_of_symbol s tp =
  match s with
  | Rest d -> (duration d tp, 0.)
  | Note (d, f) -> (duration d tp, frequency f)

(* -------------------------------------------------------------------------- *)
(* ------------------------------ PLAYING SOUNDS  --------------------------- *)
(* -------------------------------------------------------------------------- *)

let play_sound s tp =
  let d, f = df_of_symbol s tp in
  let pid =
    Unix.create_process "/opt/homebrew/bin/play"
      [|
        "play";
        "-r";
        "44100";
        "-n";
        "synth";
        string_of_float d;
        "sin";
        string_of_float f;
      |]
      Unix.stdin Unix.stdout Unix.stderr
  in
  let _ = Unix.waitpid [] pid in
  ()

let play_score sc = List.iter (fun s -> play_sound s sc.tempo) sc.symbols

(* let () = Printf.printf "%.2f\n" (o1_frequency G) *)
let _ = { note = A; octave = 3 }
let r_4 = Rest Quarter
let r_8 = Rest Eighth
let r_16 = Rest Sixteenth
let g_4_8 = Note (Eighth, { note = G; octave = 4 })
let f_4_8 = Note (Eighth, { note = F; octave = 4 })
let e_4_8 = Note (Eighth, { note = E; octave = 4 })
let d_4_8 = Note (Eighth, { note = D; octave = 4 })
let c_4_8 = Note (Eighth, { note = C; octave = 4 })

(*
let _ = play_sound r_4 292
let _ = play_sound g_4_8 292
let _ = play_sound g_4_8 292
let _ = play_sound g_4_8 292
let _ = play_sound g_4_8 292
let _ = play_sound g_4_8 292
let _ = play_sound g_4_8 292
let _ = play_sound g_4_8 292
let _ = play_sound g_4_8 292
let _ = play_sound f_4_8 292
let _ = play_sound e_4_8 292
let _ = play_sound d_4_8 292
let _ = play_sound r_4 292
let _ = play_sound r_16 292
let _ = play_sound r_8 292
*)

let l1 = [ r_4; r_16; r_8 ]
let l2 = [ f_4_8; e_4_8; d_4_8 ]
let l3 = List.init 7 (fun _i -> g_4_8)
let sc1 = (r_4 :: l3) @ l2 @ l1

(* let _ = play_score { symbols = sc1; tempo = 292 } ;; *)

let sc2 =
  sc1 @ [ r_8; d_4_8 ] @ l3 @ [ f_4_8; e_4_8; d_4_8 ]
  @ [ r_8; c_4_8; d_4_8; d_4_8 ]

(* let _ = play_score { symbols = sc2; tempo = 292 } ;; *)

(* -------------------------------------------------------------------------- *)
(* -------------------- GENERATING WAW FILES FROM SCORES -------------------- *)
(* -------------------------------------------------------------------------- *)

let pi = 4.0 *. atan 1.0

(* Generate a sine wave with a given frequency and duration *)
let generate_wave frequency duration =
  let num_samples = int_of_float (duration *. 44100.) in
  let silence_samples = Array.make 40 0 in
  let wave_samples =
    Array.init num_samples (fun i ->
        let t = float i /. 44100. in
        int_of_float (32767.0 *. sin (2.0 *. pi *. frequency *. t)))
  in
  Array.append wave_samples silence_samples

let wave_of_symbol s t =
  let d, f = df_of_symbol s t in
  generate_wave f d

let flatten_array_list (arr_list : int array list) : int array =
  let total_length =
    List.fold_left (fun acc arr -> acc + Array.length arr) 0 arr_list
  in
  let result = Array.make total_length 0 in
  let _ =
    List.fold_left
      (fun index arr ->
        Array.blit arr 0 result index (Array.length arr);
        index + Array.length arr)
      0 arr_list
  in
  result

let write_wav filename score =
  let oc = open_out_bin filename in
  let data = List.map (fun s -> wave_of_symbol s score.tempo) score.symbols in
  let data = flatten_array_list data in
  let num_samples = Array.length data in
  let write_int32_le oc n =
    output_byte oc (n land 0xFF);
    output_byte oc ((n lsr 8) land 0xFF);
    output_byte oc ((n lsr 16) land 0xFF);
    output_byte oc ((n lsr 24) land 0xFF)
  in
  let write_int16_le oc n =
    output_byte oc (n land 0xFF);
    output_byte oc ((n lsr 8) land 0xFF)
  in
  (* WAV header *)
  output_string oc "RIFF";
  write_int32_le oc (36 + (num_samples * 2));
  output_string oc "WAVEfmt ";
  write_int32_le oc 16;
  write_int16_le oc 1;
  write_int16_le oc 1;
  write_int32_le oc 44100;
  write_int32_le oc (44100 * 2);
  write_int16_le oc 2;
  write_int16_le oc 16;
  output_string oc "data";
  write_int32_le oc (num_samples * 2);
  Array.iter (fun sample -> write_int16_le oc sample) data;
  close_out oc

let () =
  let filename = "output.wav" in
  write_wav filename { symbols = sc2; tempo = 146 }
