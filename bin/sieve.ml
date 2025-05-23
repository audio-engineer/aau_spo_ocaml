let max = read_int ()
let prime = Array.make (max + 1) true

let () =
  (* Assign `false` to the first two elements in the array *)
  prime.(0) <- false;
  prime.(1) <- false;

  let limit = truncate (sqrt (float max)) in

  for n = 2 to limit do
    if prime.(n) then
      let m = ref (n * n) in

      while !m <= max do
        prime.(!m) <- false;
        m := !m + n
      done
  done

let () =
  for n = 2 to max do
    if prime.(n) then Printf.printf "%d\n" n
  done
