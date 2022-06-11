type tree = Leaf of leaf | Node of int * tree * tree and leaf = {letter: char; weight: int}

type buffer = {mutable value: int; mutable nbits: int}

let eof = '\000'

let weight = function
  | Leaf f -> f.weight
  | Node (w,_,_) -> w


let rec insert tr l = match l with
  | [] -> [tr]
  | h::t when weight tr <= weight h -> tr :: l
  | h::t -> h :: insert tr t

let alphabet path = 
  let t = Array.make 256 0 in
  let f = open_in path in
  let l = ref [] in
  begin try
    while true do
      let c = input_char f in
      let k = int_of_char c in
      t.(k) <- t.(k) + 1
    done
  with
    | End_of_file -> In_channel.close f;
      t.(0) <- 1;
      for i = 0 to 255 do
        if t.(i) <> 0 then
          let a = Leaf {letter= char_of_int i; weight= t.(i)} in
          l := insert a !l
      done
  end;
  !l

let rec huffman_tree l = match l with
  | [] -> failwith "empty list"
  | [x] -> x
  | t1::t2::t -> let tm = Node (weight t1 + weight t2, t1, t2) in
    huffman_tree (insert tm t)

let codes tc =
  let rec compute code t = match t with
    | Leaf f -> [(f.letter, code)]
    | Node (_,a,b) -> (compute (code ^ "0") a) @ (compute (code ^ "1") b)
  in compute "" tc

let codes_map tc = 
  let m = Array.make 256 "" in
  let l = codes tc in
  let rec aux = function
    | [] -> ()
    | (a,c)::t -> m.(int_of_char a) <- c; aux t
  in aux l;
  m

let encoding input output =
  let tc = alphabet input |> huffman_tree in
  let t = codes_map tc in
  let buff = {value= 0; nbits= 0} in
  let f_in = open_in input and f_out = open_out output in
  output_value f_out tc;
  let p = ref 1 and stop = ref false in
  while not !stop do
    let k = match In_channel.input_char f_in with 
      | None -> stop := true; int_of_char eof
      | Some c -> int_of_char c 
    in
    let n = String.length t.(k) in
    for i = 0 to n-1 do
      if buff.nbits = 8 then begin
        Out_channel.output_char f_out (char_of_int buff.value);
        buff.value <- 0; buff.nbits <- 0;
        p := 1
      end;
      let b = if t.(k).[i] = '0' then 0 else 1 in 
      buff.value <- buff.value + (b * !p); 
      buff.nbits <- buff.nbits + 1;
      p := 2 * !p 
    done
  done;
  if buff.nbits <> 0 then Out_channel.output_char f_out (char_of_int buff.value);
  In_channel.close f_in; Out_channel.close f_out

let bits_of_char c =
  let k = ref (int_of_char c) in
  let b = ref [] in
  for i = 1 to 8 do
    b := (!k mod 2) :: !b;
    k := !k / 2
  done;
  List.rev !b

let decoding input output =
  let buff = Buffer.create 256 and
  f_in = open_in input in
  let tc = (input_value f_in: tree) in
  let rec aux t bits = match t with
    | Leaf f -> let c = f.letter in
      if c <> eof then begin
        Buffer.add_char buff c;
        aux tc bits
      end else ()
    | Node (_,a,b) -> begin
        match bits with
        | [] -> begin
          match In_channel.input_char f_in with
          | None -> ()
          | Some c -> aux t (bits_of_char c)
          end 
        | 0 :: tl -> aux a tl
        | _ :: tl -> aux b tl
        end
  in aux tc []; In_channel.close f_in;
  let f_out = open_out output in
  Out_channel.output_bytes f_out (Buffer.to_bytes buff);
  Out_channel.close f_out

