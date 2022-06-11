open Huffman

let _ =
  let argc = Array.length Sys.argv in
  if argc = 3 then
    Huffman.encoding Sys.argv.(1) Sys.argv.(2)
  else if argc = 4 && Sys.argv.(1) = "-d" then
    Huffman.decoding Sys.argv.(2) Sys.argv.(3)
  else
    let name = Sys.argv.(0) in
    print_string ("Usage:\n\t" ^ name ^ " input output : compress 'input'\n\t" ^ name ^ " -d input output : decompress 'input'\n")
