let read_bytes_from_file filename =
  let ic = open_in_bin filename in
  let file_size = in_channel_length ic in
  let buffer = Bytes.create file_size in
  really_input ic buffer 0 file_size;
  close_in ic;
  buffer
;;

let match_opcode byte =
  match byte with
  | 136 | 137 -> "mov"
  | 217 -> "cx, bx"
  | 229 -> "ch, ah"
  | 218 -> "dx, bx"
  | 222 -> "si, bx"
  | 251 -> "bx, di"
  | 200 -> "al, cl"
  | 237 -> "ch, ch"
  | 195 -> "bx, ax"
  | 243 -> "bx, si"
  | 252 -> "sp, di"
  | 197 -> "bp, ax"
  | byte -> Printf.sprintf "%i" byte

let print_bytes_in_binary bytes =
  let byte_to_binary byte =
    let rec to_binary n acc =
      if n = 0 && String.length acc = 8 then acc
      else to_binary (n lsr 1) ((string_of_int (n land 1)) ^ acc)
    in
    to_binary (Char.code byte) ""
  in
  let len = Bytes.length bytes in
  for i = 0 to len - 1 do
    let int = Bytes.get_uint8 bytes i in
    let opcode = match_opcode int in
    let binary = byte_to_binary (Bytes.get bytes i) in
    Printf.printf "%s %s " binary opcode;
    if (i + 1) mod 2 = 0 || i = len - 1 then Printf.printf "\n"
  done
;;

let () =
  let filename = "./part1/listing_0038_many_register_mov" in
  let bytes = read_bytes_from_file filename in
  print_bytes_in_binary bytes;
