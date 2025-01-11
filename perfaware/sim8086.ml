exception InvalidInput of int
exception InvalidOpcode of int
exception InvalidFieldValue of int

type s_field =
  | NoSign (* =0 No sign extension *)
  | Sign (* =1 Sign extend 8-bit immediate data to 16 bits if W=1 *)

let map_s_field = function
  | 0b0 -> NoSign
  | 0b1 -> Sign
  | value -> raise (InvalidFieldValue value)

type w_field =
  | Byte (* =0 Instruction operates on byte data *)
  | Word (* =1 Instruction operates on word data *)

let map_w_field byte =
  match byte land 0b1 with
    | 0b0 -> Byte (* 8bit address *)
    | 0b1 -> Word (* 16bit address *)
    | value -> raise (InvalidFieldValue value)

type d_field =
  | FromRegField (* =0 Instruction source is specified in REG field *)
  | ToRegField (* =1 Instruction destination is specified in REG field *)

let map_d_field byte =
  match (byte lsr 1) land 0b1 with
    | 0b0 -> FromRegField
    | 0b1 -> ToRegField
    | value -> raise (InvalidFieldValue value)

type v_field =
  | One (* =0 Shift/rotate count is one *)
  | InCLRegister (* =1 Shift/rotate count is specified in CL register *)

let map_v_field = function
  | 0b0 -> One
  | 0b1 -> InCLRegister
  | value -> raise (InvalidFieldValue value)

type z_field =
  | FlagClear (* =0 Repeat/loop while zero flag is clear *)
  | FlagSet (* =1 Repeat/loop while zero flag is set *)

let map_z_field = function
  | 0b0 -> FlagClear
  | 0b1 -> FlagSet
  | value -> raise (InvalidFieldValue value)

type mod_field = Register (* Register Mode (no displacement) *)

let map_mod_field = function
  | 0b11 -> Register
  | value -> raise (InvalidFieldValue value)

type reg_field = AL | BL | CL | DL | AH | BH | CH | DH | AX | BX | CX | DX | SP | BP | SI | DI

let map_reg_field = function
  | 0b000, Byte -> AL
  | 0b011, Byte -> BL
  | 0b001, Byte -> CL
  | 0b010, Byte -> DL
  | 0b100, Byte -> AH
  | 0b111, Byte -> BH
  | 0b101, Byte -> CH
  | 0b110, Byte -> DH
  | 0b000, Word -> AX
  | 0b001, Word -> CX
  | 0b010, Word -> DX
  | 0b011, Word -> BX
  | 0b100, Word -> SP
  | 0b101, Word -> BP
  | 0b110, Word -> SI
  | 0b111, Word -> DI
  | value, _ -> raise (InvalidFieldValue value)

let repr_reg_field = function
  | AL -> "al"
  | BL -> "bx"
  | CL -> "cl"
  | DL -> "dl"
  | AH -> "ah"
  | BH -> "bh"
  | CH -> "ch"
  | DH -> "dh"
  | AX -> "ax"
  | BX -> "bx"
  | CX -> "cx"
  | DX -> "dx"
  | SP -> "sp"
  | BP -> "bp"
  | SI -> "si"
  | DI -> "di"

type opcode = Mov

let map_opcode byte =
  match byte land 0b11111100 with
    | 0b10001000 -> Mov
    | value -> raise (InvalidOpcode value)

type instruction = Instruction of opcode * d_field * w_field

let map_instruction = function
  | byte -> Instruction (map_opcode byte, map_d_field byte, map_w_field byte)

let repr_instruction = function
  | Instruction (Mov, _, _) -> "mov"

let read_bytes_from_file filename =
  let ic = open_in_bin filename in
  let file_size = in_channel_length ic in
  let buffer = Bytes.create file_size in
    really_input ic buffer 0 file_size;
    close_in ic;
    buffer

let match_opcode = function
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

let foo byte iteration =
  match iteration mod 2 = 0 with
    | true ->
      let instruction = map_instruction byte in
        repr_instruction instruction
    | false -> match_opcode byte

let print_bytes_in_binary bits bytes =
  let len = Bytes.length bytes in
    for i = 0 to len - 1 do
      let int = Bytes.get_uint8 bytes i in
      let opcode = foo int i in
        Printf.printf "%s " opcode;
        if (i + 1) mod (bits / 8) = 0 || i = len - 1 then Printf.printf "\n"
    done

let () =
  let filename = "./part1/listing_0038_many_register_mov" in
  let bytes = read_bytes_from_file filename in
    print_bytes_in_binary 16 bytes
