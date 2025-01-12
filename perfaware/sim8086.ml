exception InvalidInput of int
exception InvalidOpcode of int
exception InvalidFieldValue of int

type s_field = NoSign | Sign

let map_s_field = function
  | 0b0 -> NoSign
  | 0b1 -> Sign
  | value -> raise (InvalidFieldValue value)

type data_size_field = ByteData | WordData

let parse_data_size_field byte =
  match byte land 0b1 with
    | 0b0 -> ByteData
    | 0b1 -> WordData
    | value -> raise (InvalidFieldValue value)

type direction_field = SourceFromRegister | DestinationToRegister

let map_direction_field byte =
  match (byte lsr 1) land 0b1 with
    | 0b0 -> SourceFromRegister
    | 0b1 -> DestinationToRegister
    | value -> raise (InvalidFieldValue value)

type v_field = One | InCLRegister

let map_v_field = function
  | 0b0 -> One
  | 0b1 -> InCLRegister
  | value -> raise (InvalidFieldValue value)

type z_field = FlagClear | FlagSet

let map_z_field = function
  | 0b0 -> FlagClear
  | 0b1 -> FlagSet
  | value -> raise (InvalidFieldValue value)

type mod_field = Register (* Register Mode (no displacement) *)

let map_mod_field = function
  | 0b11 -> Register
  | value -> raise (InvalidFieldValue value)

type reg_field = AL | BL | CL | DL | AH | BH | CH | DH | AX | BX | CX | DX | SP | BP | SI | DI
type rm_field = reg_field

let map_reg_field = function
  | 0b000, ByteData -> AL
  | 0b011, ByteData -> BL
  | 0b001, ByteData -> CL
  | 0b010, ByteData -> DL
  | 0b100, ByteData -> AH
  | 0b111, ByteData -> BH
  | 0b101, ByteData -> CH
  | 0b110, ByteData -> DH
  | 0b000, WordData -> AX
  | 0b001, WordData -> CX
  | 0b010, WordData -> DX
  | 0b011, WordData -> BX
  | 0b100, WordData -> SP
  | 0b101, WordData -> BP
  | 0b110, WordData -> SI
  | 0b111, WordData -> DI
  | value, _ -> raise (InvalidFieldValue value)

let reg_field_to_string = function
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

type instruction =
  | MovRegisterMemoryToFromRegister of
      direction_field * data_size_field * mod_field * reg_field * rm_field
  | MovImmediateToRegisterMemory of data_size_field * mod_field * rm_field
  | MovImmediateToRegister of data_size_field * reg_field

let parse_instruction bytes idx =
  let first_byte = Bytes.get_uint8 bytes idx in
    match first_byte land 0b11110000 with
      | 0b10000000 -> (
        match first_byte land 0b11111100 with
          | 0b10001000 ->
            let second_byte = Bytes.get_uint8 bytes (idx + 1) in
            let direction = map_direction_field first_byte in
            let data_size = parse_data_size_field first_byte in
            let mod_field = map_mod_field (second_byte lsr 6) in
            let reg_field = map_reg_field ((second_byte lsr 3) land 0b111, data_size) in
            let rm_field = map_reg_field (second_byte land 0b111, data_size) in
              ( MovRegisterMemoryToFromRegister
                  (direction, data_size, mod_field, reg_field, rm_field),
                2 )
          | _ -> raise (InvalidOpcode first_byte))
      | 0b11000000 -> (
        match first_byte land 0b11111110 with
          | 0b11000110 ->
            let second_byte = Bytes.get_uint8 bytes (idx + 1) in
            let data_size = parse_data_size_field first_byte in
            let mod_field = map_mod_field (second_byte lsr 6) in
            let rm_field = map_reg_field (second_byte land 0b111, data_size) in
              (MovImmediateToRegisterMemory (data_size, mod_field, rm_field), 1)
          | _ -> raise (InvalidOpcode first_byte))
      | _ -> raise (InvalidOpcode first_byte)

let instruction_to_string = function
  | MovRegisterMemoryToFromRegister (_, _, Register, reg_field, rm_field) ->
    Printf.sprintf "mov %s, %s" (reg_field_to_string reg_field) (reg_field_to_string rm_field)
  | MovImmediateToRegisterMemory _ -> "mov"
  | MovImmediateToRegister _ -> "mov"

let read_bytes_from_file filename =
  let ic = open_in_bin filename in
  let file_size = in_channel_length ic in
  let buffer = Bytes.create file_size in
    really_input ic buffer 0 file_size;
    close_in ic;
    buffer

let print_bytes_in_binary bits bytes =
  let len = Bytes.length bytes in
  let i = ref 0 in
    while !i < len do
      let instruction, forward_steps = parse_instruction bytes !i in
        i := !i + forward_steps;
        Printf.printf "%s \n" (instruction_to_string instruction)
    done

let () =
  let filename = "./part1/listing_0038_many_register_mov" in
  let bytes = read_bytes_from_file filename in
    print_bytes_in_binary 16 bytes
