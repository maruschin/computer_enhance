exception InvalidInput of int
exception InvalidOpcode of int
exception InvalidFieldValue of int

type s_field = NoSign | Sign

let map_s_field = function
  | 0b0 -> NoSign
  | 0b1 -> Sign
  | value -> raise (InvalidFieldValue value)

type data_size = ByteData | WordData

let parse_data_size byte =
  match byte land 0b1 with
    | 0b0 -> ByteData
    | 0b1 -> WordData
    | value -> raise (InvalidFieldValue value)

type direction = SourceFromRegister | DestinationToRegister

let parse_direction byte =
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

type mode =
  | RegisterMode
  | MemoryMode
  | MemoryModeLowDisplacement of int
  | MemoryModeHighDisplacement of int

let parse_mode value bytes idx =
  match value with
    | 0b00 -> MemoryMode
    | 0b01 -> MemoryModeLowDisplacement (Bytes.get_uint8 bytes (idx + 2))
    | 0b10 -> MemoryModeHighDisplacement (Bytes.get_uint16_ne bytes (idx + 2))
    | 0b11 -> RegisterMode
    | value -> raise (InvalidFieldValue value)

type register = AL | BL | CL | DL | AH | BH | CH | DH | AX | BX | CX | DX | SP | BP | SI | DI

let parse_register = function
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

let register_to_string = function
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

type register_memory =
  | Register of register
  | Memory of register
  | MemorySum of register * register
  | DirectAddress of int

let map_register_map_to_address_calculation = function
  | 0b000 -> MemorySum (BX, SI)
  | 0b001 -> MemorySum (BX, DI)
  | 0b010 -> MemorySum (BP, SI)
  | 0b011 -> MemorySum (BP, DI)
  | 0b100 -> Memory SI
  | 0b101 -> Memory DI
  | 0b110 -> Memory BP
  | 0b111 -> Memory BX
  | value -> raise (InvalidFieldValue value)

let parse_register_memory mode register_byte data_size =
  match mode with
    | RegisterMode -> Register (parse_register (register_byte, data_size))
    | MemoryMode -> map_register_map_to_address_calculation register_byte
    | MemoryModeLowDisplacement _ | MemoryModeHighDisplacement _ ->
      map_register_map_to_address_calculation register_byte

let register_memory_to_string mode displacement =
  match (mode, displacement) with
    | Register register, None -> register_to_string register
    | Memory register, None -> Printf.sprintf "[%s]" (register_to_string register)
    | Memory register, Some displacement ->
      Printf.sprintf "[%s + %i]" (register_to_string register) displacement
    | MemorySum (left_register, right_register), None ->
      Printf.sprintf "[%s + %s]"
        (register_to_string left_register)
        (register_to_string right_register)
    | MemorySum (left_register, right_register), Some displacement ->
      Printf.sprintf "[%s + %s + %i]"
        (register_to_string left_register)
        (register_to_string right_register)
        displacement
    | DirectAddress address, None -> Printf.sprintf "%i" address
    | _ -> "error"

type instruction =
  | MovRegisterMemoryToFromRegister of direction * data_size * mode * register * register_memory
  | MovImmediateToRegisterMemory of data_size * mode * register_memory
  | MovImmediateToRegister of data_size * register * int

let parse_instruction bytes idx =
  let first_byte = Bytes.get_uint8 bytes idx in
    match first_byte land 0b11110000 with
      | 0b10000000 -> (
        match first_byte land 0b11111100 with
          | 0b10001000 -> (
            let second_byte = Bytes.get_uint8 bytes (idx + 1) in
            let direction = parse_direction first_byte in
            let data_size = parse_data_size first_byte in
            let mode = parse_mode (second_byte lsr 6) bytes idx in
            let register = parse_register ((second_byte lsr 3) land 0b111, data_size) in
            let register_memory = parse_register_memory mode (second_byte land 0b111) data_size in
              match mode with
                | RegisterMode ->
                  ( MovRegisterMemoryToFromRegister
                      (direction, data_size, mode, register, register_memory),
                    2 )
                | MemoryModeLowDisplacement _ ->
                  ( MovRegisterMemoryToFromRegister
                      (direction, data_size, mode, register, register_memory),
                    3 )
                | MemoryModeHighDisplacement _ ->
                  ( MovRegisterMemoryToFromRegister
                      (direction, data_size, mode, register, register_memory),
                    4 )
                | MemoryMode ->
                  ( MovRegisterMemoryToFromRegister
                      (direction, data_size, mode, register, register_memory),
                    2 ))
          | _ -> raise (InvalidOpcode first_byte))
      | 0b11000000 -> (
        match first_byte land 0b11111110 with
          | 0b11000110 -> (
            let second_byte = Bytes.get_uint8 bytes (idx + 1) in
            let data_size = parse_data_size first_byte in
            let mode = parse_mode (second_byte lsr 6) bytes idx in
            let register_memory = parse_register_memory mode (second_byte land 0b111) data_size in
              match mode with
                | RegisterMode ->
                  ( MovImmediateToRegisterMemory (data_size, mode, register_memory),
                    match data_size with
                      | ByteData -> 3
                      | WordData -> 4 )
                | MemoryModeLowDisplacement _ ->
                  ( MovImmediateToRegisterMemory (data_size, mode, register_memory),
                    match data_size with
                      | ByteData -> 4
                      | WordData -> 5 )
                | MemoryModeHighDisplacement _ ->
                  ( MovImmediateToRegisterMemory (data_size, mode, register_memory),
                    match data_size with
                      | ByteData -> 5
                      | WordData -> 6 )
                | MemoryMode ->
                  ( MovImmediateToRegisterMemory (data_size, mode, register_memory),
                    match data_size with
                      | ByteData -> 3
                      | WordData -> 4 ))
          | _ -> raise (InvalidOpcode first_byte))
      | 0b10110000 -> (
        let data_size = parse_data_size ((first_byte lsr 3) land 0b1) in
        let register = parse_register (first_byte land 0b111, data_size) in
          match data_size with
            | ByteData ->
              (MovImmediateToRegister (data_size, register, Bytes.get_uint8 bytes (idx + 1)), 2)
            | WordData ->
              (MovImmediateToRegister (data_size, register, Bytes.get_uint16_ne bytes (idx + 1)), 3)
        )
      | _ -> raise (InvalidOpcode first_byte)

let instruction_to_string = function
  | MovRegisterMemoryToFromRegister (direction, _, mode, register, register_memory) -> (
    match (direction, mode) with
      | DestinationToRegister, RegisterMode ->
        Printf.sprintf "mov %s, %s" (register_to_string register)
          (register_memory_to_string register_memory None)
      | SourceFromRegister, RegisterMode ->
        Printf.sprintf "mov %s, %s"
          (register_memory_to_string register_memory None)
          (register_to_string register)
      | ( DestinationToRegister,
          (MemoryModeLowDisplacement displacement | MemoryModeHighDisplacement displacement) ) ->
        Printf.sprintf "mov %s, %s" (register_to_string register)
          (register_memory_to_string register_memory (Some displacement))
      | ( SourceFromRegister,
          (MemoryModeLowDisplacement displacement | MemoryModeHighDisplacement displacement) ) ->
        Printf.sprintf "mov %s, %s"
          (register_memory_to_string register_memory (Some displacement))
          (register_to_string register)
      | DestinationToRegister, MemoryMode ->
        Printf.sprintf "mov %s, %s" (register_to_string register)
          (register_memory_to_string register_memory None)
      | SourceFromRegister, MemoryMode ->
        Printf.sprintf "mov %s, %s"
          (register_memory_to_string register_memory None)
          (register_to_string register))
  | MovImmediateToRegisterMemory (data_size, mode, register_memory) -> (
    match mode with
      | RegisterMode -> Printf.sprintf "mov %s" (register_memory_to_string register_memory None)
      | MemoryModeLowDisplacement displacement | MemoryModeHighDisplacement displacement -> (
        match data_size with
          | ByteData ->
            Printf.sprintf "mov %s byte"
              (register_memory_to_string register_memory (Some displacement))
          | WordData ->
            Printf.sprintf "mov %s word"
              (register_memory_to_string register_memory (Some displacement)))
      | MemoryMode -> (
        match data_size with
          | ByteData ->
            Printf.sprintf "mov %s byte" (register_memory_to_string register_memory None)
          | WordData ->
            Printf.sprintf "mov %s word" (register_memory_to_string register_memory None)))
  | MovImmediateToRegister (_, register, value) ->
    Printf.sprintf "mov %s, %i" (register_to_string register) value

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
  let filename = "./part1/listing_0039_more_movs" in
  let bytes = read_bytes_from_file filename in
    print_bytes_in_binary 16 bytes
