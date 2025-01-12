exception InvalidInput of int
exception InvalidOpcode of int

type s_field = [ `NoSign | `Sign ]

let map_s_field = function
  | 0b0 -> `NoSign
  | 0b1 -> `Sign
  | value -> failwith (Printf.sprintf "Invalid s_field value %i" value)

type data_size = [ `ByteData | `WordData ]

let data_size_bitshift = function
  | `ByteData -> 1
  | `WordData -> 2

let parse_data_size byte =
  match byte land 0b1 with
    | 0b0 -> `ByteData
    | 0b1 -> `WordData
    | value -> failwith (Printf.sprintf "Invalid data_size value %i" value)

let parse_data bytes idx data_size =
  match data_size with
    | `ByteData -> Bytes.get_uint8 bytes idx
    | `WordData -> Bytes.get_uint16_ne bytes idx

type direction = SourceFromRegister | DestinationToRegister

let parse_direction byte =
  match (byte lsr 1) land 0b1 with
    | 0b0 -> SourceFromRegister
    | 0b1 -> DestinationToRegister
    | value -> failwith (Printf.sprintf "Invalid direction value %i" value)

type v_field = [ `One | `InCLRegister ]

let map_v_field = function
  | 0b0 -> `One
  | 0b1 -> `InCLRegister
  | value -> failwith (Printf.sprintf "Invalid v_field value %i" value)

type z_field = [ `FlagClear | `FlagSet ]

let map_z_field = function
  | 0b0 -> `FlagClear
  | 0b1 -> `FlagSet
  | value -> failwith (Printf.sprintf "Invalid z_field value %i" value)

type mode =
  | RegisterMode
  | MemoryMode
  | MemoryModeLowDisplacement of int
  | MemoryModeHighDisplacement of int

let mode_bitshift = function
  | RegisterMode -> 2
  | MemoryMode -> 2
  | MemoryModeLowDisplacement _ -> 3
  | MemoryModeHighDisplacement _ -> 4

let parse_mode value bytes idx =
  match value with
    | 0b00 -> MemoryMode
    | 0b01 -> MemoryModeLowDisplacement (Bytes.get_uint8 bytes (idx + 2))
    | 0b10 -> MemoryModeHighDisplacement (Bytes.get_uint16_ne bytes (idx + 2))
    | 0b11 -> RegisterMode
    | value -> failwith (Printf.sprintf "Invalid mode value %i" value)

type register = AL | BL | CL | DL | AH | BH | CH | DH | AX | BX | CX | DX | SP | BP | SI | DI

let parse_register = function
  | 0b000, `ByteData -> AL
  | 0b011, `ByteData -> BL
  | 0b001, `ByteData -> CL
  | 0b010, `ByteData -> DL
  | 0b100, `ByteData -> AH
  | 0b111, `ByteData -> BH
  | 0b101, `ByteData -> CH
  | 0b110, `ByteData -> DH
  | 0b000, `WordData -> AX
  | 0b001, `WordData -> CX
  | 0b010, `WordData -> DX
  | 0b011, `WordData -> BX
  | 0b100, `WordData -> SP
  | 0b101, `WordData -> BP
  | 0b110, `WordData -> SI
  | 0b111, `WordData -> DI
  | value, _ -> failwith (Printf.sprintf "Invalid register value %i" value)

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

let is_direct_address = function
  | Memory BP -> true
  | DirectAddress _ -> true
  | _ -> false

let direct_address_bitshift register_memory data_size =
  if is_direct_address register_memory then
    match data_size with
      | `ByteData -> 1
      | `WordData -> 2
  else 0

let map_register_map_to_address_calculation = function
  | 0b000 -> MemorySum (BX, SI)
  | 0b001 -> MemorySum (BX, DI)
  | 0b010 -> MemorySum (BP, SI)
  | 0b011 -> MemorySum (BP, DI)
  | 0b100 -> Memory SI
  | 0b101 -> Memory DI
  | 0b110 -> Memory BP
  | 0b111 -> Memory BX
  | value -> failwith (Printf.sprintf "Invalid register_memory_valculation value %i" value)

let parse_register_memory mode register_byte data_size =
  match mode with
    | RegisterMode -> Register (parse_register (register_byte, data_size))
    | MemoryMode -> map_register_map_to_address_calculation register_byte
    | MemoryModeLowDisplacement _ | MemoryModeHighDisplacement _ ->
      map_register_map_to_address_calculation register_byte

let parse_direct_address_mode register_memory bytes idx =
  if is_direct_address register_memory then DirectAddress (Bytes.get_uint16_ne bytes (idx + 2))
  else register_memory

let register_memory_to_string mode displacement =
  match (mode, displacement) with
    | Register register, None -> register_to_string register
    | Memory register, None | Memory register, Some 0 ->
      Printf.sprintf "[%s]" (register_to_string register)
    | Memory register, Some displacement ->
      Printf.sprintf "[%s + %i]" (register_to_string register) displacement
    | MemorySum (left_register, right_register), None
    | MemorySum (left_register, right_register), Some 0 ->
      Printf.sprintf "[%s + %s]"
        (register_to_string left_register)
        (register_to_string right_register)
    | MemorySum (left_register, right_register), Some displacement ->
      Printf.sprintf "[%s + %s + %i]"
        (register_to_string left_register)
        (register_to_string right_register)
        displacement
    | DirectAddress address, None -> Printf.sprintf "[%i]" address
    | _ -> "error"

type operation = Mov

type instruction =
  | RegisterMemoryToFromRegister of
      operation * direction * data_size * mode * register * register_memory
  | ImmediateToRegisterMemory of operation * data_size * mode * register_memory * int
  | ImmediateToRegister of operation * data_size * register * int
  | MemoryToAccumulator of operation * data_size * int
  | AccumulatorToMemory of operation * data_size * int

let parse_instruction bytes idx =
  let first_byte = Bytes.get_uint8 bytes idx in
    match first_byte land 0b11110000 with
      | 0b10000000 -> (
        match first_byte land 0b11111100 with
          | 0b10001000 ->
            let second_byte = Bytes.get_uint8 bytes (idx + 1) in
            let direction = parse_direction first_byte in
            let data_size = parse_data_size first_byte in
            let mode = parse_mode (second_byte lsr 6) bytes idx in
            let register = parse_register ((second_byte lsr 3) land 0b111, data_size) in
            let register_memory = parse_register_memory mode (second_byte land 0b111) data_size in
            let register_memory = parse_direct_address_mode register_memory bytes idx in
            let bitshift = mode_bitshift mode + direct_address_bitshift register_memory data_size in
              ( RegisterMemoryToFromRegister
                  (Mov, direction, data_size, mode, register, register_memory),
                bitshift )
          | _ -> raise (InvalidOpcode first_byte))
      | 0b11000000 -> (
        match first_byte land 0b11111110 with
          | 0b11000110 ->
            let second_byte = Bytes.get_uint8 bytes (idx + 1) in
            let data_size = parse_data_size first_byte in
            let mode = parse_mode (second_byte lsr 6) bytes idx in
            let register_memory = parse_register_memory mode (second_byte land 0b111) data_size in
            let data = parse_data bytes (idx + mode_bitshift mode) data_size in
            let bitshift = mode_bitshift mode + data_size_bitshift data_size in
              (ImmediateToRegisterMemory (Mov, data_size, mode, register_memory, data), bitshift)
          | _ -> raise (InvalidOpcode first_byte))
      | 0b10110000 ->
        let data_size = parse_data_size ((first_byte lsr 3) land 0b1) in
        let register = parse_register (first_byte land 0b111, data_size) in
        let mode_bitshift = 1 in
        let bitshift = mode_bitshift + data_size_bitshift data_size in
        let data = parse_data bytes (idx + mode_bitshift) data_size in
          (ImmediateToRegister (Mov, data_size, register, data), bitshift)
      | 0b10100000 -> (
        match first_byte land 0b11111110 with
          | 0b10100000 ->
            let data_size = parse_data_size first_byte in
            let mode_bitshift = 1 in
            let data = parse_data bytes (idx + mode_bitshift) data_size in
            let bitshift = mode_bitshift + data_size_bitshift data_size in
              (MemoryToAccumulator (Mov, data_size, data), bitshift)
          | 0b10100010 ->
            let data_size = parse_data_size first_byte in
            let mode_bitshift = 1 in
            let data = parse_data bytes (idx + mode_bitshift) data_size in
            let bitshift = mode_bitshift + data_size_bitshift data_size in
              (AccumulatorToMemory (Mov, data_size, data), bitshift)
          | _ -> raise (InvalidOpcode first_byte))
      | _ -> failwith (Printf.sprintf "Invalid opcode %i" (Bytes.get_uint8 bytes (idx - 2)))

let instruction_to_string = function
  | RegisterMemoryToFromRegister (Mov, direction, _, mode, register, register_memory) -> (
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
  | ImmediateToRegisterMemory (Mov, data_size, mode, register_memory, data) -> (
    match mode with
      | RegisterMode -> Printf.sprintf "mov %s" (register_memory_to_string register_memory None)
      | MemoryModeLowDisplacement displacement | MemoryModeHighDisplacement displacement -> (
        match data_size with
          | `ByteData ->
            Printf.sprintf "mov %s byte %i"
              (register_memory_to_string register_memory (Some displacement))
              data
          | `WordData ->
            (Printf.sprintf "mov %s word %i"
               (register_memory_to_string register_memory (Some displacement)))
              data)
      | MemoryMode -> (
        match data_size with
          | `ByteData ->
            Printf.sprintf "mov %s byte %i" (register_memory_to_string register_memory None) data
          | `WordData ->
            (Printf.sprintf "mov %s word %i" (register_memory_to_string register_memory None)) data)
    )
  | ImmediateToRegister (Mov, _, register, value) ->
    Printf.sprintf "mov %s, %i" (register_to_string register) value
  | MemoryToAccumulator (Mov, _, value) -> Printf.sprintf "mov ax, [%i]" value
  | AccumulatorToMemory (Mov, _, value) -> Printf.sprintf "mov [%i], ax" value

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
  let filename = "./part1/listing_0040_challenge_movs" in
  let bytes = read_bytes_from_file filename in
    print_bytes_in_binary 16 bytes
