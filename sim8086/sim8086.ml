let int_to_binary n =
  if n = 0 then "0"
  else
    let rec loop x acc = if x = 0 then acc else loop (x lsr 1) (string_of_int (x land 1) ^ acc) in
      loop n ""

exception InvalidInput of int

module S = struct
  type t = [ `NoSign | `Sign ]

  let parse = function
    | 0b0 -> `NoSign
    | 0b1 -> `Sign
    | code -> failwith (Printf.sprintf "Invalid s value %i" code)
end

module W = struct
  type t = Byte | Word

  let parse code =
    match code land 0b1 with
      | 0b0 -> Byte
      | 0b1 -> Word
      | code -> failwith (Printf.sprintf "Invalid W code %i" code)

  let bitshift = function
    | Byte -> 1
    | Word -> 2
end

module D = struct
  type t = SourceFromRegister | DestinationToRegister

  let parse code =
    match (code lsr 1) land 0b1 with
      | 0b0 -> SourceFromRegister
      | 0b1 -> DestinationToRegister
      | code -> failwith (Printf.sprintf "Invalid direction value %i" code)
end

module V = struct
  type t = [ `One | `InCLRegister ]

  let parse = function
    | 0b0 -> `One
    | 0b1 -> `InCLRegister
    | code -> failwith (Printf.sprintf "Invalid v_field value %i" code)
end

module Z = struct
  type t = [ `FlagClear | `FlagSet ]

  let parse = function
    | 0b0 -> `FlagClear
    | 0b1 -> `FlagSet
    | code -> failwith (Printf.sprintf "Invalid z_field value %i" code)
end

module Data = struct
  type t = Byte of int | Word of int

  let parse bytes idx data_size =
    match data_size with
      | W.Byte -> Byte (Bytes.get_uint8 bytes idx)
      | W.Word -> Word (Bytes.get_uint16_ne bytes idx)

  let to_string = function
    | Byte value -> Printf.sprintf "byte %i" value
    | Word value -> Printf.sprintf "word %i" value
end

module Displacement = struct
  type t = Low of int | High of int | None
end

module Mode = struct
  type t = Register | Memory of Displacement.t

  let bitshift = function
    | Register -> 2
    | Memory None -> 2
    | Memory (Low _) -> 3
    | Memory (High _) -> 4

  let parse value bytes idx =
    match value with
      | 0b00 -> Memory None
      | 0b01 -> Memory (Low (Bytes.get_uint8 bytes (idx + 2)))
      | 0b10 -> Memory (High (Bytes.get_uint16_ne bytes (idx + 2)))
      | 0b11 -> Register
      | value -> failwith (Printf.sprintf "Invalid mode value %i" value)
end

module Register = struct
  type t = AL | BL | CL | DL | AH | BH | CH | DH | AX | BX | CX | DX | SP | BP | SI | DI

  let parse code data_size =
    match (code, data_size) with
      | 0b000, W.Byte -> AL
      | 0b011, W.Byte -> BL
      | 0b001, W.Byte -> CL
      | 0b010, W.Byte -> DL
      | 0b100, W.Byte -> AH
      | 0b111, W.Byte -> BH
      | 0b101, W.Byte -> CH
      | 0b110, W.Byte -> DH
      | 0b000, W.Word -> AX
      | 0b001, W.Word -> CX
      | 0b010, W.Word -> DX
      | 0b011, W.Word -> BX
      | 0b100, W.Word -> SP
      | 0b101, W.Word -> BP
      | 0b110, W.Word -> SI
      | 0b111, W.Word -> DI
      | code, _ -> failwith (Printf.sprintf "Invalid register code %i" code)

  let to_string = function
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
end

module RegisterMemory = struct
  type t =
    | Register of Register.t
    | Memory of Register.t * Displacement.t
    | MemorySum of Register.t * Register.t * Displacement.t
    | DirectAddress of int

  let is_direct_address = function
    | Memory (BP, _) -> true
    | DirectAddress _ -> true
    | _ -> false

  let direct_address_bitshift register_memory data_size =
    if is_direct_address register_memory then
      match data_size with
        | W.Byte -> 1
        | W.Word -> 2
    else 0

  let map_register_map_to_address_calculation value displacement =
    match value with
      | 0b000 -> MemorySum (BX, SI, displacement)
      | 0b001 -> MemorySum (BX, DI, displacement)
      | 0b010 -> MemorySum (BP, SI, displacement)
      | 0b011 -> MemorySum (BP, DI, displacement)
      | 0b100 -> Memory (SI, displacement)
      | 0b101 -> Memory (DI, displacement)
      | 0b110 -> Memory (BP, displacement)
      | 0b111 -> Memory (BX, displacement)
      | value -> failwith (Printf.sprintf "Invalid register_memory_valculation value %i" value)

  let parse mode register_code data_size =
    match mode with
      | Mode.Register ->
        let register = Register.parse register_code data_size in
          Register register
      | Mode.Memory displacement ->
        map_register_map_to_address_calculation register_code displacement

  let parse_direct_address_mode register_memory bytes idx =
    if is_direct_address register_memory then DirectAddress (Bytes.get_uint16_ne bytes (idx + 2))
    else register_memory

  let to_string register_memory =
    match register_memory with
      | Register register -> Register.to_string register
      | Memory (register, None) | Memory (register, Low 0) | Memory (register, High 0) ->
        Printf.sprintf "[%s]" (Register.to_string register)
      | Memory (register, Low displacement) | Memory (register, High displacement) ->
        Printf.sprintf "[%s + %i]" (Register.to_string register) displacement
      | MemorySum (left_register, right_register, None)
      | MemorySum (left_register, right_register, Low 0)
      | MemorySum (left_register, right_register, High 0) ->
        Printf.sprintf "[%s + %s]"
          (Register.to_string left_register)
          (Register.to_string right_register)
      | MemorySum (left_register, right_register, Low displacement)
      | MemorySum (left_register, right_register, High displacement) ->
        Printf.sprintf "[%s + %s + %i]"
          (Register.to_string left_register)
          (Register.to_string right_register)
          displacement
      | DirectAddress address -> Printf.sprintf "[%i]" address
end

module Opcode = struct
  type t = Mov | Add | Sub | Cmp

  let to_string = function
    | Mov -> "mov"
    | Add -> "add"
    | Sub -> "sub"
    | Cmp -> "cmp"
end

type instruction =
  (*
  MOV: 100010dw | mod reg r/m | (DISP-LO) | (DISP-HI)
  ADD: 000000dw
  SUB: 001010dw
  CMP: 001110dw
  *)
  | RegisterMemoryToFromRegister of Opcode.t * D.t * W.t * Mode.t * Register.t * RegisterMemory.t
  (*
  MOV: 1100011w | mod 000 r/m | (DISP-LO) | (DISP-HI) | data | data if w=1
  ADD: 100000sw | mod 000 r/m | (DISP-LO) | (DISP-HI) | data | data if s:w=01
  SUB: 100000sw | mod 101 r/m
  CMP: 100000sw | mod 111 r/m
  *)
  | ImmediateToRegisterMemory of Opcode.t * W.t * Mode.t * RegisterMemory.t * Data.t
  (*
  MOV: 1011 w reg | data | data if w=1
  *)
  | ImmediateToRegister of Opcode.t * W.t * Register.t * Data.t
  (*
  ADD: 0000010w | data | data if w=1
  *)
  | ImmediateToAccumulator of Opcode.t * W.t * Data.t
  (*
  SUB: 0010110w | data | data if w=1
  *)
  | ImmediateFromAccumulator of Opcode.t * W.t * Data.t
  (*
  CMP: 0011110w | data
  *)
  | ImmediateWithAccumulator of Opcode.t * W.t * Data.t
  (*
  MOV: 1010001w | addr-lo | addr-hi
  *)
  | MemoryToAccumulator of Opcode.t * W.t * Data.t
  (*
  MOV: 1010001w | addr-lo | addr-hi
  *)
  | AccumulatorToMemory of Opcode.t * W.t * Data.t

exception InvalidOpcode of int

let parse_register_memory_and_register bytes idx =
  let first_byte = Bytes.get_uint8 bytes idx in
  let second_byte = Bytes.get_uint8 bytes (idx + 1) in
  let d = D.parse first_byte in
  let w = W.parse first_byte in
  let mode = Mode.parse (second_byte lsr 6) bytes idx in
  let register = Register.parse ((second_byte lsr 3) land 0b111) w in
  let register_memory = RegisterMemory.parse mode (second_byte land 0b111) w in
  let register_memory = RegisterMemory.parse_direct_address_mode register_memory bytes idx in
  let bitshift = Mode.bitshift mode + RegisterMemory.direct_address_bitshift register_memory w in
    (RegisterMemoryToFromRegister (Mov, d, w, mode, register, register_memory), bitshift)

let parse_instruction bytes idx =
  let first_byte = Bytes.get_uint8 bytes idx in
    match first_byte land 0b11110000 with
      | 0b10000000 -> (
        match first_byte land 0b11111100 with
          | 0b10001000 -> parse_register_memory_and_register bytes idx
          | _ -> raise (InvalidOpcode first_byte))
      | 0b11000000 -> (
        match first_byte land 0b11111110 with
          | 0b11000110 ->
            let second_byte = Bytes.get_uint8 bytes (idx + 1) in
            let data_size = W.parse first_byte in
            let mode = Mode.parse (second_byte lsr 6) bytes idx in
            let register_memory = RegisterMemory.parse mode (second_byte land 0b111) data_size in
            let data = Data.parse bytes (idx + Mode.bitshift mode) data_size in
            let bitshift = Mode.bitshift mode + W.bitshift data_size in
              (ImmediateToRegisterMemory (Mov, data_size, mode, register_memory, data), bitshift)
          | _ -> raise (InvalidOpcode first_byte))
      | 0b10110000 ->
        let data_size = W.parse ((first_byte lsr 3) land 0b1) in
        let register = Register.parse (first_byte land 0b111) data_size in
        let mode_bitshift = 1 in
        let bitshift = mode_bitshift + W.bitshift data_size in
        let data = Data.parse bytes (idx + mode_bitshift) data_size in
          (ImmediateToRegister (Mov, data_size, register, data), bitshift)
      | 0b10100000 -> (
        match first_byte land 0b11111110 with
          | 0b10100000 ->
            let data_size = W.parse first_byte in
            let mode_bitshift = 1 in
            let data = Data.parse bytes (idx + mode_bitshift) data_size in
            let bitshift = mode_bitshift + W.bitshift data_size in
              (MemoryToAccumulator (Mov, data_size, data), bitshift)
          | 0b10100010 ->
            let data_size = W.parse first_byte in
            let mode_bitshift = 1 in
            let data = Data.parse bytes (idx + mode_bitshift) data_size in
            let bitshift = mode_bitshift + W.bitshift data_size in
              (AccumulatorToMemory (Mov, data_size, data), bitshift)
          | _ -> raise (InvalidOpcode first_byte))
      | _ ->
        failwith
          (Printf.sprintf "Invalid opcode %s" (int_to_binary (Bytes.get_uint8 bytes (idx - 2))))

let instruction_to_string instruction =
  match instruction with
    | RegisterMemoryToFromRegister (opcode, direction, _, _, register, register_memory) -> (
      let reg = Register.to_string register in
      let rm = RegisterMemory.to_string register_memory in
        match direction with
          | DestinationToRegister -> Printf.sprintf "mov %s, %s" reg rm
          | SourceFromRegister -> Printf.sprintf "mov %s, %s" rm reg)
    | ImmediateToRegisterMemory (opcode, _, _, register_memory, data) ->
      Printf.sprintf "%s %s %s" (Opcode.to_string opcode)
        (RegisterMemory.to_string register_memory)
        (Data.to_string data)
    | ImmediateToRegister (opcode, _, register, (Word data | Byte data)) ->
      Printf.sprintf "mov %s, %i" (Register.to_string register) data
    | MemoryToAccumulator (opcode, _, (Word data | Byte data)) -> Printf.sprintf "mov ax, [%i]" data
    | AccumulatorToMemory (opcode, _, (Word data | Byte data)) -> Printf.sprintf "mov [%i], ax" data
    | ImmediateToAccumulator (opcode, w, data) -> "fo"
    | ImmediateFromAccumulator (opcode, w, data) -> "fo"
    | ImmediateWithAccumulator (opcode, w, data) -> "fo"

let read_bytes_from_file filename =
  let ic = open_in_bin filename in
  let file_size = in_channel_length ic in
  let buffer = Bytes.create file_size in
    really_input ic buffer 0 file_size;
    close_in ic;
    buffer

let print_bytes_in_binary bytes =
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
    print_bytes_in_binary bytes
