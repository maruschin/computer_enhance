package harvesin

import "core:os"
import "core:strings"

write_file :: proc(filepath: string, data: string) {}

read_file :: proc(filepath: string) {
	data, ok := os.read_entire_file(filepath, context.allocator)
	if !ok {
		// could not read file
		return
	}
	defer delete(data, context.allocator)

	it := string(data)
	for line in strings.split_lines_iterator(&it) {
		// process line
	}
}



main :: proc() {}
