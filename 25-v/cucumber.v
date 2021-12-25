import os

fn read_data(filename string) [][]byte {
	lines := os.read_lines(filename) or { exit(1) }
	width := lines[0].len
	height := lines.len
	mut data := [][]byte{len: height, init: []byte{len: width, init: `.`}}
	for y := 0; y < height; y += 1 {
		for x := 0; x < width; x += 1 {
			data[y][x] = lines[y][x]
		}
	}
	return data
}

fn do_horizontal_step(mut data [][]byte, mut buffer [][]byte) int {
	height := data.len
	width := data[0].len
	mut count := 0
	for y := 0; y < height; y += 1 {
		for x := 0; x < width; x += 1 {
			left := (x - 1 + width) % width
			right := (x + 1) % width
			match data[y][x] {
				`.` {
					if data[y][left] == `>` {
						buffer[y][x] = `>`
					} else {
						buffer[y][x] = `.`
					}
				}
				`>` {
					if data[y][right] == `.` {
						buffer[y][x] = `.`
						count += 1
					} else {
						buffer[y][x] = `>`
					}
				}
				`v` {
					buffer[y][x] = `v`
				}
				else {}
			}
		}
	}
	return count
}

fn do_vertical_step(mut data [][]byte, mut buffer [][]byte) int {
	height := data.len
	width := data[0].len
	mut count := 0
	for y := 0; y < height; y += 1 {
		for x := 0; x < width; x += 1 {
			above := (y - 1 + height) % height
			below := (y + 1) % height
			match data[y][x] {
				`.` {
					if data[above][x] == `v` {
						buffer[y][x] = `v`
					} else {
						buffer[y][x] = `.`
					}
				}
				`v` {
					if data[below][x] == `.` {
						buffer[y][x] = `.`
						count += 1
					} else {
						buffer[y][x] = `v`
					}
				}
				`>` {
					buffer[y][x] = `>`
				}
				else {}
			}
		}
	}
	return count
}

fn count_steps(mut data [][]byte, mut buffer [][]byte) int {
	mut steps := 0
	mut changed := 1
	for changed >= 1 {
		c1 := do_horizontal_step(mut data, mut buffer)
		c2 := do_vertical_step(mut buffer, mut data)
		changed = c1 + c2
		steps += 1
	}
	return steps
}

fn main() {
	filename := os.args[1]
	mut data := read_data(filename)
	mut buffer := [][]byte{len: data.len, init: []byte{len: data[0].len}}
	steps := count_steps(mut data, mut buffer)
	println(steps)
}
