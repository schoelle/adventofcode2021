note
	description: "AdventOfCode 2021 Day 4 solution"
	date: "$Date$"
	revision: "$Revision$"

class
	APPLICATION

inherit
	ARGUMENTS_32
	ALGAE_USER

create
	make

feature {NONE} -- Initialization

	numbers: DS_ARRAYED_LIST[DOUBLE]
		-- Input numbers

	boards: DS_ARRAYED_LIST[BOARD]
		-- Bingo boards

	read_input(file_name: IMMUTABLE_STRING_32)
			-- Read the inputs from `file_name` and initialize state
		local
			file: PLAIN_TEXT_FILE
			number_parts: DS_LIST[STRING]
			splitter: ST_SPLITTER
			builder: AL_MATRIX_BUILDER
			bingo_row: ARRAY[DOUBLE]
			i, j: INTEGER
			board: BOARD
		do
			-- Read number line
			create file.make_open_read (file_name)
			create splitter.make_with_separators (",")
			file.read_line
			number_parts := splitter.split (file.last_string)
			across
				number_parts as part
			loop
				part.item.adjust
				numbers.force_last (part.item.to_double)
			end
			-- Skip empty line
			file.read_line
			-- Read bingo definitions
			create splitter.make
			from
			until
				file.end_of_file
			loop
				builder := al.new_horizontal_builder
				from
					i := 1
				until
					i > 5
				loop
					create bingo_row.make_filled (0.0, 1, 5)
					file.read_line
					number_parts := splitter.split (file.last_string)
					j := 1
					across
						number_parts as v
					loop
						bingo_row.put (v.item.to_double, j)
						j := j + 1
					end
					builder.add_array (bingo_row)
					i := i + 1
				end
				-- Skip empty line
				if not file.end_of_file then
					file.read_line
				end
				create board.make
				builder.item.copy_values_into (board.numbers)
				boards.force_last (board)
			end
			file.close
		end

	play_bingo
		local
			bingo_found: BOOLEAN
			draw: DOUBLE
			i: INTEGER
		do
			from
				i := 1
			until
				bingo_found
			loop
				draw := numbers.item(i)
				across
					boards as board
				loop
					board.item.mark (draw)
					if board.item.has_bingo then
						bingo_found := true
						print("Score for first bingo is " + board.item.score.out + "%N")
					end
				end
				i := i + 1
			end
		end

	find_worst
		local
			draw: DOUBLE
			i: INTEGER
			last_board: BOARD
		do
			from
				i := 1
			until
				boards.is_empty
			loop
				draw := numbers.item(i)
				from
					boards.start
				until
					boards.off
				loop
					last_board := boards.item_for_iteration
					last_board.mark (draw)
					if last_board.has_bingo then
						boards.remove_at
					else
						boards.forth
					end
				end
				i := i + 1
			end
			if attached last_board then
				print("Worst board has score " + last_board.score.out + "%N")
			end
		end

	make
			-- Run application.
		local
			file_name: IMMUTABLE_STRING_32
		do
			create numbers.make_default
			create boards.make_default

			if argument_count >= 1 then
				file_name := argument (1)
				read_input(file_name)
				play_bingo
				across
					boards as board
				loop
					board.item.reset
				end
				find_worst
			else
				print ("Input file argument missing.%N")
			end
		end

end
