note
	description: "Bingo Board"
	author: "Bernd Schoeller"
	date: "$Date$"
	revision: "$Revision$"

class
	BOARD

inherit
	ALGAE_USER

create
	make

feature -- Creation

	make
		do
			numbers := al.matrix(5, 5)
			markers := al.matrix_filled (5, 5, 1.0)
			last_mark := 0.0
		end

feature -- Access

	numbers: AL_MATRIX
		-- Numbers on the board

	markers: AL_MATRIX
		-- Markers already on the board (0.0 = marked, 1.0 = not marked)

	last_mark: DOUBLE
		-- Last number used to mark the board

	score: DOUBLE
			-- Score of the board
		do
			Result := numbers.column_by_column.times (markers.column_by_column) * last_mark
		end

feature -- Status

	has_bingo: BOOLEAN
			-- Does the current board has `bingo` state (row or column all marked)?
		local
			sum: DOUBLE
		do
			across markers.rows as row loop
				sum := row.item.sum
				Result := Result or markers.same_double (sum, 0.0)
			end
			across markers.columns as column loop
				sum := column.item.sum
				Result := Result or markers.same_double (sum, 0.0)
			end
		end

feature -- Operations

	mark (a_number: DOUBLE)
			-- Mark a single number on the board
		do
			across numbers.column_by_column as cell loop
				if numbers.same_double (cell.item, a_number) then
					markers.put (0.0, cell.row, cell.column)
					last_mark := a_number
				end
			end
		end

	reset
			-- Reset all markers on the board
		do
			markers.fill (1.0)
		end

end
