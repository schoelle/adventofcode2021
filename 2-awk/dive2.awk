BEGIN { depth = 0; forward = 0; aim = 0 }
/forward/ { forward += $2 ; depth += aim * $2 }
/down/ { aim += $2 }
/up/ { aim -= $2 }
END { print depth, forward, (depth * forward) }
