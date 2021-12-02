BEGIN { depth = 0; forward = 0 }
/forward/ { forward += $2 }
/down/ { depth += $2 }
/up/ { depth -= $2 }
END { print depth, forward, (depth * forward) }
