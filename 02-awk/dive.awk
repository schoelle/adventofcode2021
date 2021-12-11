#!/usr/bin/awk -f
/forward/ { forward += $2 }
/down/ { depth += $2 }
/up/ { depth -= $2 }
END { print (depth * forward) }
