#!/usr/bin/node
filename = process.argv[2]
fs = require('fs')

function strsubset(a,b) {
    if (a.length > b.length) return false;
    for (var c of a) { if (!b.includes(c)) return false; }
    return true;
}

fs.readFile(filename, 'utf8', function(err,data) {
    const counts = {}
    var total = 0
    for (var i = 0; i < 10; i++) { counts[i] = 0 }
    if (err){ 
	console.log(err)
    } else {
	const lines = data.split('\n')
	for(const line of lines) {
	    parts = line.split(' | ')
	    if (parts.length == 2) {
		const input = parts[0].split(' ').map(x => Array.from(x).sort().join(""))
		const output = parts[1].split(' ').map(x => Array.from(x).sort().join(""))
		const digits = {}
		digits[1] = input.find(x => x.length == 2)
		digits[4] = input.find(x => x.length == 4)
		digits[7] = input.find(x => x.length == 3)
		digits[8] = input.find(x => x.length == 7)
		digits[9] = input.find(x => x.length == 6 && strsubset(digits[4],x))
		digits[0] = input.find(x => x.length == 6 && strsubset(digits[1],x) && x != digits[9])
		digits[6] = input.find(x => x.length == 6 && x != digits[9] && x != digits[0])
		digits[3] = input.find(x => x.length == 5 && strsubset(digits[1],x))
		digits[5] = input.find(x => x.length == 5 && strsubset(x,digits[6]))
		digits[2] = input.find(x => x.length == 5 && x != digits[3] && x != digits[5])
		const revdig = Object.fromEntries(Object.entries(digits).map(x => x.reverse()))
		const number = output.map(x => revdig[x])
		for (var n of number) { counts[n] += 1 }
		total += parseInt(number.join(''))
	    }
	}
    }
    console.log(counts[1] + counts[4] + counts[7] + counts[8])
    console.log(total)
})
