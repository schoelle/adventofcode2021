package main
import (
	"bufio"
	"log"
	"os"
)

var index int
var data string
var bitsLeft int
var bitsRead int
var versionSum int

func min(a int,b int) int {
	if a > b { return b }
	return a
}

func max(a int,b int) int {
	if a < b { return b }
	return a
}

func current() int {
	c := int(data[index])
	if c > 64 { return c-55 }
	return c-48
}

func initInput(newData string) {
	data = newData
	index = 0
	bitsLeft = 4
	bitsRead = 0
	versionSum = 0
}

func read(bits int) int {
	value := 0
	bitsNeeded := bits
	for bitsNeeded > 0 {
		bitsUsed := min(bitsLeft,bitsNeeded)
		bitsLeft = bitsLeft-bitsUsed
		value = value << bitsUsed
		cur := (current() >> bitsLeft) & ((1 << bitsUsed)-1)
		value = value | cur
		bitsNeeded = bitsNeeded - bitsUsed
		if bitsLeft == 0 {
			bitsLeft = 4
			index = index + 1
		}
	}
	bitsRead = bitsRead + bits
	return value
}

func blockSum(values []int) int {
	result := 0
	for _,v := range values { result += v }
	return result
}

func blockProduct(values []int) int {
	result := 1
	for _,v := range values { result *= v }
	return result
}

func blockMinimum(values []int) int {
	result := values[0]
	for _,v := range values { result = min(result,v) }
	return result	
}

func blockMaximum(values []int) int {
	result := values[0]
	for _,v := range values { result = max(result,v) }
	return result	
}

func blockGreater(values []int) int {
	if values[0] > values[1] { return 1 }
	return 0
}

func blockSmaller(values []int) int {
	if values[0] < values[1] { return 1 }
	return 0
}

func blockEquals(values []int) int {
	if values[0] == values[1] { return 1 }
	return 0
}

func readSubblocks() []int {
	var res []int
	lengthType := read(1)
	if lengthType == 0 {
		endBits := bitsRead + read(15)
		for bitsRead < endBits {
			res = append(res, readBlock())
		}
	} else {
		toRead := read(11)
		for toRead > 0 {
			res = append(res, readBlock())
			toRead = toRead - 1
		}
	}
	return res
}

func readLiteral() int {
	value := 0
	end := 1
	for end > 0 {
		end = read(1)
		value = (value << 4) | read(4)
	}
	return value
}

func readBlock() int {
	version := read(3)
	versionSum = versionSum + version
	btype := read(3)
	if btype == 4 {
		return readLiteral()
	} else {
		values := readSubblocks()
		if btype == 0 { return blockSum(values) }
		if btype == 1 { return blockProduct(values) }
		if btype == 2 { return blockMinimum(values) }
		if btype == 3 { return blockMaximum(values) }
		if len(values) != 2 {
			log.Fatalf("Got %d arguments to operation %d", len(values), btype)
		}
		if btype == 5 { return blockGreater(values) }
		if btype == 6 { return blockSmaller(values) }
		if btype == 7 { return blockEquals(values) }
	}
	log.Fatalf("Unhandled type %d", btype)
	return 0
}

func process(line string) {
	initInput(line);
	value := readBlock()
	log.Printf("VersionSum %d", versionSum)
	log.Printf("Value %d", value)
}

func main() {
	filename := os.Args[1]
	file, err := os.Open(filename)
	if err != nil {
		log.Fatalf("Error reading: %s", err)
	}
	scanner := bufio.NewScanner(file)
	scanner.Split(bufio.ScanLines)
	for scanner.Scan() {
		process(scanner.Text())
	}
	file.Close()
}
