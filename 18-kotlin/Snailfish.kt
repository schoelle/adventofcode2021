import java.io.File
import kotlin.collections.ArrayList

enum class NodeType { LEAF, TREE }

abstract class NodeContent()

class Node() {
    private var ctype: NodeType = NodeType.LEAF
    private var value: Int = 0
    private var left: Node? = null
    private var right: Node? = null

    override fun toString(): String {
	if (ctype == NodeType.LEAF) {
	    return "$value"
	}
	val leftString = left?.toString()
	val rightString = right?.toString()
	return "[$leftString,$rightString]"
    }

    fun copy(): Node {
	val ret = Node()
	if (ctype == NodeType.LEAF) {
	    ret.setValue(value)
	} else {
	    ret.setChildren(getLeft().copy(),getRight().copy())
	}
	return ret
    }
    
    fun setValue(value: Int) {
	this.left = null
	this.right = null
	this.value = value
	this.ctype = NodeType.LEAF
    }

    fun setChildren(left: Node, right: Node) {
	this.left = left
	this.right = right
	this.ctype = NodeType.TREE
    }

    fun getType(): NodeType {
	return ctype
    }

    fun getLeft(): Node {
	return left!!
    }

    fun getRight(): Node {
	return right!!
    }

    fun getValue(): Int {
	return value
    }

    fun leftLeafNode(): Node {
	if (ctype == NodeType.LEAF) {
	    return this
	}
	return left!!.leftLeafNode();
    }

    fun rightLeafNode(): Node {
	if (ctype == NodeType.LEAF) {
	    return this
	}
	return right!!.rightLeafNode();
    }

    fun explode(depth: Int, leftValue: Node?, rightValue: Node?): Boolean {
	if (ctype == NodeType.LEAF) {
	    throw Exception("Can't explode value")
	}
	if ((depth > 4) &&
	     (getLeft().getType() == NodeType.LEAF) &&
	     (getRight().getType() == NodeType.LEAF)) {
	    leftValue?.let {
		leaf -> leaf.setValue(leaf.getValue() + getLeft().getValue())
	    }
	    rightValue?.let {
		leaf -> leaf.setValue(leaf.getValue() + getRight().getValue())
	    }
	    setValue(0)
	    return true
	}
	var done = false
	if (getLeft().getType() == NodeType.TREE) {
	    done = getLeft().explode(depth+1, leftValue, getRight().leftLeafNode())
	}
	if (!done && getRight().getType() == NodeType.TREE) {
	    done = getRight().explode(depth+1, getLeft().rightLeafNode(), rightValue)
	}
	return done
    }

    fun split(): Boolean {
	if (ctype == NodeType.LEAF) {
	    if (value >= 10) {
		val left = Node()
		val right = Node()
		left.setValue(value / 2)
		right.setValue(value - left.getValue())
		setChildren(left, right)
		return true
	    }
	    return false
	}
	var done = getLeft().split()
	if (!done) {
	    done = getRight().split()
	}
	return done
    }

    fun magnitude(): Int {
	if (ctype == NodeType.LEAF) {
	    return value
	}
	return 3 * getLeft().magnitude() + 2 * getRight().magnitude()
    }
}

fun add(left:Node,right:Node): Node {
    val node = Node()
    node.setChildren(left.copy(),right.copy())
    var done = false
    while (!done) {
	done = !node.explode(1,null,null) && !node.split()
    }
    return node
}

var index: Int = 0
var currentLine: String = ""

fun current(): Char {
    return currentLine[index]
}

fun next() {
    index++
}

fun readNode(): Node {
    if (current() == '[') {
	next() // [
	val leftNode = readNode()
	next() // ,
	val rightNode = readNode()
	next() // ]
	val node = Node()
	node.setChildren(leftNode, rightNode)
	return node
    } else {
	val startIndex = index
        while (current().isDigit()) next()
        val numStr = currentLine.substring(startIndex, index)
	val node = Node()
	node.setValue(numStr.toInt())
	return node
    }
}
    
fun parseLine(line: String): Node {
    currentLine = line
    index = 0
    return readNode()
}

fun readInput(fileName: String): ArrayList<Node> {
    val res = ArrayList<Node>()
    File(fileName).forEachLine {
	if (it != "") {
	    res.add(parseLine(it))
	}
    }
    return res
}

fun problem1(nodes: ArrayList<Node>) {
    var node = nodes[0]
    for (i in 1..nodes.size-1) {
	node = add(node,nodes[i])
    }
    println(node.magnitude())
}

fun problem2(nodes: ArrayList<Node>) {
    var max = 0
    for (i in 0..nodes.size-1) {
	for (j in 0..nodes.size-1) {
	    if (i != j) {
		val value = add(nodes[i],nodes[j]).magnitude()
		if (value > max) {
		    max = value
		}
	    }
	}
    }
    println(max)
}

fun main(args: Array<String>) {
    val nodes = readInput(args[0])
    problem1(nodes)
    problem2(nodes)
}

