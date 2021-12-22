Object subclass: Vector [
    | x y z |
    <comment: 'A vector in 3D space'>
    Vector class >> new [
	| r |
	r := super new.
	r init.
	^r
    ]

    Vector class >> x: xv y: yv z: zv [
	| r |
	r := Vector new.
	^((r setX: xv) setY: yv) setZ: zv
    ]

    init [
	x := 0.
	y := 0.
	z := 0.
    ]
    
    printOn: stream [
	stream nextPutAll: '[x:'.
	x printOn: stream.
	stream nextPutAll: ',y:'.
	y printOn: stream.
	stream nextPutAll: ',z:'.
	z printOn: stream.
	stream nextPutAll: ']'.
    ]

    xValue [ ^x ]
    yValue [ ^y ]
    zValue [ ^z ]
    setX: v [ x := v. ^self ]
    setY: v [ y := v. ^self ]
    setZ: v [ z := v. ^self ]
].

Object subclass: Cube [
    | from to |
    <comment: 'I represent a 3-dimensional cube'>

    Cube class >> from: p1 to: p2 [
	| r |
	r := super new.
	r setFrom: p1.
	r setTo: p2.
	^r
    ]
    
    printOn: stream [
	super printOn: stream.
	stream nextPutAll: ' from: '.
	from printOn: stream.
	stream nextPutAll: ' to: '.
	to printOn: stream.	    
	stream nextPutAll: ' (size: '.
	(self getSize) printOn: stream.
	stream nextPutAll: ')'.	
    ]

    getFrom [ ^from ]
    getTo [ ^to ]
    setFrom: p [ from := p ]
    setTo: p [ to := p ]

    getSize [
	| r |
	r := (Vector x: (((to xValue) - (from xValue) + 1) max: 0)
		     y: (((to yValue) - (from yValue) + 1) max: 0)
		     z: (((to zValue) - (from zValue) + 1) max: 0)).
	^r
		  
    ]

    volume [
	^((self getSize) xValue) * ((self getSize) yValue) * ((self getSize) zValue)
    ]

    hasVolume [
	^((self volume) > 0)
    ]

    subtract: other [
	| c r |
	r := OrderedCollection new.
	c:= Cube
	    from: from
	    to: (Vector x: (to xValue)
			y: (to yValue)
			z: ((to zValue) min: (((other getFrom) zValue) - 1))). 
	(c hasVolume) ifTrue: [r add: c].
	c := Cube
	    from: (Vector x: (from xValue)
			  y: (from yValue)
			  z: ((from zValue) max: (((other getTo) zValue) + 1)))
	    to: to.
	(c hasVolume) ifTrue: [r add: c].
	c := Cube
	    from: (Vector x: (from xValue)
			  y: (from yValue) 
			  z: ((from zValue) max: (((other getFrom) zValue))))
	    to: (Vector x: (to xValue)
			y: ((to yValue) min: (((other getFrom) yValue) - 1))
			z: ((to zValue) min: (((other getTo) zValue)))).
	(c hasVolume) ifTrue: [r add: c].
	c := Cube
	    from: (Vector x: (from xValue)
			  y: ((from yValue) max: (((other getTo) yValue) + 1))
			  z: ((from zValue) max: (((other getFrom) zValue))))
	    to: (Vector x: (to xValue)
			y: (to yValue)
			z: ((to zValue) min: (((other getTo) zValue)))).
	(c hasVolume) ifTrue: [r add: c].
	c := Cube
	    from: (Vector x: (from xValue)
			  y: ((from yValue) max: (((other getFrom) yValue)))
			  z: ((from zValue) max: (((other getFrom) zValue))))
	    to: (Vector x: ((to xValue) min: (((other getFrom) xValue) - 1)) 
			y: ((to yValue) min: (((other getTo) yValue)))
			z: ((to zValue) min: (((other getTo) zValue)))).
	(c hasVolume) ifTrue: [r add: c].
	c := Cube
	    from: (Vector x: ((from xValue) max: (((other getTo) xValue) + 1))
			  y: ((from yValue) max: (((other getFrom) yValue)))
			  z: ((from zValue) max: (((other getFrom) zValue))))
	    to: (Vector x: (to xValue) 
			y: ((to yValue) min: (((other getTo) yValue)))
			z: ((to zValue) min: (((other getTo) zValue)))).
	(c hasVolume) ifTrue: [r add: c].
	^r
    ]

    prune: bounds [
	from := Vector x: ((from xValue) max: ((bounds getFrom) xValue))
		       y: ((from yValue) max: ((bounds getFrom) yValue))
		       z: ((from zValue) max: ((bounds getFrom) zValue)).
	to := Vector x: ((to xValue) min: ((bounds getTo) xValue))
		     y: ((to yValue) min: ((bounds getTo) yValue))
		     z: ((to zValue) min: ((bounds getTo) zValue)).	
    ]

]

Object subclass: Universe [
    | cubes |
    <comment: 'A universe of cubes'>
    Universe class >> new [
	| r |
	r := super new.
	r init.
	^r
    ]
    
    init [
	cubes := OrderedCollection new.
    ]
    
    addCube: cube [
	self subtractCube: cube.
	cubes add: cube.
    ]
    
    subtractCube: cube [
	| n |
	n := OrderedCollection new.
	cubes do: [ :c |
	    n addAll: (c subtract: cube)
	].
	cubes := n
    ]

    addFromFile: filename [
	| f |
	f := FileStream open: filename mode: FileStream read.
	f linesDo: [ :line |
	    (line =~ '(\w+) x=(.*)\.\.(.*),y=(.*)\.\.(.*),z=(.*)\.\.(.*)') ifMatched: [:match |
		| p1 p2 c |
		    p1 := Vector x: ((match at: 2) asNumber) y: ((match at: 4) asNumber) z: ((match at: 6) asNumber).
		p2 := Vector x: ((match at: 3) asNumber) y: ((match at: 5) asNumber) z: ((match at: 7) asNumber).
		c := Cube from: p1 to: p2.
		(match at: 1) = 'on' ifTrue: [self addCube: c.] ifFalse: [self subtractCube: c].
	    ].
	].
	f close.
    ]

    prune: bounds [
	| n |
	n := OrderedCollection new.
	cubes do: [ :c |
	    | nc |
	    nc := (c prune: bounds).
	    (nc hasVolume) ifTrue: [ n add: nc ].
	].
	cubes := n
    ]

    printOn: stream [
	stream nextPutAll: 'Universe:'.
	stream nl.
	cubes do: [:c |
	    c printOn: stream.
	    stream nl.
	]
    ]

    volume [
	| v |
	v := 0.
	cubes do: [ :c |
	    v := v + (c volume).
	].
	^v
    ]

]

universe := Universe new.
universe addFromFile: (Smalltalk arguments at: 1).
'Problem 2: ' display.
(universe volume) printNl.
universe prune: (Cube from: (Vector x: -50 y: -50 z: -50) to: (Vector x: 50 y: 50 z: 50)).
'Problem 1: ' display.
(universe volume) printNl.
