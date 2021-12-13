#!/usr/bin/tclsh

# Read the input
set dots {}
set folds {}
set fn [lindex $argv 0]
set fp [open $fn r]
set inputtext [read $fp]
close $fp

# Parse the input
foreach line [split $inputtext "\n"] {
    if [regexp {fold along (.)=(.*)} $line -> xy pos] {
	lappend folds "$xy $pos"
    } elseif [regexp {(.*),(.*)} $line -> xpos ypos] {
	lappend dots "$xpos $ypos"
    }
}

# Perform folding
proc fold {dots xy pos} {
    # Fold the dots
    set newdots {}
    foreach dot $dots {
	set x [lindex $dot 0]
	set y [lindex $dot 1]
	if {$xy == "x"} {
	    if {$x > $pos} {
		lappend newdots "[expr 2 * $pos - $x] $y"
	    } else {
		lappend newdots "$x $y"
	    }
	} else {
	    if {$y > $pos} {
		lappend newdots "$x [expr 2 * $pos - $y]"
	    } else {
		lappend newdots "$x $y"
	    }
	}
    }
    # Remove duplicates
    set result {}
    foreach dot $newdots {
	if {[lsearch -exact $result $dot] == -1} {
	    lappend result $dot
	}
    }
    return $result
}

# plot
proc plot {dots} {
    set maxx 0
    set maxy 0
    foreach dot $dots {
	set x [lindex $dot 0]
	set y [lindex $dot 1]
	if {$x > $maxx} {set maxx $x}
	if {$y > $maxy} {set maxy $y}
    }
    for {set y 0} {$y <= $maxy} {incr y} {
	for {set x 0} {$x <= $maxx} {incr x} {
	    set pos "$x $y"
	    if {[lsearch -exact $dots $pos] == -1} {
		puts -nonewline " "
	    } else {
		puts -nonewline "x"
	    }
	}
	puts ""
    }
}

# First problem
set first [lindex $folds 0]
set xy [lindex $first 0]
set pos [lindex $first 1]
set firstfold [fold $dots $xy $pos]
puts [llength $firstfold]

# Second problem
foreach fold $folds {
    set xy [lindex $fold 0]
    set pos [lindex $fold 1]
    set dots [fold $dots $xy $pos]
}
plot $dots

exit 0
