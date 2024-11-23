package require Tcl 8.5

namespace eval scwutil {
	variable command_metadata
	set loaded 0
    if { ![catch {load librdi_scw[info sharedlibextension]}] } { 
	set loaded 1
    } else {
	puts "Failed to load the librdi_scw shared library "

	}
  proc show_brief {result names} {
	variable command_metadata
	upvar $result res

	set maxlen 0
	foreach name $names {
	    set len [string length $name]
	    if { $maxlen < $len } {
		set maxlen $len
	    }
	}

	foreach name $names {
	    set brief {}
	    if { [dict exists $command_metadata $name brief] } {
		set brief [dict get $command_metadata $name brief]
	    }
	    set pad [string repeat " " [expr $maxlen - [string length $name]]]
	    append res "$name$pad - $brief\n"
	}
    }


 proc get_options {argListVar optList {strict 1}} {
	upvar 1 $argListVar arglist

	set result [dict create]
	foreach opt $optList {
	    set flags [lindex $opt 2]
	    if { [dict exists $flags default] } {
		dict set result [lindex $opt 0] [dict get $flags default]
	    } elseif { ![dict exists $flags args] || [dict get $flags args] == 0 } {
		dict set result [lindex $opt 0] 0
	    }
	}

	while { [string index [set arg [lindex $arglist 0]] 0] == "-" } {
	    if { $arg == "--" } {
		set arglist [lrange $arglist 1 end]
		break;
	    }

	    set matchnames {}
	    set matchopt {}
	    set arglen [string length $arg]
	    foreach opt $optList {
		set name -[lindex $opt 0]
		set flags [lindex $opt 2]
		if { [string equal -length $arglen $arg $name] } {
		    if { [dict exists $flags deprecated] && [dict get $flags deprecated] == 1} {
			puts "warning: $name is deprecated as it is not required, it will be removed in future"
		    }
		    set matchopt $opt
		    lappend matchnames $name
		}
	    }

	    set matchcount [llength $matchnames]
	    if { $matchcount != 1 } {
		if { $matchcount == 0 } {
		    set optnames {}
		    foreach opt $optList {
			set flags [lindex $opt 2]
			if { ![dict exists $flags deprecated] || [expr [dict exists $flags deprecated] && [dict get $flags deprecated] != 1] } {
			    lappend optnames -[lindex $opt 0]
			}
		    }
		    error "bad option '$arg': [join $optnames]"
		} else {
		    error "ambiguous option '$arg': [join $matchnames]"
		}
	    }

	    set nargs 0
	    set flags [lindex $matchopt 2]
	    if { [dict exists $flags args] } {
		set nargs [dict get $flags args]
	    }
	    if { $nargs == 0 } {
		set value 1
	    } elseif { [llength $arglist] <= $nargs && ![dict exists $flags optional] } {
		error "option [lindex $arglist 0] require $nargs arguments"
	    } elseif { $nargs == 1 } {
		set value [lindex $arglist 1]
	    } else {
		set value [lrange $arglist 1 $nargs]
	    }
	    dict set result [lindex $matchopt 0] $value
	    set arglist [lrange $arglist 1+$nargs end]
	}
	if { $strict && [llength $arglist] } {
	    error "unexpected arguments: $arglist"
	}
	return $result
    }
 proc setcmdmeta {cmd type data {cmd_prefix ""}} {
	variable command_metadata

	dict set command_metadata "$cmd_prefix$cmd" $type $data

	if { $type == "categories" } {
	    #if { $data == "scw" && [string first "xsct" [file tail [info nameofexecutable]]] == -1 } {
		#return
	    #}
	    dict set command_metadata commands category_commands "$cmd_prefix$cmd" 1
	    foreach category $data {
		dict set command_metadata $category category_commands "$cmd_prefix$cmd" 1
		dict set command_metadata commands categories $category 1
		dict set command_metadata categories categories $category 1
	    }
	}
    }
 proc getcmdmeta {cmd type args} {
	variable command_metadata

	if { [llength $args] > 1 } {
	    error "wrong # of args: should be \"getcmdmeta $cmd $type \[default-value\]\""
	}
	if { ![dict exists $command_metadata $cmd $type] } {
	    if { [llength $args] == 1 } {
		return [lindex $args 0]
	    }
	}
	return [dict get $command_metadata $cmd $type]
    }

   proc help {args} {
	variable command_metadata
	if { [llength $args] > 0 } {
	    set option [join $args]
	} else {
	    set option categories
	}
	set matches [lsearch -all -inline -glob [dict keys $command_metadata] "$option*"]
	if { [llength $matches] == 0 } {
	    error "unknown command or category \"$option\": must be [join [dict keys $command_metadata] {, }]"
	}

	set result ""
	if { [llength $matches] == 1 } {
	    set name [lindex $matches 0]
	} else {
	    set match [lsearch $matches $option]
	    if { $match < 0 } {
		append result "Matching commands and categories\n\n"
		show_brief result $matches
		return $result
	    }
	    set name [lindex $matches $match]
	}
	if { $name == "categories" } {
	    append result "Available Help Categories\n\n"
	    set categories [dict keys [dict get $command_metadata commands categories]]
	    #if { [string first "xsct" [file tail [info nameofexecutable]]] == -1 } {
		#set id [lsearch -exact $categories "sdk"]
		#set categories [lreplace $categories $id $id]
	    #}
	    show_brief result $categories
	    append result "\nType \"help\" followed by above \"category\" for more details or\n"
	    append result "help\" followed by the keyword \"commands\" to list all the commands\n"
	} elseif { [dict exists $command_metadata $name category_commands] } {
	    append result "Category commands\n\n"
	    show_brief result "[dict keys [dict get $command_metadata $name category_commands]]\n"
	    append result "\nType \"help\" followed by above \"command\", or the above \"command\" followed by\n\"-help\" for more details\n"
	} else {
	    append result "NAME\n    "
	    show_brief result "[list $name]\n"
	    if { [dict exists $command_metadata $name description] } {
		if { [dict exists $command_metadata $name description SYNOPSIS] } {
		    append result "\nSYNOPSIS"
		    append result "[dict get $command_metadata $name description SYNOPSIS]\n"
		} else {
		    append result "[dict get $command_metadata $name description]\n"
		}
		if { [dict exists $command_metadata $name description OPTIONS] } {
		    append result "OPTIONS"
		    append result "[dict get $command_metadata $name description OPTIONS]\n"
		}
		if { [dict exists $command_metadata $name description NOTE] } {
		    append result "NOTE"
		    append result "[dict get $command_metadata $name description NOTE]\n"
		}
		if { [dict exists $command_metadata $name description EXAMPLE] } {
		    append result "EXAMPLE"
		    append result "[dict get $command_metadata $name description EXAMPLE]\n"
		}
		if { [dict exists $command_metadata $name description RETURNS] } {
		    append result "RETURNS"
		    append result "[dict get $command_metadata $name description RETURNS]\n"
		}
	    }
	}
	return $result
    }
    namespace export help

    setcmdmeta categories brief {List Help Categories}
    setcmdmeta commands brief {List all Commands}
    setcmdmeta scw brief {List all SCW commands}

 
}
package provide scwutil 0.1
