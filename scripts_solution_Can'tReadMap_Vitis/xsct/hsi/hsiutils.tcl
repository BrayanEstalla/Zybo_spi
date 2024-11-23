#######################################################################
# Copyright (c) 2015-2018 Xilinx, Inc.  All rights reserved.
#
# This   document  contains  proprietary information  which   is
# protected by  copyright. All rights  are reserved. No  part of
# this  document may be photocopied, reproduced or translated to
# another  program  language  without  prior written  consent of
# XILINX Inc., San Jose, CA. 95124
#
# Xilinx, Inc.
# XILINX IS PROVIDING THIS DESIGN, CODE, OR INFORMATION "AS IS" AS A
# COURTESY TO YOU.  BY PROVIDING THIS DESIGN, CODE, OR INFORMATION AS
# ONE POSSIBLE   IMPLEMENTATION OF THIS FEATURE, APPLICATION OR
# STANDARD, XILINX IS MAKING NO REPRESENTATION THAT THIS IMPLEMENTATION
# IS FREE FROM ANY CLAIMS OF INFRINGEMENT, AND YOU ARE RESPONSIBLE
# FOR OBTAINING ANY RIGHTS YOU MAY REQUIRE FOR YOUR IMPLEMENTATION.
# XILINX EXPRESSLY DISCLAIMS ANY WARRANTY WHATSOEVER WITH RESPECT TO
# THE ADEQUACY OF THE IMPLEMENTATION, INCLUDING BUT NOT LIMITED TO
# ANY WARRANTIES OR REPRESENTATIONS THAT THIS IMPLEMENTATION IS FREE
# FROM CLAIMS OF INFRINGEMENT, IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS FOR A PARTICULAR PURPOSE.
#
#######################################################################

namespace eval ::hsi::utils {
    variable version 0.1
    variable sdk $env(XILINX_VITIS)
    variable rdidata $env(RDI_DATADIR)
    variable lgengine ""
    variable repopath ""
    variable repo_list ""
    variable repo_app_dict [dict create]

    #---------------------------------------------------------------------------------------#
    # Open Hardware Design
    # Description:  Opens the hardware design and updates the local database.
    #               Next time, it returns the already opened designed.
    # Arguments  :  Hardware project / Hardware Design (*.xsa)
    # Type	 :  XSCT command
    #---------------------------------------------------------------------------------------#
    proc openhw { args } {
	variable ::xsdb::designtable

	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]

	if { $params(help) } {
	    return [help [lindex [info level 0] 0]]
	}

	if { [llength $args] == 1 } {
	    set hdf [get_hw [lindex $args 0]]
	} else {
	    error "wrong # args: should be \"openhw <hw-proj | xsa-file>\""
	}

	set hdf [file normalize $hdf]

	if { ![dict exists $designtable $hdf] } {
	    set design [::hsi::open_hw_design $hdf -no_overwrite]

	    # If PS config file exists in the design directory
	    # Source the ps_config_params.tcl, which is generated when FSBL Config parameters change
	    set path [file dirname $hdf]/ps_config_params.tcl
	    if { [file exists $path] == 1 } {
		namespace eval ::hsi [list source $path]
	    }

	    # Generate PS INIT files, if not present
	    set configurablecell [ ::hsi::get_cells -filter { CONFIGURABLE == true } -hierarchical]
	    if { $configurablecell != "" } {
		set dirname [file dirname $hdf]
		if { [check_ps_init_files $dirname] == 0 } {
            if { [file writable $dirname] == 1 } {
                ::hsi::generate_target {psinit} $configurablecell -dir $dirname
            }
		}
	    }

	    ::hsi::current_hw_design [::hsi::get_hw_designs $design]
	    dict set designtable $hdf design $design
	} else {
	    set design [dict get $designtable $hdf design]
	    ::hsi::current_hw_design [::hsi::get_hw_designs $design]
	}
	return ""
    }
    namespace export openhw
    ::xsdb::setcmdmeta openhw categories {projects}
    ::xsdb::setcmdmeta openhw brief {Open a hardware design.}
    ::xsdb::setcmdmeta openhw description {
SYNOPSIS {
    openhw <hw-proj | xsa file>
        Open a hardware design exported from Vivado.
        XSA file exported from Vivado, or the hardware project created
	using 'createhw' command can be passed as argument.
}
OPTIONS {
    None
}
RETURNS {
    If successful, this command returns nothing.
    Otherwise it returns an error.
}
EXAMPLE {
    openhw ZC702_hw_platform
        Open the hardware project ZC702_hw_platform.

    openhw /tmp/wrk/hw1/system.xsa
        Open the hardware project corresponding to the system.xsa.
}
}

    #---------------------------------------------------------------------------------------#
    # Close Hardware Design
    # Description:  Closes the hardware design and updates the local database.
    # Arguments  :  Hardware project / Hardware Design (*.xsa)
    # Type	 :  XSCT command
    #---------------------------------------------------------------------------------------#
    proc closehw { args } {
	variable ::xsdb::designtable

	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]

	if { $params(help) } {
	    return [help [lindex [info level 0] 0]]
	}

	if { [llength $args] == 1 } {
	    set hdf [get_hw [lindex $args 0]]
	} else {
	    error "wrong # args: should be \"closehw <hw-proj | xsa file>\""
	}

	set hdf [file normalize $hdf]

	if { [dict exists $designtable $hdf] } {
	    set design [dict get $designtable $hdf design]
	    ::hsi::close_hw_design $design
	    set designtable [ dict remove $designtable $hdf]
	} else {
	    error "Cannot close hw design \'$hdf\'.\nDesign is not opened in the current session.\n"
	}
	return
    }
    namespace export closehw
    ::xsdb::setcmdmeta closehw categories {projects}
    ::xsdb::setcmdmeta closehw brief {Close a hardware design.}
    ::xsdb::setcmdmeta closehw description {
SYNOPSIS {
    closehw <hw project | xsa file>
        Close a hardware design that was opened using 'openhw' command.
        XSA file exported from Vivado, or the hardware project created
	using 'createhw' command can be passed as argument.
}
OPTIONS {
    None.
}
RETURNS {
    If successful, this command returns nothing.
    Otherwise it returns an error.
}
EXAMPLE {
    closehw ZC702_hw_platform
       Close the hardware project ZC702_hw_platform.

    closehw /tmp/wrk/hw1/system.xsa
       Close the hardware project corresponding to the system.xsa.
}
}

    #---------------------------------------------------------------------------------------#
    # Open BSP
    # Description:  Opens the BSP and updates the local database.
    # Arguments  :  BSP project / Software Design (*.mss)
    # Type	 :  XSCT command
    #---------------------------------------------------------------------------------------#
    proc openbsp { args } {
	puts "\nNote:: \"openbsp\" command is deprecated"
	return
    }
    namespace export openbsp

    proc do_prebuild_one { hw targetmss processor os targetdir appTemplate } {
	
	set bspdir [file dirname $targetmss]
	openhw $hw
	hsi create_sw_design -name $targetmss -proc $processor -os $os -app $appTemplate
	hsi write_mss -name system -dir $targetdir
	hsi close_sw_design [ hsi current_sw_design]    
	::hsi::utils::generate_bsp_sources $hw $targetmss $bspdir
	::hsi::utils::closesw $targetmss
	hsi create_sw_design -name $targetmss -proc $processor -os $os -app $appTemplate
	hsi write_mss -name system -dir $targetdir -force
	hsi close_sw_design [ hsi current_sw_design]	
    }
    proc do_prebuild_two { hw targetmss processor os targetdir appTemplate  } {
	set bspdir [file dirname $targetmss]
	openhw $hw
	hsi create_sw_design -name $targetmss -proc $processor -os $os -app $appTemplate
	hsi write_mss -name system -dir $targetdir
	hsi close_sw_design [ hsi current_sw_design]    
	::hsi::utils::generate_bsp_sources $hw $targetmss $bspdir
	::hsi::utils::closesw $targetmss
	hsi create_sw_design -name $targetmss -proc $processor -os $os -app $appTemplate
	hsi write_mss -name system -dir $targetdir -force
	hsi close_sw_design [ hsi current_sw_design] 
    }
    proc do_prebuild_three { hw targetmss processor os targetdir appTemplate } {
	openhw $hw
	hsi create_sw_design -name $targetmss -proc $processor -os $os -app $appTemplate
	hsi write_mss -name defaultsystem -dir $targetdir
	hsi close_sw_design [ hsi current_sw_design]
    }
    proc create_dt_mss { args } {
	if { [llength $args] == 2 } {
	    set hwfile [lindex $args 0]
	    set outdir [lindex $args 1]
	    set mssfile "$outdir/system.mss"
	    if { ![file exists $hwfile] } {
		    error "hw specification file is not existing."
	    }
	    if { ![file isdirectory $outdir] } {
		    error "output directory is not existing."
	    }
	    # Check if the device_tree core is present.
	    set swcores [hsi get_sw_cores device_tree]
	    if { $swcores == ""  } {
		    error "device_tree is not part of the Repository. Please add the repository having device_tree."
	    }
	    set target_proc [get_dt_target $hwfile]			
    
	    if { [file exists $mssfile] } {
		    opensw $mssfile
	    } else {
		    hsi create_sw_design temp_system -proc $target_proc -os device_tree
		    ::hsi::write_mss -name system -dir $outdir
		    ::hsi::close_sw_design [::hsi::current_sw_design]
		    opensw $outdir/system.mss
	    }
	} else {
		error "wrong # args: should be \"create_dt_mss <xsa file> <output dir>\""
	}
    }
    proc get_dt_target { args } {
	if { [llength $args] == 1 } {
	    set hwfile [lindex $args 0]
	    openhw $hwfile
	    set iplist [hsi get_cells -filter "IP_TYPE == PROCESSOR" -hierarchical]
	    set target_proc ""	
	    if { [lsearch $iplist "psu_cortexa53_0"] != -1 } {
		set target_proc "psu_cortexa53_0"
	    }
	    if { [lsearch $iplist "ps7_cortexa9_0"] != -1 } {
		set target_proc "ps7_cortexa9_0"
	    }
	    if { [lsearch $iplist "psu_cortexa72_0"] != -1 } {
		set target_proc "psu_cortexa72_0"
	    }	    
	    if { [lsearch $iplist "psv_cortexa72_0"] != -1 } {
		set target_proc "psv_cortexa72_0"
	    }
	    if { [lsearch $iplist "*psxl_cortexa78_0"] != -1 } {
		set target_proc "psxl_cortexa78_0"
	    }
	    if { [lsearch $iplist "*psx_cortexa78_0"] != -1 } {
		set target_proc "psx_cortexa78_0"
	    }
            
	    if { $target_proc == "" } {
		set a72_proc "psv_cortexa72_0"
		set hier_proc [::scw::get_processor_name $a72_proc $hwfile]
		if { [lsearch $iplist $hier_proc] != -1 } {
		    set target_proc $hier_proc
		}
	    }
	    return $target_proc		
	} else {
	    error "wrong # args: should be \"get_dt_target <xsa file> \""
	}
    }
    proc generate_dt { args } {
	if { [llength $args] == 2 } {
		set hwfile [lindex $args 0]
		set outdir [lindex $args 1]			
		create_dt_mss $hwfile $outdir			
		::hsi::generate_bsp -dir $outdir			
		
	} else {
		error "wrong # args: should be \"generate_dt <xsa file> <output dir>\""
	}
    }
    proc get_linker_flags { args } {
	if { [llength $args] == 2 } {
	    set hwfile  [lindex $args 0]
	    set mssfile [lindex $args 1]
	    openhw $hwfile
	    opensw $mssfile
	    set osname [hsi get_property NAME [ hsi get_os ]]
	    set osver [hsi get_property VERSION [ hsi get_os ]]			
	    set oscore [hsi get_sw_cores  -filter "NAME==$osname &&  VERSION==$osver"]
	    set os_linker_flags [hsi get_property APP_LINKER_FLAGS [ hsi get_sw_cores $oscore ]]
	    
	    set ret_flags $os_linker_flags
	    
	    set liblist [::hsi::get_libs]
	    foreach lib $liblist {				
		    set libver [hsi get_property VERSION [ hsi get_libs $lib ]]			
		    set libcore [hsi get_sw_cores  -filter "NAME==$lib &&  VERSION==$libver"]
		    set lib_linker_flags [hsi get_property APP_LINKER_FLAGS [ hsi get_sw_cores $libcore ]]
		    set ret_flags "$ret_flags $lib_linker_flags"
	    }			
	    return $ret_flags			
		
	} else {
	    error "wrong # args: should be \"get_linker_flags <xsa file> <mss file>\""
	}			
    }
    #---------------------------------------------------------------------------------------#
    # Close BSP
    # Description:  Closes the BSP and updates the local database.
    # Arguments  :  Software Design (*.mss)
    # Type	 :  XSCT command
    #---------------------------------------------------------------------------------------#
    proc closebsp { args } {
	puts "\nNote:: \"closebsp\" command is deprecated"
	return
    }
    namespace export closebsp

    #---------------------------------------------------------------------------------------#
    # Writes updated sw design to MSS
    # Description: Opens sw and hw designs and write the corresponding MSS
    # Arguments  : HW and SW Design Dir Path.
    # Type	 : XSCT command
    #---------------------------------------------------------------------------------------#
    proc updatemss { args } {
	puts "\nNote:: \"updatemss\" command is deprecated"
	set options {
	    {help "command help"}
	}
	#array set params [::xsdb::get_options args $options]
	#
	#if { $params(help) } {
	#    return [help [lindex [info level 0] 0]]
	#}
	#
	#if { ![info exists params(mss)] } {
	#    error "mss file not specified"
	#}
	#
	#::sdk::check_sdk_workspace
	#set params(hw) [find_hw $params(mss)]
	#update_mss -hw $params(hw) -mss $params(mss)
	return [::scw::platform write]
    }
    namespace export updatemss

    #---------------------------------------------------------------------------------------#
    # Get Address Ranges
    # Description: Returns the address ranges w.r.t. a processor instance
    # Arguments  : Hw Design (HDF), Processor Instance
    #              -json (Output in JSON format)
    #---------------------------------------------------------------------------------------#
    proc getaddrmap { args } {
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]
	if { $params(help) } {
	    return [help [lindex [info level 0] 0]]
	}
	if { [llength $args] == 2 } {
	    set hw_design [get_hw [lindex $args 0]]
	    set proc_instance [lindex $args 1]
	} else {
	    error "wrong # args: should be \"getaddrmap <hw-proj> <proc-instance>\""
	}

	::sdk::check_sdk_workspace
	return [get_addr_ranges $hw_design $proc_instance]
    }
    namespace export getaddrmap
    ::xsdb::setcmdmeta getaddrmap categories {projects}
    ::xsdb::setcmdmeta getaddrmap brief {Get the address ranges of IP connected to processor.}
    ::xsdb::setcmdmeta getaddrmap description {
SYNOPSIS {
    getaddrmap <hw spec file> <processor-instance>
        Return the address ranges of all the IP connected to the processor in a
	tabular format, along with details like size and access flags of all IP.
}
OPTIONS {
    None.
}
RETURNS {
    If successful, this command returns the output of IPs and ranges.
    Otherwise it returns an error.
}
EXAMPLE {
    getaddrmap system.xsa ps7_cortexa9_0
        Return the address map of peripherals connected to ps7_cortexa9_0.
        system.xsa is the hw specification file exported from Vivado.
}
}

    #---------------------------------------------------------------------------------------#
    # Get All Peripherals
    # Description: Returns all the peripherals and their properties in a design
    # Arguments : Hw Design (HDF)
    # Type	: XSCT Command
    #---------------------------------------------------------------------------------------#
    proc getperipherals { args } {
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]
	if { $params(help) } {
	    return [help [lindex [info level 0] 0]]
	}
	set processor ""
	if { [llength $args] == 1} {
	    set hw_design [lindex $args 0]
	} elseif { [llength $args] == 2 } {
	    set hw_design [lindex $args 0]
	    set processor [lindex $args 1]
	} else {
	    error "wrong # args: should be \"getperipherals <hw-proj> <processor-instance>\""
	}

	::sdk::check_sdk_workspace
	set hw_design [get_hw $hw_design]
	return [get_all_periphs $hw_design $processor]
    }
    namespace export getperipherals
    ::xsdb::setcmdmeta getperipherals categories {projects}
    ::xsdb::setcmdmeta getperipherals brief {Get a list of all peripherals in the HW design}
    ::xsdb::setcmdmeta getperipherals description {
SYNOPSIS {
    getperipherals <xsa> <processor-instance>
        Return the list of all the peripherals in the hardware design, along
        with version and type. If [processor-instance] is specified, return
        only a list of slave peripherals connected to that processor.
}
OPTIONS {
    None.
}
RETURNS {
    If successful, this command returns the list of peripherals.
    Otherwise it returns an error.
}
EXAMPLE {
    getperipherals system.xsa
        Return a list of peripherals in the hardware design.

    getperipherals system.xsa ps7_cortexa9_0
        Return a list of peripherals connected to processor ps7_cortexa9_0 in
        the hardware design.
}
}

    #---------------------------------------------------------------------------------------#
    # Get All Processors
    # Description: Returns the list of all processors in a design
    # Arguments : Hw Design (XSA)
    # Type	: XSCT Command
    #---------------------------------------------------------------------------------------#
    proc getprocessors { args } {
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]
	if { $params(help) } {
	    return [help [lindex [info level 0] 0]]
	}
	if { [llength $args] == 1} {
	    set hw_design [lindex $args 0]
	} else {
	    error "wrong # args: should be \"getprocessors <xsa>\""
	}

	openhw $hw_design
	set proc_cells [::hsi::get_cells -filter {IP_TYPE == "PROCESSOR"} -hierarchical]
	set proc_list [list]
	foreach cell $proc_cells {
	    lappend proc_list [hsi get_property name $cell]
	}
	
	# check for aiengine in IP and add it processors list
	foreach cell [::hsi::get_cells -hierarchical] {
            if { [hsi get_property IP_NAME $cell] == "ai_engine" } {
		lappend proc_list "ai_engine"
	    }
        }
	return $proc_list
    }
    namespace export getprocessors
    ::xsdb::setcmdmeta getprocessors categories {projects}
    ::xsdb::setcmdmeta getprocessors brief {Get a list of all processors in the hardware design.}
    ::xsdb::setcmdmeta getprocessors description {
SYNOPSIS {
    getprocessors <xsa>
        Return the list of all the processors in the hardware design
}
OPTIONS {
    None.
}
RETURNS {
    If successful, this command returns the list of processors.
    Otherwise, it returns an error.
}
EXAMPLE {
    getprocessors system.xsa
        Return a list of processors in the hardware design.
}
}

    #---------------------------------------------------------------------------------------#
    # Repository
    # Description: Repository functionality
    # Arguments  : Repository path
    # Type 	 : XSCT Command
    #---------------------------------------------------------------------------------------#
    proc repo { args } {
	variable repopath
	set options {
	    {set "set repo path" {args 1}}
	    {add-platforms "add platforms to repo" {args 1}}
	    {remove-platforms-dir "remove the directory from platforms repos" {args 1}}
	    {list-platforms-dir "get platform repository directories"}
	    {get "get repo path"}
	    {platform-info "platform info" {args 1}}
	    {scan "scan the repo"}
	    {os "get os list from repo"}
	    {drivers "get driver list from repo"}
	    {platforms "get platforms list from repo"}
	    {libs "get library list from repo"}
	    {apps "get application templates from repo"}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]
	if { $params(help) } {
	    return [help [lindex [info level 0] 0]]
	}
	if { !([info exists params(set)] || $params(get) || $params(scan)) && !($params(os) || \
	       [info exists params(add-platforms)] || $params(drivers) ||  $params(platforms) || \
	       $params(libs) || $params(apps) || [info exists params(platform-info)] || \
			[info exists params(remove-platforms-dir)] || $params(list-platforms-dir)) } {
	    error "Wrong # args: should be \"repo \[options\]\""
	}

	if { [info exists params(set)] + [info exists params(platform-info)] +  [info exists params(remove-platforms-dir)] + \
		 $params(list-platforms-dir) + $params(scan) + \
	     $params(get) + $params(os) + $params(drivers) + $params(libs) + $params(apps)> 1 } {
	    error "conflicting options specified, use only one of -set, -get, -scan, -os, -drivers, -libs, -platform-info \
		, -list-platforms-dir, -remove-platforms-dir, -platforms or -apps"
	}	
	if { [info exists params(set)] } {
	    return [::sdk::set_user_repo_path_sdk $params(set)]
	} elseif { $params(get) } {
	    return [::sdk::get_user_repo_path_sdk]
	} elseif { $params(scan) } {
	    return [init_repo]
	} elseif { $params(os) } {
	    return [get_repo_os]
	} elseif { $params(libs) } {
	    return [get_all_libs]
	} elseif { $params(drivers) }  {
	    return [get_repo_drivers]
	} elseif { $params(apps) } {
	    if { [ llength $args ] == 1 } {                
                return [get_apps_for_platform [lindex $args 0 ]]
            } else {
		return [get_all_app_details]
	    }
	} elseif { $params(platforms) } {
	    return [::sdk::getplatforms]
	} elseif { [info exists params(add-platforms)] } {
	    return [::sdk::addplatforms $params(add-platforms)]
	} elseif { [info exists params(platform-info)] } {
	    return [::sdk::reportplatform $params(platform-info)]
	} elseif { $params(list-platforms-dir) } {
	    return [::sdk::list_platforms_dir]
	} elseif { [info exists params(remove-platforms-dir)] } {
	    return [::sdk::remove_platforms_dir $params(remove-platforms-dir)]
	}
    }
    namespace export repo
    ::xsdb::setcmdmeta repo categories {projects}
    ::xsdb::setcmdmeta repo brief {Get, set, or modify software repositories}
    ::xsdb::setcmdmeta repo description {
SYNOPSIS {
    repo [OPTIONS]
        Get/set the software repositories path currently used.
        This command is used to scan the repositories, to get the list
        of OS/libs/drivers/apps from repository.
}
OPTIONS {
    -set <path-list>
        Set the repository path and load all the software cores available.
        Multiple repository paths can be specified as Tcl list.

    -get
        Get the repository path(s).

    -scan
        Scan the repositories.
        Used this option to scan the repositories, when some changes are done.

    -os
        Return a list of all the OS from the repositories.

    -libs
        Return a list of all the libs from the repositories.

    -drivers
        Return a list of all the drivers from the repositories.

    -apps
       	Return a list of all the applications from repositories along with the following details.
            Supported processor - Processors for which the application can be built.
            Supported OS        - OS for which the application can be built.
            Platform required   - Indicates whether a platform is required to create the application.
                                  AIE applications need a platform while other applications can be 
                                  created using a platform or xsa.

    -add-platforms <platforms directory>
        Add the specified directory to the platform repository.

    -remove-platforms-dir  <platforms directory>
        Remove the specified directory from the platform repository.
}
RETURNS {
    Depends on the OPTIONS specified.

    -scan, -set
        Returns nothing.

    -get
        Returns the current repository path.

    -os, -libs, -drivers, -apps
        Returns the list of OS/libs/drivers/apps respectively.

}
EXAMPLE {
    repo -set <repo-path>
        Set the repository path to the path specified by <repo-path>.

    repo -os
        Return a list of OS from the repo.

    repo -libs
        Return a list of libraries from the repo.
}
}

    #---------------------------------------------------------------------------------------#
    # Configure BSP
    # Description: BSP configuration parameters
    # Arguments  : Hardware Design (XSA), Software Design (MSS) and other options
    # Type	 : XSCT Command
    #---------------------------------------------------------------------------------------#
    proc configbsp { args } {
	puts "\nNote:: \"configbsp\" command is deprecated. Use \"bsp config\" command"
	set options {
	    {hw "hardware project" {deprecated 1 args 1}}
	    {bsp "software design" {args 1}}
	    {os "config params of os"}
	    {proc "config params of processor"}
	    {lib "config params of library" {args 1}}
	    {append "append parameter values"}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]
	if { $params(help) } {
	    return [help [lindex [info level 0] 0]]
	}

	if { ![info exists params(bsp)] } {
	    error "bsp not specified"
	}

	# Check for valid options
	set argsspecified 0
	if { [llength $args] > 0 } {
	    set argsspecified 1
	}
	if { $params(proc) + [info exists params(lib)] + $params(os) + $argsspecified > 1 } {
	    error "conflicting options specified, use only one of -proc, -lib, -os or <parameter> <value>"
	}

	::sdk::check_sdk_workspace

	find_hw $params(bsp)

	if { [llength $args] == 2 } {
	    set parameter [lindex $args 0]
	    set value [lindex $args 1]
	    set current_val $value
	    if { $params(append) } {
		set current_val [::hsi::get_property VALUE [::hsi::get_comp_params -filter "NAME == $parameter"]]
		append current_val " $value"
	    }
	    # Set the config property
	    builtin_bsp -config {*}[list "-option" $parameter "-value" $current_val]
	} elseif { [llength $args] == 1 } {
	    if { $params(append) } {
		error "Wrong # args: should be \"configbsp \[options\] \[<parameter> \[<value>\]\]\""
	    }
	    set parameter [lindex $args 0]
	    set paramlist [::hsi::get_comp_params]
	    if { [lsearch $paramlist $parameter] == -1 } {
		error "$parameter not found in the BSP"
	    }
	    return [builtin_bsp -config {*}[list "-option" $parameter]]
	} else {
	    if { $params(proc) } {
		# For Processor Config parameters
		return [::scw::bsp listparams -proc]
	    } elseif { $params(os) } {
		# For OS Config parameters
		return [::scw::bsp listparams -os]
	    } elseif { [info exists params(lib)] } {
		# For Library Config parameters
		set liblist [::hsi::get_libs]
		if { [lsearch $liblist $params(lib)] == -1 } {
		    error "$params(lib) not found in the BSP"
		}
		return [::scw::bsp listparams -os]
	    } else {
		error "wrong # args: specify atleast one of -proc, -lib, -os, \n\<parameter> or <parameter> <value>"
	    }
	}
    }
    namespace export configbsp

    #---------------------------------------------------------------------------------------#
    # Set Libraries
    # Description: Get Libraries
    # Arguments  : Hardware Design (XSA), Software Design (MSS), Library, Version
    # Type	 : XSCT Command
    #---------------------------------------------------------------------------------------#
    proc setlib { args } {
	puts "\nNote:: \"setlib\" command is deprecated. Use \"bsp setlib\" command"
	set options {
	    {hw "hardware project" {deprecated 1 args 1}}
	    {bsp "software project" {args 1}}
	    {lib "library name" {args 1}}
	    {ver "library version" {default "latest" args 1}}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options]
	if { $params(help) } {
	    return [help [lindex [info level 0] 0]]
	}

	if { ![info exists params(bsp)] } {
	    error "bsp not specified"
	}
	if { ![info exists params(lib)] } {
	    error "library name not specified"
	}

	::sdk::check_sdk_workspace
	set platform [find_hw $params(bsp)]
	
	return [::scw::bsp setlib -name $params(lib) -ver $params(ver)]
    }
    namespace export setlib

    #---------------------------------------------------------------------------------------#
    # Delete Libraries
    # Description: Delete Libraries
    # Arguments  : Hardware Design (XSA), Software Design (MSS), Library
    # Type	 : XSCT Command
    #---------------------------------------------------------------------------------------#
    proc removelib { args } {
	puts "\nNote:: \"removelib\" command is deprecated. Use \"bsp removelib\" command"
	set options {
	    {hw "hardware project" {deprecated 1 args 1}}
	    {bsp "software project" {args 1}}
	    {lib "library name" {args 1}}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options]
	if { $params(help) } {
	    return [help [lindex [info level 0] 0]]
	}

	if { ![info exists params(bsp)] } {
	    error "bsp not specified"
	}
	if { ![info exists params(lib)] } {
	    error "library name not specified"
	}

	::sdk::check_sdk_workspace
	find_hw $params(bsp)
	
	set liblist [hsi get_libs]
	if { [lsearch $liblist $params(lib)] == -1 } {
	    error "$params(lib) library not found in BSP"
	}
	return [::scw::bsp removelib -name $params(lib)]
    }
    namespace export removelib

    #---------------------------------------------------------------------------------------#
    # Get Libraries
    # Description: Get Libraries
    # Arguments  : Hardware Design (XSA), Software Design (MSS)
    # Type	 : XSCT Command
    #---------------------------------------------------------------------------------------#
    proc getlibs { args } {
	puts "\nNote:: \"getlibs\" command is deprecated. Use \"bsp getlibs\" command"
	set options {
	    {hw "hardware project" {deprecated 1 args 1}}
	    {bsp "software design" {args 1}}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]
	if { $params(help) } {
	    return [help [lindex [info level 0] 0]]
	}

	if { ![info exists params(hw)] || ![info exists params(bsp)] } {
	    if { [llength $args] == 2 } {
		puts "warning: specifying hw project is deprecated as it is not required, it will be removed in future"
		set params(bsp) [get_sw [lindex $args 1]]
		set args [lrange $args 2 end]
	    }
	}

	if { [llength $args] > 0 } {
	    error "wrong # args: should be \"getlibs \[OPTIONS\]\""
	}
	if { ![info exists params(bsp)] } {
	    error "bsp not specified"
	}

	::sdk::check_sdk_workspace
	find_hw $params(bsp)
	return [::scw::bsp getlibs]
    }
    namespace export getlibs

    #---------------------------------------------------------------------------------------#
    # Set Drivers
    # Description: Set Drivers
    # Arguments  : Hardware Design (XSA), Software Design (MSS), IP, Driver, Version
    # Type	 : XSCT Command
    #---------------------------------------------------------------------------------------#
    proc setdriver { args } {
	puts "\nNote:: \"setdriver\" command is deprecated. Use \"bsp setdriver\" command"
	set options {
	    {hw "hardware project" {deprecated 1 args 1}}
	    {bsp "software project" {args 1}}
	    {ip "peripheral" {args 1}}
	    {driver "driver name" {args 1}}
	    {ver "driver version" {default "latest" args 1}}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options]
	if { $params(help) } {
	    return [help [lindex [info level 0] 0]]
	}

	if { ![info exists params(bsp)] } {
	    error "bsp not specified"
	}
	if { ![info exists params(ip)] } {
	    error "ip name not specified"
	}

	::sdk::check_sdk_workspace
	find_hw $params(bsp)
	return [::scw::bsp setdriver -ip $params(ip) -driver $params(driver) -ver $params(ver)]
    }
    namespace export setdriver

    #---------------------------------------------------------------------------------------#
    # Get Drivers
    # Description: Get Drivers
    # Arguments  : Hardware Design (HDF/XML), Software Design (MSS)
    # Type	 : XSCT Command
    #---------------------------------------------------------------------------------------#
    proc getdrivers { args } {
	puts "\nNote:: \"getdrivers\" command is deprecated. Use \"bsp getdrivers\" command"
	set options {
	    {hw "hardware project" {deprecated 1 args 1}}
	    {bsp "software project" {args 1}}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]
	if { $params(help) } {
	    return [help [lindex [info level 0] 0]]
	}

	if { ![info exists params(hw)] || ![info exists params(bsp)] } {
	    if { [llength $args] == 2 } {
		puts "warning: specifying hw project is deprecated as it is not required, it will be removed in future"
		set params(bsp) [get_sw [lindex $args 1]]
		set args [lrange $args 2 end]
	    }
	}
	if { [llength $args] != 0 } {
	    error "wrong # args: should be \"getdrivers \[OPTIONS\]\""
	}

	if { ![info exists params(bsp)] } {
	    error "bsp not specified"
	}

	::sdk::check_sdk_workspace
	find_hw $params(bsp)
	return [::scw::bsp getdrivers]
    }
    namespace export getdrivers

    #---------------------------------------------------------------------------------------#
    # Set OS Version
    # Description: Set OS Version
    # Arguments  : Hardware Design (HDF/XML), Software Design (MSS), OS, Version
    # Type	 : XSCT Command
    #---------------------------------------------------------------------------------------#
    proc setosversion { args } {
	puts "\nNote:: \"setosversion\" command is deprecated. Use \"bsp setosversion\" command"
	set options {
	    {hw "hardware project" {deprecated 1 args 1}}
	    {bsp "software project" {args 1}}
	    {ver "os version" {default "latest" args 1}}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options]
	if { $params(help) } {
	    return [help [lindex [info level 0] 0]]
	}

	if { ![info exists params(bsp)] } {
	    error "bsp not specified"
	}

	# Get the latest version for OS
	::sdk::check_sdk_workspace
	if { $params(ver) == "latest" || $params(ver) == ""} {
	    set osdict [get_repo_os -dict]
	    dict for {os osdetails} $osdict {
		if { $params(os) == [dict get $osdetails name] } {
		    set params(ver) [dict get $osdetails version]
		}
	    }
	}

	find_hw $params(bsp)
	return [::scw::bsp setosversion -ver $params(ver)]
    }
    namespace export setosversion

    #---------------------------------------------------------------------------------------#
    # Get OS
    # Description: Get OS
    # Arguments  : Hardware Design (HDF/XML), Software Design (MSS)
    # Type	 : XSCT Command
    #---------------------------------------------------------------------------------------#
    proc getos { args } {
	puts "\nNote:: \"getos\" command is deprecated. Use \"bsp getos\" command"
	set options {
	    {hw "hardware project" {deprecated 1 args 1}}
	    {bsp "software project" {args 1}}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]
	if { $params(help) } {
	    return [help [lindex [info level 0] 0]]
	}

	if { ![info exists params(hw)] || ![info exists params(bsp)] } {
	    if { [llength $args] == 2 } {
		puts "warning: specifying hw project is deprecated as it is not required, it will be removed in future"
		set params(bsp) [get_sw [lindex $args 1]]
		set args [lrange $args 2 end]
	    }
	}
	if { [llength $args] != 0 } {
	    error "wrong # args: should be \"getos \[OPTIONS\]\""
	}
	if { ![info exists params(bsp)] } {
	    error "bsp not specified"
	}

	::sdk::check_sdk_workspace
	find_hw $params(bsp)
	return [::scw::bsp getos]
    }
    namespace export getos

    #---------------------------------------------------------------------------------------#
    # Regenerate BSP
    # Description: Regenerate BSP
    # Arguments  : HW Design, SW Design
    #---------------------------------------------------------------------------------------#
    proc regenbsp { args } {
	puts "\nNote:: \"regenbsp\" command is deprecated. Use \"platform generate\" command"
	set options {
	    {hw "hardware project" {deprecated 1 args 1}}
	    {bsp "software project" {args 1}}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options]
	if { $params(help) } {
	    return [help [lindex [info level 0] 0]]
	}

	if { ![info exists params(bsp)] } {
	    error "bsp not specified"
	}

	::sdk::check_sdk_workspace
	find_hw $params(bsp)
	return [::scw::platform generate]
    }
    namespace export regenbsp

    #---------------------------------------------------------------------------------------#
    # Get Hardware File
    # Description:  Returns the hdf file from the corresponding hw project
    # Arguments  :  hwproj / hdf file
    #---------------------------------------------------------------------------------------#
    proc get_hw { hw } {
	set valid_extns [list ".hdf" ".dsa" ".xsa" ".xml"]
	if { [file exists $hw] } {
	    if { [lsearch $valid_extns [file extension $hw]] == -1 } {
		error "invalid hardware specification file $hw"
	    } else {
		return $hw
	    }
	} else {
	    if { [lsearch [::sdk::getprojects] $hw] == -1 } {
		error "$hw is not available in the current workspace\n\use 'getprojects' command to see list of available projects in current workspace"
	    }
	    set hw_spec [glob -nocomplain -directory [file join [::sdk::getws] "$hw"] -type f system*[join $valid_extns ","]]
	    if { $hw_spec == "" } {
		set hw_spec [glob -nocomplain -directory [file join [::sdk::getws] "$hw/export/$hw/hw"] -type f *[join $valid_extns ","]]
		if { $hw_spec == "" } {
		    error "unable to find hardware specification file in $hw"
		}
	    }
	    return $hw_spec
	}
    }

    #---------------------------------------------------------------------------------------#
    # Get Software File
    # Description:  Returns the mss file from the corresponding sw project
    # Arguments  :  bsp proj / mss file
    #---------------------------------------------------------------------------------------#
    proc get_sw { sw } {
	if { [file extension $sw] != ".mss"} {
	    if { [lsearch [::sdk::getprojects] $sw] == -1 } {
		error "$sw is not available in the current workspace\n\use 'getprojects' command to see list of available projects in current workspace"
	    }
	    return [file join [::sdk::getws] "$sw/system.mss"]
	} else {
	    error "bsp-proj is not supported, provide mss file"
	}
    }

    #---------------------------------------------------------------------------------------#
    # Find Hardware Platform
    # Description:  Returns the hw platform name from the corresponding sw project
    # Arguments  :  bsp proj / mss file
    #---------------------------------------------------------------------------------------#
    proc find_hw { sw } {
	set chan [sdk::getsdkchan]
		    
	set user_list [sdk::xsdk_eval $chan "XSDx" getProjects "o{[dict create Type s]}" eA [list [dict create Type "platform"]]]
	if { [lindex $user_list 0] == "" } {
	    set user_list [lrange $user_list 1 end]
	}
	if { [lindex $user_list 0] != "" } {
	    set user_projs [split [lindex $user_list 0] ";"]
	} else {
	    error "$sw is not available in the current workspace"
	}
	
	foreach proj $user_projs {
	    builtin_platform -active $proj
	    set syslist [builtin_system -list]
	    if { $syslist == "" } {
		error "$sw is not available in the current workspace"
	    }
	    set sys_dict [::json::json2dict $syslist]
	    dict for {skey svalue} $sys_dict {
		builtin_system -active $skey
		set domlist [builtin_domain -list]
		if { $domlist == "" } {
		    error "$sw is not available in the current workspace"
		}
		set dom_dict [::json::json2dict $domlist]
		dict for {dkey dvalue} $dom_dict {
		    builtin_domain -active $dkey
		    if { $sw == $dkey } {
			return $proj
		    }
		}
	    }
	}
	error "Could not find HW project corresponding to $sw"
    }

    #---------------------------------------------------------------------------------------#
    # Open Software Design
    # Description:  Opens the software design and updates the local database.
    #               Next time, it returns the already opened designed.
    # Arguments  :  Software Design (*.mss)
    #---------------------------------------------------------------------------------------#
    proc opensw { args } {
	variable ::xsdb::swdesignmaps

	set mss [lindex $args 0]
	set mss [file normalize $mss]

	if { ![dict exists $swdesignmaps $mss] } {
	    set design [::hsi::open_sw_design $mss]
	    ::hsi::current_sw_design [::hsi::get_sw_designs $design]
	    dict set swdesignmaps $mss design $design
	} else {
	    set design [dict get $swdesignmaps $mss design]
	    ::hsi::current_sw_design [::hsi::get_sw_designs $design]
	}
	return ""
    }

    #---------------------------------------------------------------------------------------#
    # Close Software Design
    # Description:  Closes the software design and updates the local database.
    # Arguments  :  Software Design (*.mss)
    #---------------------------------------------------------------------------------------#
    proc closesw { args } {
	variable ::xsdb::swdesignmaps

	set mss [lindex $args 0]
	set mss [file normalize $mss]

	if { [dict exists $swdesignmaps $mss] } {
	    set design [dict get $swdesignmaps $mss design]
	    ::hsi::close_sw_design $design
	    set swdesignmaps [dict remove $swdesignmaps $mss]
	} else {
	    error "Cannot close sw design \'$mss\'.\nDesign is not opened in the current session.\n\n"
	}
	return ""
    }

    #---------------------------------------------------------#
    # Check if any of the PS init files exists
    #---------------------------------------------------------#
    proc check_ps_init_files { dirname } {
	set ps_files [::hsi::get_hw_files -filter {NAME=~*_init.tcl || NAME=~*_init.c || NAME=~*_init.h}]
	foreach filepath $ps_files {
	    set filepath [file normalize $dirname/$filepath]
	    if { [file exists $filepath] != 1 } {
		return 0
	    }
	}
	return 1
    }

    #---------------------------------------------------------------------------------------#
    # Set Hardware Software Design
    # Description:  Opens the hardware & software design and updates the local database.
    #               Next time, it returns the already opened designed.
    # Arguments  :  Hardware Design (*hdf/*xml), Software Design (*.mss)
    #---------------------------------------------------------------------------------------#
    proc set_current_hw_sw { args } {
	set hdfpath [lindex $args 0]
	set msspath [lindex $args 1]

	openhw $hdfpath
	opensw $msspath
    }

    #---------------------------------------------------------------------------------------#
    # Get all open designs
    # Description:  Lists all the opened HW & SW designs
    # Arguments  :
    #---------------------------------------------------------------------------------------#
    proc get_all_open_designs { } {
	variable ::xsdb::designtable
	variable ::xsdb::swdesignmaps

	set ret "XSDB Design Table:\n"
	append ret "Hardware Designs - $designtable\n"
	append ret "Software Designs - $swdesignmaps\n"
	append ret "\nHSI Design Table:\n"
	set hsihwmap [::hsi::get_hw_designs]
	append ret "Hardware Designs - $hsihwmap \n"
	set hsiswmap [::hsi::get_sw_designs]
	append ret "Software Designs - $hsiswmap \n"
	return $ret
    }

    #---------------------------------------------------------------------------------------#
    # Create SW Design & Write MSS
    # Description: Creates the SW design and write the corresponding MSS
    # Arguments  :  HW Design (hdf/xml), Name of the SW Design, SW Design Dir Path, Processor
    #               Instance, OS, Application
    #---------------------------------------------------------------------------------------#
    proc write_sw_mss { args } {
	set options {
	    {hw "hardware project" {args 1}}
	    {sw "software design name" {args 1}}
	    {dir "mss path" {args 1}}
	    {processor "processor name" {args 1}}
	    {os "os name" {args 1}}
	    {app "application name" {default "" args 1}}
	}
	array set params [::xsdb::get_options args $options]

	openhw $params(hw)
	# Create a SW design
	if { $params(app) == "" } {
	    ::hsi::create_sw_design $params(sw) -proc $params(processor) -os $params(os)
	} else {
	    ::hsi::create_sw_design $params(sw) -proc $params(processor) -app $params(app) -os $params(os)
	}

	# Write the MSS file
	::hsi::write_mss -name system -dir $params(dir)

	::hsi::close_sw_design [::hsi::current_sw_design]
	opensw $params(dir)/system.mss
    }

    #---------------------------------------------------------------------------------------#
    # Sync SW Design with HW design
    # Description: Syncs SW design with HW design changes
    # Arguments  : New HW design, Old HW design, SW Design, SW Design Dir Path
    #---------------------------------------------------------------------------------------#
    proc sync_sw_with_hw_changes { args } {
	set options {
	    {newhw "new hardware design" {args 1}}
	    {oldhw "old hardware design" {args 1}}
	    {sw "software design" {args 1}}
	    {dir "mss path" {args 1}}
	}
	array set params [::xsdb::get_options args $options]

	openhw $params(oldhw)
	opensw $params(sw)
	openhw $params(newhw)
	# Sync the software design with hardware design changes
	::hsi::internal::update_sw_design
	# Write the changes back to MSS
	::hsi::write_mss -name system -dir $params(dir) -force
	generate_bsp_sources $params(newhw)  $params(sw) $params(dir)
    }

    #---------------------------------------------------------------------------------------#
    # Writes updated sw design to MSS
    # Description: Opens sw and hw designs and write the corresponding MSS
    # Arguments  :  HW Design (hdf/xml) and SW Design Dir Path.
    #---------------------------------------------------------------------------------------#
    proc update_mss { args } {
	set options {
	    {hw "hardware design" {args 1}}
	    {mss "mss path" {args 1}}
	}
	array set params [::xsdb::get_options args $options]

	set_current_hw_sw $params(hw) $params(mss)
	#Write the MSS file
	::hsi::write_mss -force -name system -dir [file dirname [file normalize $params(mss)]]
    }

    #---------------------------------------------------------------------------------------#
    # Get hardware handoff files
    # Description: Opens a HW designs and gets a list of hardware handoff files.
    # Arguments  : Hw Design (HDF/XML), File Type
    #---------------------------------------------------------------------------------------#
    proc get_hw_files_on_hw { args } {
	set hw_design [lindex $args 0]
	set type [lindex $args 1]
	openhw $hw_design
	::hsi::get_hw_files -filter "TYPE == $type"
    }

    #---------------------------------------------------------------------------------------#
    # Get Address Ranges
    # Description: Returns the address ranges w.r.t. a processor instance
    # Arguments  : Hw Design (HDF), Processor Instance
    #              -json (Output in JSON format)
    #---------------------------------------------------------------------------------------#
    proc get_addr_ranges { args } {
	set options {
	    {dict "dict with address map"}
	    {json "output in json format"}
	}
	array set params [::xsdb::get_options args $options 0]

	set hw_design [lindex $args 0]
	set proc_instance [lindex $args 1]

	openhw $hw_design
	set memlist [::hsi::get_mem_ranges -of_objects [::hsi::get_cells $proc_instance -hierarchical ] -filter {MEM_TYPE != "MEMORY"}]
	set dmemlist [::hsi::get_mem_ranges -of_objects [::hsi::get_cells $proc_instance -hierarchical ] -filter { IS_DATA == true && IS_INSTRUCTION == false && MEM_TYPE == "MEMORY"}]
	set imemlist [::hsi::get_mem_ranges -of_objects [::hsi::get_cells $proc_instance -hierarchical ] -filter { IS_DATA == false && IS_INSTRUCTION == true && MEM_TYPE == "MEMORY" }]
	set idmemlist [::hsi::get_mem_ranges -of_objects [::hsi::get_cells $proc_instance -hierarchical ] -filter { IS_DATA == true && IS_INSTRUCTION == true && MEM_TYPE == "MEMORY" }]

	set tz ""
	set acctype ""
	set segment ""
	set mmap ""
	foreach mem $memlist {
	    set base [::common::get_property BASE_VALUE $mem]
	    set high [::common::get_property HIGH_VALUE $mem]
	    # skip the addr region, if base and high are not int
	    if { ![string is double -strict $base] || [catch {expr $base >> 8}] || ![string is double -strict $high] || [catch {expr $high >> 8}]} continue
	    set slaveintf [::common::get_property SLAVE_INTERFACE $mem]
	    set memtype [::common::get_property MEM_TYPE $mem]
	    set name [get_hier_name $mem]
	    set tz [::common::get_property TRUSTZONE $mem]
	    set acctype [::common::get_property ACCESS_TYPE $mem]
	    set flags 3
	    set int_dict [dict_format $params(json) [dict create name [string_format $params(json) $name] \
						     base [string_format $params(json) $base] \
						     high [string_format $params(json) $high] \
						     size [string_format $params(json) [expr $high - $base + 1]] \
						     slaveintf [string_format $params(json) $slaveintf] \
						     type [string_format $params(json) $memtype] \
						     flags [string_format $params(json) $flags] \
						     segment [string_format $params(json) $segment] \
						     acctype [string_format $params(json) $acctype] \
						     tz [string_format $params(json) $tz]]]
	    if { $slaveintf != "" } {
		append name "_$slaveintf"
	    }
	    dict set mmap $name $int_dict
	}

	foreach mem $dmemlist {
	    set base [::common::get_property BASE_VALUE $mem]
	    set high [::common::get_property HIGH_VALUE $mem]
	    if { ![string is double -strict $base] || [catch {expr $base >> 8}] || ![string is double -strict $high] || [catch {expr $high >> 8}]} continue
	    set slaveintf [::common::get_property SLAVE_INTERFACE $mem]
	    set memtype [::common::get_property MEM_TYPE $mem]
	    set name [get_hier_name $mem]
	    set segment [::common::get_property ADDRESS_BLOCK $mem]
	    set tz [::common::get_property TRUSTZONE $mem]
	    set acctype [::common::get_property ACCESS_TYPE $mem]
	    set match 0
	    foreach imem $imemlist {
		set ibase [::common::get_property BASE_VALUE $imem]
		set ihigh [::common::get_property HIGH_VALUE $imem]
		# For MB, we could have ILMB and DLMB at same address
		if { $base == $ibase } {
		    set match 1
		    set id [lsearch -exact $imemlist $imem]
		    set imemlist [lreplace $imemlist $id $id]
		    if { $high > $ihigh} { set high $ihigh }
		    break
		}
	    }

	    if { $match } {
		set flags 7
	    } else {
		set flags 3
	    }

	    set int_dict [dict_format $params(json) [dict create name [string_format $params(json) $name] \
						     base [string_format $params(json) $base] \
						     high [string_format $params(json) $high] \
						     size [string_format $params(json) [expr $high - $base + 1]] \
						     slaveintf [string_format $params(json) $slaveintf] \
						     type [string_format $params(json) $memtype] \
						     flags [string_format $params(json) $flags] \
						     segment [string_format $params(json) $segment] \
						     acctype [string_format $params(json) $acctype] \
						     tz [string_format $params(json) $tz]]]
	    if { $slaveintf != "" } {
		append name "_$slaveintf"
	    }
	    if { $segment != "" } {
		append name "_$segment"
	    }
	    dict set mmap $name $int_dict
	}

	foreach mem $imemlist {
	    set base [::common::get_property BASE_VALUE $mem]
	    set high [::common::get_property HIGH_VALUE $mem]
	    if { ![string is double -strict $base] || [catch {expr $base >> 8}] || ![string is double -strict $high] || [catch {expr $high >> 8}]} continue
	    set slaveintf [::common::get_property SLAVE_INTERFACE $mem]
	    set memtype [::common::get_property MEM_TYPE $mem]
	    set name [get_hier_name $mem]
	    set segment [::common::get_property ADDRESS_BLOCK $mem]
	    set tz [::common::get_property TRUSTZONE $mem]
	    set acctype [::common::get_property ACCESS_TYPE $mem]
	    set flags 5
	    set int_dict [dict_format $params(json) [dict create name [string_format $params(json) $name] \
						     base [string_format $params(json) $base] \
						     high [string_format $params(json) $high] \
						     size [string_format $params(json) [expr $high - $base + 1]] \
						     slaveintf [string_format $params(json) $slaveintf] \
						     type [string_format $params(json) $memtype] \
						     flags [string_format $params(json) $flags] \
						     segment [string_format $params(json) $segment] \
						     acctype [string_format $params(json) $acctype] \
						     tz [string_format $params(json) $tz]]]
	    if { $slaveintf != "" } {
		append name "_$slaveintf"
	    }
	    if { $segment != "" } {
		append name "_$segment"
	    }
	    dict set mmap $name $int_dict
	}

	foreach mem $idmemlist {
	    set base [::common::get_property BASE_VALUE $mem]
	    set high [::common::get_property HIGH_VALUE $mem]
	    if { ![string is double -strict $base] || [catch {expr $base >> 8}] || ![string is double -strict $high] || [catch {expr $high >> 8}]} continue
	    set slaveintf [::common::get_property SLAVE_INTERFACE $mem]
	    set memtype [::common::get_property MEM_TYPE $mem]
	    set name [get_hier_name $mem]
	    set segment [::common::get_property ADDRESS_BLOCK $mem]
	    set tz [::common::get_property TRUSTZONE $mem]
	    set acctype [::common::get_property ACCESS_TYPE $mem]
	    set flags 7
	    set int_dict [dict_format $params(json) [dict create name [string_format $params(json) $name] \
						     base [string_format $params(json) $base] \
						     high [string_format $params(json) $high] \
						     size [string_format $params(json) [expr $high - $base + 1]] \
						     slaveintf [string_format $params(json) $slaveintf] \
						     type [string_format $params(json) $memtype] \
						     flags [string_format $params(json) $flags] \
						     segment [string_format $params(json) $segment] \
						     acctype [string_format $params(json) $acctype] \
						     tz [string_format $params(json) $tz]]]
	    if { $slaveintf != "" } {
		append name "_$slaveintf"
	    }
	    if { $segment != "" } {
		append name "_$segment"
	    }
	    dict set mmap $name $int_dict
	}

	set outdict [dict_format $params(json) $mmap]

	if { $params(dict) || $params(json) } {
	    return $outdict
	} else {
	    set border "[string repeat "=" 80]\n"
	    set formatStr {%23s%15s%15s%15s%7s}
	    set temp [format $formatStr "IP NAME" "BASE ADDRESS" "HIGH ADDRESS" "SIZE" "FLAGS"]
	    set output $border
	    append output "$temp\n"
	    append output $border
	    dict for {ip details} $outdict {
		set flags [dict get $details flags]
		set flagstr ""
		if { $flags & 0x1} {
		    append flagstr "R"
		}
		if { $flags & 0x2 } {
		    append flagstr "W"
		}
		if { $flags & 0x4 } {
		    append flagstr "X"
		}
		append output "[format $formatStr $ip [dict get $details base] [dict get $details high] [format 0x%x [dict get $details size]] $flagstr]\n"
	    }
	    return $output
	}
    }


    #---------------------------------------------------------------------------------------#
    # Get HIER NAME
    # Description: It returns the HIER NAME property of the mem range.
    #		   If the mem range is not a cell or it doesn't have a HIER NAME property,
    # 		   it returns the NAME property.
    #---------------------------------------------------------------------------------------#
    proc get_hier_name { mem } {
	set name [::common::get_property NAME $mem]
	set cell_obj [::hsi::get_cells -hierarchical $mem]
	if { $cell_obj != "" } {
	    set hiername [::common::get_property HIER_NAME $cell_obj]
	    if { $hiername != "" } {
		set name $hiername
	    }
	}
	return $name
    }

     #---------------------------------------------------------------------------------------#
     # Get Configurable IP Instance NAME
     # Description: This returns configurable instance in the hw design.
     #                   This returns valid value for zynq and zynqMp.
     #                   It is empty for other designs
     #                   It gets NAME property of the instance
     # Arguments  : Hw Design (HDF/XML), File Type
     #---------------------------------------------------------------------------------------#
     proc get_configurable_ip { args } {
         set hw_design [lindex $args 0]
         openhw $hw_design
	 set name ""
	 if { [hsi get_cells -filter {CONFIGURABLE == true}] != "" } {
	    set name [::common::get_property NAME [::hsi::get_cells -filter { CONFIGURABLE == true }]]
	 }
	 return $name
     }
     namespace export get_configurable_ip

    #---------------------------------------------------------------------------------------#
    # Get All Peripherals
    # Description: Returns all the peripherals and their properties in a design
    # Arguments  : Hw Design (HDF)
    #              -json (json format)
    #---------------------------------------------------------------------------------------#
    proc get_all_periphs { args } {
	set options {
	    {json "output in json format"}
	}
	array set params [::xsdb::get_options args $options 0]

	set processor ""
	set nargs [llength $args]
	if { $nargs < 1 || $nargs > 2 } {
	    error "wrong # args: should be \"get_all_periphs \[OPTIONS\] <hw-design> \[processor\]\""
	} elseif { $nargs == 1} {
	    set hw_design [lindex $args 0]
	} elseif { $nargs == 2 } {
	    set hw_design [lindex $args 0]
	    set processor [lindex $args 1]
	}
	openhw $hw_design

	# Get all the cells in the Hw design
	set periphs ""
	if { $processor != "" } {
	    set iplist [hsi get_cells -filter "IP_TYPE == PROCESSOR" -hierarchical]
	    if { [lsearch $iplist $processor] == -1 } {
	        error "processor \'$processor\' not found in $hw_design.\nAvailable processors are [regsub -all { } $iplist {, }]"
	    }
	    set periphs [::common::get_property SLAVES [::hsi::get_cells $processor -hierarchical]]
	} else {
	    set periphs [::hsi::get_cells -hierarchical]
	}
	set retdict [get_slave_props $params(json) $periphs]

	if { $params(json) } {
	    return $retdict
	} else {
	    #Table format
	    set border "[string repeat "=" 80]\n"
	    set formatstr {%25s%10s%23s%18s}
	    set temp [format $formatstr "IP INSTANCE" "VERSION" "TYPE" "IP TYPE"]
	    set output $border
	    append output "$temp\n"
	    append output $border

	    dict for {ip details} $retdict {
		set temp [format $formatstr $ip [dict get $details version] [dict get $details type] [dict get $details ip_type]]
		append output "$temp\n"
	    }
	    return $output
	}
    }

    #---------------------------------------------------------------------------------------#
    # Get Slave Properties
    # Description: Returns the properties of all slaves connected to a peripheral
    # Arguments  : json format, peripheral
    #---------------------------------------------------------------------------------------#
    proc get_slave_props { args } {
	set json_fmt [lindex $args 0]
	set periphs [lindex $args 1]

	set slaves {}
	foreach periph $periphs {
	    set handle [::hsi::get_cells $periph -hierarchical]
	    set name [::common::get_property NAME $handle]
	    set hier_name [::common::get_property HIER_NAME $handle]
	    if { $hier_name == "" } {
		set hier_name $name
	    }
	    set type [string_format $json_fmt [::hsi::get_property IP_NAME $handle]]
	    set vlnv [split [::hsi::get_property VLNV $handle] :]
	    if { [llength $vlnv] == 4 } {
		set version [string_format $json_fmt [lindex $vlnv 3]]
	    } else {
		set version [string_format $json_fmt [::common::get_property HW_VER $handle]]
	    }
	    set ip_type [string_format $json_fmt [::hsi::get_property IP_TYPE $handle]]
	    set int_dict [dict_format $json_fmt [dict create hier_name [string_format $json_fmt $hier_name] \
						 type $type version $version ip_type $ip_type]]
	    dict set slaves $name $int_dict
	}
	return [dict_format $json_fmt $slaves]
    }

    #---------------------------------------------------------------------------------------#
    # Get Slaves
    # Description: Get all the peripherals connected to a processor and their properties
    # Arguments  : Hw Design (HDF), Processor Instance
    #              -json (json format)
    #---------------------------------------------------------------------------------------#
    proc get_slaves { args } {
	set options {
	    {json "output in json format"}
	}
	array set params [::xsdb::get_options args $options 0]
	set hw_design [lindex $args 0]
	set proc_instance [lindex $args 1]
	openhw $hw_design

	# Get the slaves of the processor instance
	set periphs [::common::get_property SLAVES [::hsi::get_cells $proc_instance -hierarchical]]

	return [get_slave_props $params(json) $periphs]
    }
    namespace export get_slaves

    #---------------------------------------------------------------------------------------#
    # Get Design Properties
    # Description: Get design properties like DEVICE / FAMLIY / VIVADO_VERSION / TIMESTEMP
    # Arguments  : Hw Design (HDF)
    #              -json (json format)
    #---------------------------------------------------------------------------------------#
    proc get_design_properties { args } {
	set options {
	    {json "output in json format"}
	}
	array set params [::xsdb::get_options args $options 0]
	set hw_design [lindex $args 0]
	openhw $hw_design

	# Get the properties like Device, Family, Timestamp, & Version info
	set dev [string_format $params(json) [::hsi::get_property DEVICE [::hsi::current_hw_design]]]
	set family [string_format $params(json) [::hsi::get_property FAMILY [::hsi::current_hw_design]]]
	set ts [string_format $params(json) [::hsi::get_property TIMESTAMP [::hsi::current_hw_design]]]
	set vv [string_format $params(json) [::hsi::get_property VIVADO_VERSION [::hsi::current_hw_design]]]
	set part [string_format $params(json) [::hsi::get_property PART [::hsi::current_hw_design] -quiet]]

	return [dict_format $params(json) [dict create device $dev family $family timestamp $ts vivado_version $vv part $part]]
    }
    namespace export get_design_properties

    #---------------------------------------------------------------------------------------#
    # Get Parameter Value
    # Description: Get a parameter value for the peripheral
    # Arguments  : Hw Design (HDF), Peripheral Name, Parameter Name
    #              -json (json format)
    #---------------------------------------------------------------------------------------#
    proc get_param_value { args } {
	set options {
	    {json "output in json format"}
	}
	array set params [::xsdb::get_options args $options 0]
	set hw_design [lindex $args 0]
	set periph [lindex $args 1]
	set param [lindex $args 2]
	openhw $hw_design

	# Get the property "param" of the peripheral
	set val [::common::get_property $param [::hsi::get_cells $periph -hierarchical]]
	if { [llength $val] } {
	    return $val
	}

	# Get the property "param" from the CONFIG parameters of the peripheral
	set val [::common::get_property CONFIG.$param [::hsi::get_cells $periph -hierarchical]]
	if { [llength $val] } {
	    return $val
	}
    }
    namespace export get_param_value

    #---------------------------------------------------------------------------------------#
    # Add Repository
    # Description: Add the repository is a 2 step process
    #               1. Set the new repo path
    #               2. Get all the SW cores
    # Arguments  : Repository path
    #---------------------------------------------------------------------------------------#
    proc add_repo { args } {
	variable repopath
	set repopath [lindex $args 0]
	return [init_repo]
    }

    #---------------------------------------------------------------------------------------#
    # Initialize Repository
    # Description: Initialize the repository
    # Arguments  : None
    #---------------------------------------------------------------------------------------#
    proc init_repo {} {
	variable repopath
	variable repo_app_dict

        # Remove RDI_DATADIR from user repositories, before hsi::set_repo. XSCT still needs
        # them, since it handles app repositories separately
        set install_paths [split [set ::env(RDI_DATADIR)] ":"]
        set user_paths {}
        foreach path $repopath {
            if { [lsearch $install_paths [file dirname $path]] == -1 && [lsearch $user_paths $path] == -1 } {
                lappend user_paths $path
            }
        }
	# Removes existing user repo path setting
        ::sdk::scw_clear_open_sw_db
	::hsi::set_repo_path ""
        if { $user_paths != "" } {
            ::hsi::set_repo_path $user_paths
        }

	# Invalidate cache while rescanning the repo, since some apps could have been deleted
	set repo_app_dict ""
	source_all_apps
	::hsi::get_sw_cores
	return ""
    }

    #---------------------------------------------------------------------------------------#
    # Get All OS
    # Description: Get all OS from the repo
    # Arguments  :
    # Type 	 : Internal
    #---------------------------------------------------------------------------------------#
    proc get_repo_os { args } {
	set options {
	    {dict "output in dict format"}
	}
	array set params [::xsdb::get_options args $options]
	set retdict ""

	# Get ACTIVE list of OS for processor
	set oslist [::hsi::get_sw_cores -filter "TYPE == OS && CORE_STATE != DEPRECATED && CORE_STATE != OBSOLETE"]

	# Extract the information from each OS
	foreach os $oslist {
	    set name [::common::get_property NAME $os]
	    set ver [::common::get_property VERSION $os]
	    set supp_proc [::common::get_property SUPP_PERIPHS $os]

	    dict set retdict $os [dict create name $name version $ver supp_proc $supp_proc]
	}

	if { $params(dict) } {
	    return $retdict
	}

	set formatstr {%20s%10s   %s}
	set separator "[string repeat "-" 72]\n"
	set border "[string repeat "=" 72]\n"
	set output $border
	append output "[format $formatstr "OS" "VERSION" "SUPPORTED PROCESSORS"]\n"
	append output $border
	dict for {os details} $retdict {
	    set name [dict get $details name]
	    set ver [dict get $details version]
	    set proclist [dict get $details supp_proc]
	    if { $proclist == "" } {
		set proclist "all"
	    }
	    foreach proc $proclist {
		append output "[format $formatstr $name $ver $proc]\n"
		set name ""
		set ver ""
	    }
	    append output $separator
	}
        return $output
    }

    #---------------------------------------------------------------------------------------#
    # Get Supported OS
    # Description: Get supported OS for set all processors in the design
    # Arguments  : HW Design (HDF)
    #              -json (json format)
    #---------------------------------------------------------------------------------------#
    proc get_supported_os { args } {
	set options {
	    {json "output in json format"}
	}
	array set params [::xsdb::get_options args $options 0]
	set hw_design [lindex $args 0]

	# Open HW & SW designs
	openhw $hw_design
	set retdict ""
	set proc_list [::hsi::get_cells -hierarchical -filter "IP_TYPE==PROCESSOR"]
	foreach procname $proc_list {
	    set tempdict ""
	    # Get the IPNAME of the processor from the cells
	    # This IPNAME is used to filter the sw cores for SUPP_PERIPHS
	    set processor [::hsi::get_property IP_NAME [::hsi::get_cells -hierarchical $procname]]

	    # Get ACTIVE list of OS for processor
	    set oslist [::hsi::get_sw_cores -filter "SUPP_PERIPHS =~ *$processor* || SUPP_PERIPHS == \"\" && TYPE == OS && CORE_STATE != DEPRECATED && CORE_STATE != OBSOLETE"]

	    # Extract the information from each OS
	    foreach os $oslist {
		set name [string_format $params(json) [::common::get_property NAME $os]]
		set ver [string_format $params(json) [::common::get_property VERSION $os]]
		set desc [string_format $params(json) [string map {\" \\"} [::common::get_property DESCRIPTION $os]]]
		set compiler_flags [string_format $params(json) [::common::get_property APP_COMPILER_FLAGS $os]]
		set linker_flags [string_format $params(json) [::common::get_property APP_LINKER_FLAGS $os]]
		set repo [string_format $params(json) [::common::get_property REPOSITORY $os]]
		regsub -all {\\} $repo / repo

		# Inner dict - dict of properties like ver, desc, compiler flags, linker flags, repo
		# Dict of OSes (KEY = OS name & VALUE = above property info dict)
		dict set tempdict $os [dict_format $params(json) [dict create name $name version $ver desc $desc \
								  compilerflags $compiler_flags linkerflags $linker_flags repo $repo]]
	    }
	    # Dict of processor instances (KEY = processor name & VALUE = above OS dict)
	    dict set retdict $procname [dict_format $params(json) $tempdict]
	}

	return [dict_format $params(json) $retdict]
    }

    #---------------------------------------------------------------------------------------#
    # Get OS Details
    # Description: Set OS vesion in SW
    # Arguments  : SW Design, Name, Version
    #---------------------------------------------------------------------------------------#
    proc get_os_details { args } {
	set options {
	    {hw "hardware project" {args 1}}
	    {sw "software project" {args 1}}
	}
	array set params [::xsdb::get_options args $options]

	set_current_hw_sw $params(hw) $params(sw)

	# Get the version of the OS
	set osname [::hsi::get_property NAME [::hsi::get_os]]
	set osver [::hsi::get_property VERSION [::hsi::get_os]]

	set output ""
	set formatstr "  %-12s  %-15s"
	append output "[format $formatstr "OS Name " ": $osname"]\n"
	append output "[format $formatstr "OS Version " ": $osver"]\n"
	return $output
    }

    #---------------------------------------------------------------------------------------#
    # Get All Libraries
    # Description: Get all libraries from the repository
    # Arguments  : None
    #              -json (json format)
    #---------------------------------------------------------------------------------------#
    proc get_all_libs { args } {
	set options {
	    {dict "output in dict format"}
	    {json "output in json format"}
	}
	array set params [::xsdb::get_options args $options]

	# Get all the cores which are ACTIVE and are of type LIBRARY
	set liblist [::hsi::get_sw_cores -filter "TYPE == LIBRARY && CORE_STATE != DEPRECATED && CORE_STATE != OBSOLETE"]

	# Extract the information for each library
	set retdict ""
	foreach lib $liblist {
	    set name [string_format $params(json) [::common::get_property NAME $lib]]
	    set ver [string_format $params(json) [::common::get_property VERSION $lib]]
	    set desc [string_format $params(json) [string map {\" \\"} [::common::get_property DESCRIPTION $lib]]]
	    set compiler_flags [string_format $params(json) [::common::get_property APP_COMPILER_FLAGS $lib]]
	    set linker_flags [string_format $params(json) [::common::get_property APP_LINKER_FLAGS $lib]]
	    set repo [string_format $params(json) [::common::get_property REPOSITORY $lib]]
	    set supp_os [::common::get_property REQUIRES_OS $lib]
	    if { [llength $supp_os] == 0 } {
		set os_list [::hsi::get_sw_cores -filter "TYPE == OS && CORE_STATE != DEPRECATED && CORE_STATE != OBSOLETE"]
		foreach os $os_list {
		    lappend supp_os [::common::get_property NAME $os]
		}
	    }
	    set supp_peri [split [::common::get_property SUPP_PERIPHS $lib] " "]
	    set supp_proc ""
	    foreach peri $supp_peri {
		if { $peri == "psu_cortexa72" || $peri == "psv_cortexa72" || $peri == "ps7_cortexa9" || $peri == "psu_cortexa53" || \
			$peri == "psu_cortexr5" || $peri == "psu_pmu" || $peri == "psv_cortexr5" || $peri == "microblaze" || $peri == "psv_psm" || $peri == "psv_pmc" } {
		    lappend supp_proc $peri
		}
	    }
	    if { [llength $supp_proc] == 0 } {
		set supp_proc {ps7_cortexa9 psu_cortexa53 psu_cortexr5 psv_cortexr5 psu_pmu microblaze psu_cortexa72 psv_cortexa72 psv_psm psv_pmc}
	    }
	    regsub -all {\\} $repo / repo

	    # Inner dict of library properties like version, description, compiler, linker flags, repo
	    # Dict of libraries (KEY = library name & VALUE = dict of library properties)
	    dict set retdict $lib [dict_format $params(json) [dict create name $name version $ver desc $desc \
							      compilerflags $compiler_flags \
							      linkerflags $linker_flags repo $repo \
							      os [string_format $params(json) $supp_os] \
							      proc [string_format $params(json) $supp_proc]]]
	}

	set outdict [dict_format $params(json) $retdict]

        if { $params(json) || $params(dict) } {
	    return $outdict
	} else {
	    set i 0
	    set formatstr {%15s     %s}
	    set border "[string repeat "=" 32]\n"
	    set output $border
	    append output "[format $formatstr "LIBRARY" "VERSION"]\n"
	    append output $border
	    dict for {lib details} $outdict {
		append output "[format $formatstr [dict get $details name] [dict get $details version]]\n"
	    }
	    return $output
	}
    }

    #---------------------------------------------------------------------------------------#
    # Get Software Libraries
    # Description: Get software libraries from the hardware and software designs
    # Arguments  : Hardware Design (HDF/XML), Software Design (MSS)
    #              -json (json format)
    #---------------------------------------------------------------------------------------#
    proc get_sw_libs { args } {
	set options {
	    {json "output in json format"}
	}
	array set params [::xsdb::get_options args $options 0]

	if { [llength $args] == 2 } {
	    set hw_design [lindex $args 0]
	    set sw_design [lindex $args 1]
	} else {
	    error "wrong # args: should be \"get_sw_libs <xsa file> <mss file>\""
	}

	set_current_hw_sw $hw_design $sw_design

	# Get all the libraries from the opened SW design
	set liblist [::hsi::get_libs]

	# Extract the name & version info from each library
	set retdict ""
	foreach lib $liblist {
	    set name [::common::get_property NAME $lib]
	    set ver [string_format $params(json) [::common::get_property VERSION $lib]]

	    # Dict of Libraries (KEY = library name & VALUE = library version)
	    dict set retdict $name $ver
	}
	set outdict [dict_format $params(json) $retdict]

	if { $params(json) } {
	    return $outdict
	} else {
	    set output ""
	    if { $outdict != "" } {
		set formatstr {%10s%10s}
		set border "[string repeat "=" 24]\n"
		set output $border
		append output "[format $formatstr "LIBRARY" "VERSION"]\n"
		append output $border
		set formatstr {%10s%8s}
		dict for {lib ver} $outdict {
		    append output "[format $formatstr $lib $ver]\n"
		}
	    } else {
		set output "No libs found in the bsp"
	    }
	    return $output
	}
    }

    #---------------------------------------------------------------------------------------#
    # Get All Drivers
    # Description: Get all drivers from the repository for the processor
    # Arguments  : Hardware Design (HDF/XML), Processor Name
    #              -json (json format)
    #---------------------------------------------------------------------------------------#
    proc get_all_drivers { args } {
	set options {
	    {json "output in json format"}
	}
	array set params [::xsdb::get_options args $options 0]

	set hw_design [lindex $args 0]
	set procname [lindex $args 1]
	set retdict ""

	# Get the IPNAME of the processor from the cells
	# This IPNAME is used to filter the sw cores for SUPP_PERIPHS
	set processor [::hsi::get_property IP_NAME [::hsi::get_cells $procname -hierarchical]]

	# Get the ACTIVE drivers, supported for the processor
	set driverlist [::hsi::get_sw_cores -filter "SUPP_PERIPHS =~ \*$processor\* || SUPP_PERIPHS == \"\" && TYPE == DRIVER && CORE_STATE != DEPRECATED && CORE_STATE != OBSOLETE"]

	# Get the properties like name, version, description, repository for each driver
	foreach driver $driverlist {
	    set name [string_format $params(json) [::common::get_property NAME $driver]]
	    set ver [string_format $params(json) [::common::get_property VERSION $driver]]
	    set desc [string_format $params(json) [string map {\" \\"} [::common::get_property DESCRIPTION $driver]]]
	    set compiler_flags [string_format $params(json) [::common::get_property APP_COMPILER_FLAGS $driver]]
	    set linker_flags [string_format $params(json) [::common::get_property APP_LINKER_FLAGS $driver]]
	    set repo [string_format $params(json) [::common::get_property REPOSITORY $driver]]
	    regsub -all {\\} $repo / repo

	    # Inner dict of driver properties like version, description, flags, repo
	    # Dict of Drivers (KEY = driver name & VALUE = dict of driver properties)
		dict set retdict $driver [dict_format $params(json) [dict create name $name version $ver desc $desc \
								     compilerflags $compiler_flags \
								     linkerflags $linker_flags repo $repo]]
	}

	return [dict_format $params(json) $retdict]
    }

    #---------------------------------------------------------------------------------------#
    # Get Repo Drivers
    # Description: Get all drivers from the repository
    # Arguments  :
    # Type	 : Internal
    #---------------------------------------------------------------------------------------#
    proc get_repo_drivers { args } {
	set retdict ""

	# Get the ACTIVE drivers, supported for the processor
	set driverlist [::hsi::get_sw_cores -filter "TYPE == DRIVER && CORE_STATE != DEPRECATED && CORE_STATE != OBSOLETE"]

	# Get the properties like name, version, description, repository for each driver
	foreach driver $driverlist {
	    set name [::common::get_property NAME $driver]
	    set ver [::common::get_property VERSION $driver]
	    set supp_peri [::common::get_property SUPP_PERIPHS $driver]

	    # Inner dict of driver properties like version, description, flags, repo
	    # Dict of Drivers (KEY = driver name & VALUE = dict of driver properties)
	    dict set retdict $driver [dict create name $name version $ver supp_peri $supp_peri]
	}

	set formatstr {%22s%10s     %s}
	set border "[string repeat "=" 80]\n"
	set separator "[string repeat "-" 80]\n"
	set output $border
	append output "[format $formatstr "NAME" "VERSION" "SUPPORTED IPs"]\n"
	append output $border
	dict for {drv details} $retdict {
	    set name [dict get $details name]
	    set ver [dict get $details version]
	    set perilist [dict get $details supp_peri]
	    if { $perilist == "" } {
		append output "[format $formatstr $name $ver $perilist]\n"
	    }
	    foreach peri $perilist {
		append output "[format $formatstr $name $ver $peri]\n"
		set name ""
		set ver ""
	    }
	    append output $separator
	}
	return $output
    }

    #---------------------------------------------------------------------------------------#
    # Get All Drivers from Hardware Design
    # Description: Get all drivers from the hardware design
    # Arguments  : Hardware Design (HDF/XML)
    #              -json (json format)
    #---------------------------------------------------------------------------------------#
    proc get_drivers_for_hw { args } {
	set options {
	    {json "output in json format"}
	}
	array set params [::xsdb::get_options args $options 0]

	set hw_design [lindex $args 0]
	set retdict ""
	set supp_peri_list {}
	openhw $hw_design

	# Get the peripheral list associated with the processor name
	set perilist [::hsi::get_cells -hierarchical]

	# Get the generic driver details
	set generic_ver [string_format $params(json) [::hsi::get_property VERSION [::hsi::get_sw_cores -filter "NAME == generic && CORE_STATE != DEPRECATED && CORE_STATE != OBSOLETE"]]]
	set generic_repo [string_format $params(json) [::hsi::get_property REPOSITORY [::hsi::get_sw_cores -filter "NAME == generic && CORE_STATE != DEPRECATED && CORE_STATE != OBSOLETE"]]]
	regsub -all {\\} $generic_repo / generic_repo

	# Loop for each peripheral in the peripheral list to get driver info
	foreach peri $perilist {
	    set dvrdict ""
	    set periname [::hsi::get_property IP_NAME [::hsi::get_cells $peri -hierarchical]]
	    set handle [::hsi::get_cells $peri -hierarchical]
	    set vlnv [split [::hsi::get_property VLNV $handle] :]

	    if { [llength $vlnv] == 4 } {
		set ver [lindex $vlnv 3]
		set version [string_format $params(json) [lindex $vlnv 3]]
	    }

	    # Dict of Generic Driver (KEY = Generic & VALUE = (Dict of version & repo))
	    dict set drvdict generic [dict_format $params(json) [dict create name [string_format $params(json) generic] \
								 version $generic_ver repo $generic_repo]]

	    # Extract the drivers list whose SUPP_PERIPHS is this peripheral
	    set cores [::hsi::get_sw_cores -filter "SUPP_PERIPHS =~ *$periname* && TYPE == DRIVER && CORE_STATE != DEPRECATED && CORE_STATE != OBSOLETE"]
	    foreach core $cores {
		if { $core != "" } {
		    set supp_peri_list [::hsi::get_property SUPP_PERIPHS $core]
		}

		# For all the supported peripherals, extract driver info
		foreach supp_peri $supp_peri_list {
		    if { [compare_support_peris $supp_peri $periname $ver] } {
			set driverlist [::hsi::get_sw_cores -filter "SUPP_PERIPHS =~ *$periname* && TYPE == DRIVER && CORE_STATE != DEPRECATED && CORE_STATE != OBSOLETE"]
			set supp_peri_list {}

			# For each driver in the driver list, extract the info
			foreach driver $driverlist {
			    set drvname [string_format $params(json) [::common::get_property NAME $driver]]
			    set drvver [string_format $params(json) [::common::get_property VERSION $driver]]
			    set drvrepo [string_format $params(json) [::common::get_property REPOSITORY $driver]]
			    set compiler_flags [string_format $params(json) [::common::get_property APP_COMPILER_FLAGS $driver]]
			    set linker_flags [string_format $params(json) [::common::get_property APP_LINKER_FLAGS $driver]]
			    regsub -all {\\} $drvrepo / drvrepo

			    dict set drvdict $driver [dict_format $params(json) [dict create name $drvname version $drvver repo $drvrepo compilerflags $compiler_flags linkerflags $linker_flags]]
			}
		    }
		}
	    }
	    dict set retdict $peri [dict_format $params(json) [dict create version $version driver_info [::json::dict2json $drvdict]]]
	    set drvdict {}
	}
	return [dict_format $params(json) $retdict]
    }

    #----------------------------------------------------------#
    # Compare supported peripherals
    #----------------------------------------------------------#
    proc compare_support_peris { args } {
	set supp_periphs [lindex $args 0]
	set name [lindex $args 1]
	set version [lindex $args 2]

	set v "_v"
	set testname $name$v
	regsub -all {\.} $version {_} version
	set periname $testname$version
	set check [regexp $supp_periphs $periname]
	return $check
    }

    #---------------------------------------------------------------------------------------#
    # Get Drivers for Software Design
    # Description: Get all drivers from the hardware design
    # Arguments  : Hardware Design (HDF/XML), Software Design (MSS)
    #              -json (json format)
    #---------------------------------------------------------------------------------------#
    proc get_drivers_for_sw { args } {
	set options {
	    {json "output in json format"}
	    {dict "output in dict format"}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]
	if { $params(help) } {
	    return [help [lindex [info level 0] 0]]
	}
	if {$params(json) && $params(dict)} {
	    error "-json & -dict are mutually exclusive"
	}

	if { [llength $args] == 2 } {
	    set hw_design [lindex $args 0]
	    set sw_design [lindex $args 1]
	} else {
	    error "wrong # args: should be \"get_drivers_for_sw <xsa file> <mss file>\""
	}

	# Open hw & sw designs
	set_current_hw_sw $hw_design $sw_design
	set retdict ""

	# Get the drivers from the software design
	set drvlist [::hsi::get_drivers]

	# Get all information for each driver
	foreach drv $drvlist {
	    set periname [::hsi::get_property HW_INSTANCE $drv]
	    set drvname [string_format $params(json) [::hsi::get_property NAME $drv]]
	    set drvver [string_format $params(json) [::hsi::get_property VERSION $drv]]

	    dict set retdict $periname [dict_format $params(json) [dict create name $drvname ver $drvver]]
	}

	# Get the driver info for processor
	set procname [::hsi::get_property HW_INSTANCE [::hsi::get_sw_processor]]
	set procdrvname [string_format $params(json) [::hsi::get_property NAME [::hsi::get_sw_processor]] ]
	set procdrvver [string_format $params(json) [::hsi::get_property VERSION [::hsi::get_sw_processor]]]

	dict set retdict $procname [dict_format $params(json) [dict create name $procdrvname ver $procdrvver]]

	set outdict [dict_format $params(json) $retdict]

	if {$params(json) || $params(dict)} {
	    return  $outdict
	} else {
	    set formatstr {%22s%18s  %10s}
	    set border "[string repeat "=" 60]\n"
	    set output $border
	    append output "[format $formatstr "IP NAME" "DRIVER NAME" "DRIVER VERSION"]\n"
	    append output $border
	    set formatstr {%22s%18s%10s}
	    dict for {drv details} $outdict {
		append output "[format $formatstr $drv [dict get $details name] [dict get $details ver]]\n"
	    }
	    return $output
	}
    }

    #---------------------------------------------------------------------------------------#
    # Get Hardware / Software Details
    # Description: Get Processor, OS and Version
    # Arguments  : Hardware Design (HDF/XML), Software Design (MSS)
    #              -json (json format)
    #---------------------------------------------------------------------------------------#
    proc get_hw_sw_details { args } {
	set options {
	    {json "output in json format"}
	}
	array set params [::xsdb::get_options args $options 0]

	set hw_design [lindex $args 0]
	set sw_design [lindex $args 1]

	# Open the Hardware and Software designs
	set_current_hw_sw $hw_design $sw_design

	# Get details for processor, os name, os version
	set procname [string_format $params(json) [::hsi::get_sw_processor]]
	set osname [string_format $params(json) [::hsi::get_property NAME [::hsi::get_os]]]
	set osver [string_format $params(json) [::hsi::get_property VERSION [::hsi::get_os]]]

	set retdict [dict create procname $procname osname $osname osver $osver]
	return [dict_format $params(json) $retdict]
    }

    #---------------------------------------------------------------------------------------#
    # Get Configuration Paramaters for SW
    # Description: Get all the configurable parameters from designs
    # Arguments  : Hardware Design (HDF/XML), Software Design (MSS)
    #              -json (json format)
    #---------------------------------------------------------------------------------------#
    proc get_config_params_for_sw { args } {
	set options {
	    {json "output in json format"}
	}
	array set params [::xsdb::get_options args $options 0]

	set hw_design [lindex $args 0]
	set sw_design [lindex $args 1]

	# Open the Hardware and Software designs
	set_current_hw_sw $hw_design $sw_design
	set retdict ""

	# Get processor name, OS name and list of library names for the sw design
	set procname [::hsi::get_sw_processor]
	set osname [::hsi::get_os]
	set liblist [::hsi::get_libs]

	# For Processor Config parameters
	set paramlist [::hsi::get_comp_params -of_objects [::hsi::get_sw_processor]]
	dict set retdict $procname [read_config_values $params(json) $paramlist $procname]

	# For OS Config parameters
	set paramlist [::hsi::get_comp_params -of_objects [::hsi::get_os]]
	dict set retdict $osname [read_config_values $params(json) $paramlist $procname]

	# For Library Config parameters
	foreach lib $liblist {
	    set paramlist [::hsi::get_comp_params -of_objects [::hsi::get_libs $lib]]
	    if {$paramlist != ""} {
		dict set retdict $lib [read_config_values $params(json) $paramlist $procname]
	    }
	}
	return [dict_format $params(json) $retdict]
    }

    #----------------------------------------------------------#
    # Read Config Params
    #----------------------------------------------------------#
    proc read_config_values { args } {
	set json_fmt [lindex $args 0]
	set paramlist [lindex $args 1]
	set procname [lindex $args 2]
	set tempdict ""

	set slave_list [::hsi::get_property SLAVES [hsi::get_cells -filter "NAME == $procname" -hierarchical]]

	# Get all the information like Category, Value, default, type and description
	foreach param $paramlist {
	    set range_peri {}
	    set param_cat [string_format $json_fmt [::hsi::get_property CATEGORY $param]]
	    set param_val [string_format $json_fmt [::hsi::get_property VALUE $param]]
	    set param_default [string_format $json_fmt [::hsi::get_property CONFIG.default $param]]
	    set param_type [string_format $json_fmt [::hsi::get_property CONFIG.type $param]]
	    set param_desc [string_format $json_fmt [::hsi::get_property CONFIG.desc $param]]
	    set param_permit [string_format $json_fmt [::hsi::get_property CONFIG.permit $param]]

	    set param_values [::hsi::get_property CONFIG.values $param]
	    # For converting the strings like this to dict "New file system"=MFSINIT_NEW, "MFS Image"=MFSINIT_IMAGE, "ROM Image"=MFSINIT_ROM_IMAGE
	    set paramdict [dict_format $json_fmt [parse_config_enum_values $param_values]]

	    # If range is set, then populate each actual IP instance instead of the IP names
	    # The range needs to split to list and then extract the actual IP instance of each IP name given
	    set req_intf [hsi get_property CONFIG.requires_interface $param]
	    if { $req_intf == "" } {
		set range [::hsi::get_property CONFIG.range $param]
		set range [join $range ""]
		set range_list [split $range ,]
		foreach ip $range_list {
		    # Get all IPs with the IP NAME found in the above range list
		    set iplist [::hsi::get_cells -filter "IP_NAME==$ip" -hierarchical]
		    foreach ips $iplist {
			if { $slave_list != "" } {
			    foreach slave $slave_list {
				# List the IP names which are connected to processor as slave
				if { $slave == $ips } {
				    lappend range_peri $ips
				}
			    }
			} else {
			    lappend range_peri $ips
			}
		    }
		}
	    } else {
		set drv_list [hsi get_drivers]
		foreach drv $drv_list {
		    set sw_intf_list [hsi get_sw_interface -of_objects [hsi get_drivers $drv]]
		    foreach intf $sw_intf_list {
			if { $req_intf == $intf } {
			    lappend range_peri [hsi get_property HW_INSTANCE $drv]
			}
		    }
		}
	    }
	    # Inner dict of all properties
	    # Dict of Configurable Parameters (KEY = configurable param name & VALUE = dict of properties above)
	    dict set tempdict $param [dict_format $json_fmt [dict create category $param_cat value $param_val default $param_default \
							     type $param_type desc $param_desc permit $param_permit \
							     options $paramdict range [string_format $json_fmt $range_peri] ] ]
	}
	return [dict_format $json_fmt $tempdict]
    }

    #----------------------------------------------------------#
    # Read Config Params
    #----------------------------------------------------------#
    proc parse_config_enum_values { tempstring } {
	set list1 [split $tempstring ,]
	set list2 {}
	set list3 {}
	set tempdict ""

	for {set i 0} {$i < [llength $list1]} {incr i} {
	    lappend list2 [string trim [lindex $list1 $i] " "]
	}

	for {set i 0} {$i < [llength $list2]} {incr i} {
	    lappend list3 [split [lindex $list2 $i] =]
	}

	for {set i 0} {$i < [llength $list3]} {incr i} {
	    set name [lindex [lindex $list3 $i] 0]
	    set value [lindex [lindex $list3 $i] 1]
	    dict set tempdict $value $name
	}
	return $tempdict
    }

    #---------------------------------------------------------------------------------------#
    # Get Register Data
    # Description: Get the register data for each processor
    # Arguments  : Hardware Design (HDF/XML), Processor Name
    #              -json (json format)
    #---------------------------------------------------------------------------------------#
    proc get_all_register_data { args } {
	set options {
	    {json "output in json format"}
	}
	array set params [::xsdb::get_options args $options 0]

	set hw_design [lindex $args 0]
	set procname [lindex $args 1]
	openhw $hw_design

	set retdict ""
	# Get the peripheral list associated with the processor name
	set perilist [::hsi::get_property SLAVES [::hsi::get_cells $procname -hierarchical] ]

	# Loop for each peripheral in the peripheral list to get the registers
	foreach peri $perilist {
	    set regdict ""

	    # Extract the register list for this peripheral
	    set reglist [::hsi::get_registers -of_objects [::hsi::get_cells $peri -hierarchical]]
	    set peri_base [::hsi::get_property CONFIG.C_BASEADDR [::hsi::get_cells $peri -hierarchical]]

	    # For each register in the register list, extract the info
	    foreach reg $reglist {
		set fielddict ""
		set regenabled [::common::get_property IS_ENABLED $reg -quiet]
		if { $regenabled != "false"} {
		    set regname [::common::get_property NAME $reg]
		    set regdesc [string_format $params(json) [string map {\" \\"} [::common::get_property DESCRIPTION $reg]]]
		    set regaddr [string_format $params(json) [::common::get_property ADDRESS_OFFSET $reg]]
		    set regaccess [string_format $params(json) [::common::get_property ACCESS $reg]]
		    set regsize [string_format $params(json) [::common::get_property SIZE $reg]]
                    set reginterface [string_format $params(json) [::common::get_property INTERFACE_NAME $reg -quiet]]

		    # Get all the fields of a register and their properties
		    set regfields [::hsi::get_fields -of_objects $reg]
		    foreach field $regfields {
			set f_name [::common::get_property NAME $field]
			set f_access [string_format $params(json) [::common::get_property ACCESS $field]]
			set f_bitoff [string_format $params(json) [::common::get_property BIT_OFFSET $field]]
			set f_bitrange [string_format $params(json) [::common::get_property BIT_RANGE $field]]
			set f_bitwidth [string_format $params(json) [::common::get_property BIT_WIDTH $field]]
			set f_desc [string_format $params(json) [string map {\" \\"} [::common::get_property DESCRIPTION $field]]]
			set f_enumval [string_format $params(json) [::common::get_property ENUMERATED_VALUES $field]]
			set f_lsb [string_format $params(json) [::common::get_property LSB $field]]
			set f_mwv [string_format $params(json) [::common::get_property MODIFIED_WRITE_VALUE $field]]
			set f_msb [string_format $params(json) [::common::get_property MSB $field]]
			set f_readact [string_format $params(json) [::common::get_property READ_ACTION $field]]

			dict set fielddict $f_name [dict_format $params(json) [dict create access $f_access bit_offset $f_bitoff bit_range $f_bitrange \
									       bit_width $f_bitwidth desc $f_desc enum_values $f_enumval lsb $f_lsb \
									       mod_write_val $f_mwv msb $f_msb read_action $f_readact]]
		    }

		    # Dict of Registers (KEY = Register Name & VALUE = properties of registers like description, address offset, access and size)
		    dict set regdict $regname [dict_format $params(json)  [dict create description $regdesc address_offset $regaddr \
									   access $regaccess size $regsize  interface $reginterface\
									   fields [dict_format $params(json) $fielddict] memoryAddr [expr $peri_base + $regaddr]]]
		}
	    }

	    # Dict of Peripherals (KEY = peripheral & VALUE = dict of registers)
	    dict set retdict $peri [dict_format $params(json) $regdict]
	    set regdict {}
	}
	return [dict_format $params(json) $retdict]
    }

    #---------------------------------------------------------------------------------------#
    # Get Clock Info
    # Description: Get the clock information
    # Arguments  : Hardware Design (HDF/XML), Peripheral
    #              -json (json format)
    #---------------------------------------------------------------------------------------#
    proc get_clock_info { args } {
	set options {
	    {json "output in json format"}
	}
	array set params [::xsdb::get_options args $options 0]

	set hw_design [lindex $args 0]
	set peripheral [lindex $args 1]

	openhw $hw_design
	set retdict ""

	# Get the clock list connected to the peripheral
	set clk_list [::hsi::get_pins -of_object [::hsi::get_cells -filter "NAME == $peripheral" -hierarchical] -filter "TYPE == clk || TYPE == CLK"]

	foreach clk $clk_list {
	    set freq [string_format $params(json) [::hsi::get_property CLK_FREQ $clk]]
	    # Dict of Clocks (KEY = clock name & VALUE = frequency)
	    dict set retdict $clk $freq
	}
	return [dict_format $params(json) $retdict]
    }

    #---------------------------------------------------------------------------------------#
    # Get All App Details for given platform.
    # Description: Get the application template details
    # Arguments  : None    
    #---------------------------------------------------------------------------------------#
    proc get_apps_for_platform { plat } {
	
	variable outdict [dict create]
	
	set outdict [ ::sdk::get_apps_supported $plat ]
	set i 0
	set formatstr {%35s   %-20s  %-10s}
	set separator "[string repeat "-" 80]\n"
	set border "[string repeat "=" 80]\n"
	set output $border
	append output "[format $formatstr " SYSCONFIG" "DOMAIN" "APPS"]\n"
	append output $border
	dict for {topkey topdetails} $outdict {
	    dict for {sysconfig sdetails} $topdetails {
		dict for {domain appsmap} $sdetails {
		    set applist [dict keys $appsmap]
		    append output "[format $formatstr $sysconfig $domain [string range $applist 0 54]]\n"
		    set len [string length $applist]
		    set pos 55
		    while { $len - $pos > 0 } {
			append output "[format $formatstr "" "" [string range $applist $pos [expr $pos + 55]]]\n"
			incr pos 56
		    }
			
		    append output $separator
	        }
	    }
	}
	return $output
	
    }
    #---------------------------------------------------------------------------------------#
    # Get All App Details
    # Description: Get the application template details
    # Arguments  : None
    #              -json (json format)
    #---------------------------------------------------------------------------------------#
    proc get_all_app_details { args } {
	variable app_templates
	variable repo_app_dict

	set options {
	    {json "output in json format"}
	    {dict "output in dict format"}
	}
	array set params [::xsdb::get_options args $options]

	set retdict ""

	# Get the list of all the apps from all repos and source their tcls
	source_all_apps

	# Find all the files with TCL extensions in both standalone and linux directories
	dict for { app app_details } $repo_app_dict {
	    set os [dict get $app_details os]
	    set path [dict get $app_details path]

	    # Extract all the information like appname, description, path etc.
	    # Run the TCL commands like swapp_get_name etc. from the application namespace
	    set name [string_format $params(json) [namespace eval ::xsdb::sdk::$app {swapp_get_name}]]
	    set desc [string_format $params(json) [namespace eval ::xsdb::sdk::$app {swapp_get_description}]]
		set templatemsspath $path/data/$app.mss
		set bspConstraint false
		if {  [file exists $templatemsspath] } {
			set bspConstraint true
		}
	
		set isBspConstrained [string_format $params(json) $bspConstraint]

	    # If the funcs swapp_get_supported_processors & swapp_get_supported_os are not available in any application.tcl
	    # then consider the app to be supoorted for all processors & OSs
	    if { [namespace eval ::xsdb::sdk::$app {info commands swapp_get_supported_processors} ] == "" } {
	        set supp_processors [string_format $params(json) "ps7_cortexa9 psu_cortexa72 psv_cortexa72 psxl_cortexa78 psx_cortexa78 psu_cortexa53 psu_cortexr5 psv_cortexr5 psxl_cortexr52 microblaze "]
	    } else {
	        set supp_processors [string_format $params(json) [namespace eval ::xsdb::sdk::$app {swapp_get_supported_processors}]]
	    }

	    if { [namespace eval ::xsdb::sdk::$app {info commands swapp_get_supported_os} ] == "" } {
		set osnameslist ""
		set oslist [::hsi::get_sw_cores -filter "TYPE==OS && CORE_STATE != DEPRECATED && CORE_STATE != OBSOLETE"]
		foreach os $oslist {
		    lappend osnameslist [::hsi::get_property NAME $os]
		}
		lappend osnameslist "linux"
		set supp_os [string_format $params(json) $osnameslist]
	    } else {
		set supp_os [string_format $params(json) [namespace eval ::xsdb::sdk::$app {swapp_get_supported_os}]]
	    }

	    set path [string_format $params(json) $path]

	    # Dict of Applications (KEY = app name & VALUE = dict of app information extracted above)
	    dict set retdict $app [dict_format $params(json) [dict create userdefname $name description $desc \
							      supp_proc $supp_processors supp_os $supp_os \
							      path $path os [string_format $params(json) $os] \
							      platform_required "NO" bsp_constraints $isBspConstrained ]]
	}

	set outdict [dict_format $params(json) $retdict]

	if { $params(json) || $params(dict) } {
	    return $outdict
	} else {
	    set aie_root $::env(XILINX_VITIS)/samples
	    # aieroot doesn't exist in trim xsct
	    if { [file exists $aie_root] } {
		set all_aie_dir [glob -directory $aie_root *]
		foreach dir $all_aie_dir {
			set json_desc [glob -nocomplain -directory $dir description.json]
			if { [file exists $json_desc] } {
				set fp [open $json_desc r]
				set props [::json::json2dict [read $fp]]
				close $fp
				set name [string_format $params(json) [dict get $props name]]
				set desc [regsub "\n" [dict get $props description] " "]
    
				# Dict of Applications (KEY = app name & VALUE = dict of app information extracted above)
				dict set retdict $name [dict_format $params(json) [dict create userdefname $name description $desc \
								supp_proc [string_format $params(json) "aie"] supp_os [string_format $params(json) "aieruntime"] \
								path [string_format $params(json) $dir] os [string_format $params(json) "aieruntime"] bsp_constraints [string_format $params(json) "false"] platform_required "YES" ]]
				set outdict [dict_format $params(json) $retdict]
			}
		}
	    }
	    set i 0
	    set formatstr {%-35s %-20s  %-20s %-15s}
	    set separator "[string repeat "-" 100]\n"
	    set border "[string repeat "=" 100]\n"
	    set output $border
	    append output "[format $formatstr " APPLICATION" "SUPPORTED PROCESSOR" "SUPPORTED OS" "PLATFORM REQUIRED"]\n"
	    append output $border
	    dict for {app details} $outdict {
		set proclist [dict get $details supp_proc]
		set oslist [dict get $details supp_os]
		set name  [dict get $details userdefname]
		set platform_req [dict get $details platform_required]
		set len [llength $proclist]
		if { [llength $proclist] < [llength $oslist] } {
		    set len [llength $oslist]
		}
		if { $len == 0 } continue
		for {set i 0} {$i < $len} {incr i} {
		    append output "[format $formatstr $name [lindex $proclist $i] [lindex $oslist $i] $platform_req]\n"
		    set name ""
		    set platform_req ""
		}
		append output $separator
	    }
	    return $output
	}
    }

    #----------------------------------------------------------#
    # Get ATF sources
    #----------------------------------------------------------#
    proc get_atf_sources { } {
	# Get apps directories from all the added repositories
	set app_repos [get_all_app_repos]

	set sw_apps_dirs [dict get $app_repos standalone]
	foreach dir $sw_apps_dirs {
	    set res [glob -nocomplain -type {d  r} -directory $dir "atf_sources"]
	    if { $res != "" } {
		return $res
	    }
	}
    }

    #--------------------------------------------------------------------------#
    # Update the repo dictionary will all the apps repos
    #--------------------------------------------------------------------------#
    proc get_all_app_repos {} {
	variable repopath
	variable sdk
	variable rdidata

	set app_dict ""
	set pathsep ":"

	# Paths for the App Templates
	dict set app_dict standalone [file normalize [file join $sdk "data/embeddedsw/lib/sw_apps"]]
	dict set app_dict linux [file normalize [file join $sdk "data/embeddedsw/lib/sw_apps_linux"]]

	# RDI_DATADIR can have multiple paths separated by : or ;
	if {$::tcl_platform(platform) == "windows"} {
	    set pathsep ";"
	}
	# Split them and reverse them to adjust the priority.
	set repo_list [lreverse [split $rdidata $pathsep]]

	# If any RDI_DATADIR path & XILINX_VITIS point to same path, remove that path from scan list
	foreach dir $repo_list {
	    set apps_dir [file normalize "$dir/embeddedsw/lib/sw_apps/"]
	    if { [file exists $apps_dir] && [lsearch [dict get $app_dict standalone] $apps_dir] == -1 } {
		dict lappend app_dict standalone $apps_dir
	    }

	    set apps_dir_linux [file normalize "$dir/embeddedsw/lib/sw_apps_linux/"]
	    if { [file exists $apps_dir_linux] && [lsearch [dict get $app_dict linux] $apps_dir_linux] == -1 } {
		dict lappend app_dict linux $apps_dir_linux
	    }
	}

	#user repo
	foreach dir $repopath {
	    set apps_dir [file normalize "$dir/sw_apps/"]
	    set apps_tdir [file normalize "$dir/lib/sw_apps/"]
	    if { [file exists $apps_dir] && [lsearch [dict get $app_dict standalone] $apps_dir] == -1 } {
		dict lappend app_dict standalone $apps_dir
	    } elseif { [file exists $apps_tdir] && [lsearch [dict get $app_dict standalone] $apps_tdir] == -1 } {
		dict lappend app_dict standalone $apps_tdir
	    }

	    set apps_dir_linux [file normalize "$dir/sw_apps_linux/"]
	    set apps_tdir_linux [file normalize "$dir/lib/sw_apps_linux/"]
	    if { [file exists $apps_dir_linux] && [lsearch [dict get $app_dict linux] $apps_dir_linux] == -1 } {
		dict lappend app_dict linux $apps_dir_linux
	    } elseif { [file exists $apps_tdir_linux] && [lsearch [dict get $app_dict linux] $apps_tdir_linux] == -1 } {
		dict lappend app_dict linux $apps_tdir_linux
	    }
	}
	return $app_dict
    }

    #--------------------------------------------------------------------------#
    # Source all the application tcl from the repos
    #--------------------------------------------------------------------------#
    proc source_all_apps { } {
	variable repo_app_dict

	set app_dict [get_all_app_repos]
	set app_tcl_paths [dict create]
	# Find directories with appname/data/appname.tcl in both standalone and linux directories
	dict for {os repo_list} $app_dict {
	    foreach repo $repo_list {
		set app_dirs [glob -nocomplain -directory $repo -type d *]
		foreach dir $app_dirs {
		    # Create a namespace for each App and source the TCL file found
		    # User repos are at the end of the list, so keep going until the
		    # end of list, so that we pick user repo
		    # Use the tcl script with same name as app directory name. If it
		    # doesn't exist, check if there is only one tcl script
		    set name [file tail $dir]
		    set app_tcl_path [file normalize [file join $dir "data" "$name.tcl"]]
		    if { [file exists $app_tcl_path] } {
			dict set app_tcl_paths $name $app_tcl_path
		    } else {
			set app_tcl_files [glob -nocomplain [file join $dir "data" "*.tcl"]]
			set len [llength $app_tcl_files]
			if { $len > 0 } {
			    puts "WARNING: cannot find a tcl script with same name as application directory name.\n\
				 \r         ignoring the directory [file normalize $dir]"
			}
		    }
		#    if { [llength $app_tcl_files] == 1 } {
		#	set app_tcl_path [file normalize [lindex $app_tcl_files 0]]
		#	dict set app_tcl_paths $name $app_tcl_path
		#    } else {
		#	set app_tcl_path [file normalize [file join $dir "data" "$name.tcl"]]
		#	if { [file exists $app_tcl_path] } {
		#	    dict set app_tcl_paths $name $app_tcl_path
		#	} elseif { [llength $app_tcl_files] > 1 } {
		#	    puts "WARNING: more than one tcl script found under [file join $dir \"data\"],\n\
		#		 \r         but none with a name matching the application directory name.\n\
		#		 \r         Ignoring the directory [file normalize $dir]"
		#	}
		#    }
		}
	    }
	}
	dict for {name path} $app_tcl_paths {
	    file stat $path fstat
	    set dir [file dirname [file dirname $path]]
	    if { [dict exists $repo_app_dict $name] && [dict get $repo_app_dict $name path] == $dir && [dict get $repo_app_dict $name mtime] == $fstat(mtime)} {
		continue
	    }
	    namespace eval ::xsdb::sdk::$name [list source $path]
	    dict set repo_app_dict $name [dict create os $os path $dir mtime $fstat(mtime)]
	}
    }

    #---------------------------------------------------------------------------------------#
    # Get ip sub_type
    # Description: Get the ip type
    # Arguments  : Ip instance
    #---------------------------------------------------------------------------------------#
    proc get_ip_sub_type { ip_inst_object } {
	set class [hsi::get_property CLASS $ip_inst_object]
	if { [string compare -nocase cell [hsi::get_property CLASS $ip_inst_object]] != 0 } {
	    error "get_mem_type API expect only mem_range type object whereas $class type object is passed"
	}

	set ip_type [hsi::get_property CONFIG.EDK_SPECIAL $ip_inst_object]
	if { [llength $ip_type] != 0 } {
	    return $ip_type
	}

	set ip_name [hsi::get_property IP_NAME $ip_inst_object]
	if { [string compare -nocase "$ip_name"  "lmb_bram_if_cntlr"] == 0
	    || [string compare -nocase "$ip_name" "isbram_if_cntlr"] == 0
	    || [string compare -nocase "$ip_name" "axi_bram_ctrl"] == 0
	    || [string compare -nocase "$ip_name" "dsbram_if_cntlr"] == 0
	    || [string compare -nocase "$ip_name" "ps7_ram"] == 0 } {
	     set ip_type "BRAM_CTRL"
	} elseif { [string match -nocase *ddr* "$ip_name" ] == 1 } {
	     set ip_type "DDR_CTRL"
	} elseif { [string compare -nocase "$ip_name" "mpmc"] == 0 } {
	     set ip_type "DRAM_CTRL"
	} elseif { [string compare -nocase "$ip_name" "axi_emc"] == 0 } {
	     set ip_type "SRAM_FLASH_CTRL"
	} elseif { [string compare -nocase "$ip_name" "psu_ocm_ram_0"] == 0
		  || [string compare -nocase "$ip_name" "psu_ocm_ram_1"] == 0 } {
	     set ip_type "OCM_CTRL"
	} else {
	     set ip_type [hsi::get_property IP_TYPE $ip_inst_object]
	}
	return $ip_type
    }

    #---------------------------------------------------------------------------------------#
    # Is App Supported on Hardware/Software
    # Description: Check if the app is supported on the hw & sw
    # Arguments  : HW Design, OS name, Processor name, App name
    #              -json (json format)
    #---------------------------------------------------------------------------------------#
    proc is_app_supported_on_hw_sw { args } {
	variable ::xsdb::swdesignmaps
	variable repo_app_dict

	set options {
	    {hw "hardware project" {args 1}}
	    {os "os name" {default "standalone" args 1}}
	    {processor "processor name" {args 1}}
	    {arch "32bit/64bit processor" {default "" args 1}}
	    {app "application name" {default "" args 1}}
	    {sw "sw project" {default "" args 1}}
	    {checkwithdefault "check with default mss"}
	}
	array set params [::xsdb::get_options args $options]

	set swdes ""
	openhw $params(hw)

	if { $repo_app_dict == "" } {
	    get_all_app_details
	}
	# Get all the app names, if the app namespace doesn't exist
	if { ! [namespace exists ::xsdb::sdk::$params(app)] } {
	    error "$params(app) doesn't exist in the repositories"
	}

	# Create a SW design
	if { $params(sw) != "" } {
        if { [dict exists $swdesignmaps $params(sw)] } {
            set design [dict get $swdesignmaps $params(sw) design]
            ::hsi::close_sw_design $design -quiet
            set swdesignmaps [dict remove $swdesignmaps $params(sw)]
        }
	    opensw $params(sw)
	} else {
        if { $params(checkwithdefault) } {
            set swdes [::hsi::create_sw_design temsystem -proc $params(processor) -os $params(os) -app "empty_application"]
        } else {
            set swdes [::hsi::create_sw_design temsystem -proc $params(processor) -os $params(os) -app $params(app)]
        }
	}

	set proc_ip_type [::common::get_property IP_NAME [::hsi::get_cells $params(processor) -hierarchical]]
	if { $proc_ip_type == "psu_cortexa53" && $params(arch) == "32"} {
	    ::hsi::set_property -name VALUE -value "arm-none-eabi-gcc" -objects [::hsi::get_comp_params -filter "NAME == compiler"]
	    ::hsi::set_property -name VALUE -value "arm-none-eabi-ar" -objects [::hsi::get_comp_params -filter "NAME == archiver"]
	    set current_val [::hsi::get_property VALUE [::hsi::get_comp_params -filter "NAME == extra_compiler_flags"]]
	    append current_val " -march=armv7-a"
	    ::hsi::set_property -name VALUE -value $current_val -objects [::hsi::get_comp_params -filter "NAME == extra_compiler_flags"]
	}
	if { ($proc_ip_type == "psu_cortexr5" || $proc_ip_type == "psv_cortexr5" || $proc_ip_type == "ps7_cortexa9" || \
		$proc_ip_type == "microblaze")  && $params(arch) == "64" } {
	    error "-arch 64 is invalid for $params(processor) (32-bit processor)"
	}
	# Check if app is supported on HW by calling TCL func swapp_is_supported_hw in application namespace
	if { [catch {set hwsupport [namespace eval ::xsdb::sdk::$params(app) {swapp_is_supported_hw}]} msg] } {
	    if { $swdes != "" } {
		::hsi::close_sw_design $swdes
	    }
            if { $params(sw) != "" } {
                if { [dict exists $swdesignmaps $params(sw)] } {
                    set design [dict get $swdesignmaps $params(sw) design]
                    ::hsi::close_sw_design $design  -quiet
                    set swdesignmaps [dict remove $swdesignmaps $params(sw)]
                }	    
            }
	    error $msg
	}

	# Check if app is supported on SW by calling TCL func swapp_is_supported_hw in application namespace
	if { [catch {set swsupport [namespace eval ::xsdb::sdk::$params(app) {swapp_is_supported_sw}]} msg] } {
	    if { $swdes != "" } {
		::hsi::close_sw_design $swdes
	    }
            if { $params(sw) != "" } {
                if { [dict exists $swdesignmaps $params(sw)] } {
                    set design [dict get $swdesignmaps $params(sw) design]
                    ::hsi::close_sw_design $design  -quiet
                    set swdesignmaps [dict remove $swdesignmaps $params(sw)]
                }	    
            }
	    error $msg
	}

	if { $swdes != "" } {
	    ::hsi::close_sw_design $swdes
	}
        
        if { $params(sw) != "" } {
            if { [dict exists $swdesignmaps $params(sw)] } {
                set design [dict get $swdesignmaps $params(sw) design]
                ::hsi::close_sw_design $design  -quiet
                set swdesignmaps [dict remove $swdesignmaps $params(sw)]
            }	    
	}
        
        return $hwsupport
    }

    #---------------------------------------------------------------------------------------#
    # Get APM Connection Information
    # Description: Get all the APM connection information
    # Arguments  : HW Design
    #              -json (json format)
    #---------------------------------------------------------------------------------------#
    proc get_apm_connection_info { args } {
	set options {
	    {json "output in json format"}
	}
	array set params [::xsdb::get_options args $options 0]
	set hw_design [lindex $args 0]

	openhw $hw_design
	set retdict ""
	# Get the list of IPs with IP NAME 'axi_perf_mon'
	set apm_list [::hsi::get_cells -filter "IP_NAME == axi_perf_mon" -hierarchical]
	# For each APM, get interface pins of type Monitor
	foreach apm $apm_list {
	    set slotlist [string_format $params(json) [::hsi::get_intf_pins -of_objects [::hsi::get_cells $apm -hierarchical] -filter {TYPE == MONITOR}]]
	    dict set retdict $apm $slotlist
	}
	set apm_list [::hsi::get_cells -filter "IP_NAME == psu_apm" -hierarchical]
	foreach apm $apm_list {
	    set slotlist ""
	    set num_slots [::hsi::get_property CONFIG.C_NUM_MONITOR_SLOTS $apm]
	    for {set i 0} {$i < $num_slots} {incr i} {
		lappend slotlist [format "%s_%d_%s" SLOT $i AXI]
	    }
	    set slotlist [string_format $params(json) $slotlist]
	    dict set retdict $apm $slotlist
	}
	return [dict_format $params(json) $retdict]
    }

    #---------------------------------------------------------------------------------------#
    # Get PS Configuration Parameters
    # Description: Get some important PS config parameters
    # Arguments  : HW Design
    #              -json (json format)
    #---------------------------------------------------------------------------------------#
    proc get_ps_config_params { args } {
	set options {
	    {json "output in json format"}
	}
	array set params [::xsdb::get_options args $options 0]

	set hw_design [lindex $args 0]
	openhw $hw_design
	set retdict ""

	# Create a list of all the params required
	# As of now, the list is hard coded here, but in future the list may come from SDK
	set params_list {   PCW_APU_PERIPHERAL_FREQMHZ \
			    PCW_DDR_HPRLPR_QUEUE_PARTITION \
			    PCW_DDR_HPR_TO_CRITICAL_PRIORITY_LEVEL \
			    PCW_DDR_LPR_TO_CRITICAL_PRIORITY_LEVEL \
			    PCW_DDR_PORT0_HPR_ENABLE \
			    PCW_DDR_PORT1_HPR_ENABLE \
			    PCW_DDR_PORT2_HPR_ENABLE \
			    PCW_DDR_PORT3_HPR_ENABLE \
			    PCW_DDR_WRITE_TO_CRITICAL_PRIORITY_LEVEL \
			    PCW_FPGA0_PERIPHERAL_FREQMHZ \
			    PCW_UIPARAM_DDR_BUS_WIDTH \
			    PCW_UIPARAM_DDR_FREQ_MHZ \
			}

	# Get the value for each param from the above list
	for {set i 0} {$i < [llength $params_list]} {incr i} {
	    set name [lindex $params_list $i]
	    # check if cell 'processing_system_1' is present in the hw design
	    set ps7_1 [::hsi::get_cells -filter "IP_NAME == processing_system7" -hierarchical]
	    if { $ps7_1 != ""} {
		set value [string_format $params(json) [::hsi::get_property CONFIG.$name $ps7_1]]
		dict set retdict $name $value
	    }
	}

	return [dict_format $params(json) $retdict]
    }

    #---------------------------------------------------------------------------------------#
    # Get CPU Number
    # Description: Get CPU numbers
    # Arguments  : HW Design
    #              -json (json format)
    #---------------------------------------------------------------------------------------#
    proc get_cpu_nr { args } {
	set options {
	    {json "output in json format"}
	}
	array set params [::xsdb::get_options args $options 0]

	set hw_design [lindex $args 0]
	openhw $hw_design

	set retdict ""
	set jdict ""

	# Get the MDM, MB and debug bridge list
	set mdm_list [::hsi::get_cells -filter {IP_TYPE == "DEBUG"} -hierarchical]
	set mb_list [::hsi::get_cells -filter {IP_TYPE == "PROCESSOR" && IP_NAME == "microblaze"} -hierarchical]
	set debug_bridge_list [::hsi::get_cells -filter {IP_NAME == "debug_bridge"} -hierarchical]

	foreach mdm $mdm_list {
	    set mb_proc_index 0
	    set ports [::common::get_property CONFIG.C_MB_DBG_PORTS [::hsi::get_cells $mdm -hierarchical]]
	    set bscan_type [::common::get_property CONFIG.C_USE_BSCAN [::hsi::get_cells $mdm -hierarchical]]
	    if { $bscan_type == 2 } {
		# EXT_BSCAN - parse the connections to find the hierarchy
		set bscan [::xsdb::get_debug_bridge_bscan_port $mdm "bscan_ext_tdi" $debug_bridge_list]
		if { $bscan == "" } { set bscan "2.1" }
	    } else {
		set bscan [::common::get_property CONFIG.C_JTAG_CHAIN [::hsi::get_cells $mdm -hierarchical]]
		if { $bscan == "" } { set bscan 2 }
	    }
	    foreach mb $mb_list {
		for { set i 0 } { $i < $ports } { incr i } {
		    set mdm_pins [::hsi::get_intf_pins -of_objects $mdm -filter "NAME==[format "MBDEBUG_%d" $i]"]
		    set mdm_net [::common::get_property NAME [::hsi::get_intf_nets -of_objects $mdm_pins] -quiet]
		    set net_ips [::hsi::get_cells -of_objects [::hsi::get_intf_nets -of_objects $mdm_pins]]
		    foreach ip $net_ips {
			if { $ip == $mb } {
			    set mb_proc_index $i
			    dict set retdict $mb [dict_format $params(json) [dict create bscan [string_format $params(json) $bscan] \
							      index [string_format $params(json) $mb_proc_index]]]
			    break
			}
		    }
		}
	    }
	}

	set a9_proc_index 0
	set r5_proc_index 0
	set a53_proc_index 0
	set a72_proc_index 0
	set a78_proc_index 0
	set r52_proc_index 0
	# Get the other processors list which are not MB
	set proc_list [::hsi::get_cells -filter {IP_TYPE == "PROCESSOR" && IP_NAME != "microblaze"} -hierarchical]

	# For each processor, get the processor IP name
	foreach processor $proc_list {
	    set type [::common::get_property IP_NAME [::hsi::get_cells $processor -hierarchical]]

	    switch -- $type {
		"ps7_cortexa9" {
		    set jdict [dict_format $params(json) [dict create bscan \"\" index [string_format $params(json) $a9_proc_index]]]
		    incr a9_proc_index
		}

		"psv_cortexr5" -
		"psu_cortexr5" {
		    set jdict [dict_format $params(json) [dict create bscan \"\" index [string_format $params(json) $r5_proc_index]]]
		    incr r5_proc_index
		}
                
		"psxl_cortexr52" {
		    set jdict [dict_format $params(json) [dict create bscan \"\" index [string_format $params(json) $r52_proc_index]]]
		    incr r52_proc_index
		}

		"psu_cortexa53" {
		    set jdict [dict_format $params(json) [dict create bscan \"\" index [string_format $params(json) $a53_proc_index]]]
		    incr a53_proc_index
		}

		"psv_cortexa72" -
		"psu_cortexa72" {
		    set jdict [dict_format $params(json) [dict create bscan \"\" index [string_format $params(json) $a72_proc_index]]]
		    incr a72_proc_index
		}
                
		"psxl_cortexa78" -
		"psx_cortexa78" {
		    set jdict [dict_format $params(json) [dict create bscan \"\" index [string_format $params(json) $a78_proc_index]]]
		    incr a78_proc_index
		}
	    }
	    if { $jdict != "" } {
		dict set retdict $processor $jdict
	    }
	}

	return [dict_format $params(json) $retdict]
    }

    #---------------------------------------------------------------------------------------#
    # Get Address Tag for Processors
    # Description: Get address tag
    # Arguments  : HW Design
    #              -json (json format)
    #---------------------------------------------------------------------------------------#
    proc get_addr_tag_info { args } {
	set options {
	    {json "output in json format"}
	}
	array set params [::xsdb::get_options args $options 0]

	set hw_design [lindex $args 0]
	openhw $hw_design
	set retdict ""

	# Get processor list
	set proc_list [::hsi::get_cells -filter {IP_TYPE==PROCESSOR} -hierarchical]

	# Get the ADDRESS_TAG for each processor
	foreach proc $proc_list {
	    set addr_tag [string_format $params(json) [::hsi::get_property ADDRESS_TAG $proc]]
	    dict set retdict $proc $addr_tag
	}
	return [dict_format $params(json) $retdict]
    }

    #---------------------------------------------------------------------------------------#
    # Add Library
    # Description: Add library to the sw design
    # Arguments  : SW Design, Lib name, lib version
    #---------------------------------------------------------------------------------------#
    proc add_library_to_sw { args } {
	set options {
	    {hw "hardware project" {args 1}}
	    {sw "software project" {args 1}}
	    {name "library name" {args 1}}
	    {ver "library version" {args 1}}
	}
	array set params [::xsdb::get_options args $options]

	set_current_hw_sw $params(hw) $params(sw)

	# Find out the latest library version
	set ver $params(ver)
	if { $ver == "latest" || $ver == ""} {
	    set libdict [get_all_libs -dict]
	    dict for { lib libdetails } $libdict {
		if { $params(name) == [dict get $libdetails name] } {
		    set ver [dict get $libdetails version]
		}
	    }
	}

	# Add library to the opened SW design
	::hsi::add_library $params(name) $ver
    }

    #---------------------------------------------------------------------------------------#
    # Get Libraries from SW
    # Description: Get libraries from the sw design
    # Arguments  : SW Design
    #---------------------------------------------------------------------------------------#
    proc get_libs_from_sw { args } {
	set hw_design [lindex $args 0]
	set sw_design [lindex $args 1]
	set_current_hw_sw $hw_design $sw_design

	# Get the libraries from the SW design
	return [::hsi::get_libs]
    }

    #---------------------------------------------------------------------------------------#
    # Set Driver Version
    # Description: Set driver vesion in SW
    # Arguments  : SW Design, Instance, Name, Version
    #---------------------------------------------------------------------------------------#
    proc set_driver_version { args } {
	set options {
	    {hw "hardware project" {args 1}}
	    {sw "software project" {args 1}}
	    {instance "driver instance" {args 1}}
	    {name "driver name" {args 1}}
	    {ver "driver version" {default "" args 1}}
	}
	array set params [::xsdb::get_options args $options]
	set_current_hw_sw $params(hw) $params(sw)
	set ver_string ""
	# Don't set the version for none
	if { $params(ver) != "" } {
	    set ver_string "VERSION $params(ver)"
	}
	if { $params(instance) == [::hsi::get_sw_processor] } {
	    ::hsi::set_property -dict "NAME $params(name) $ver_string" [::hsi::get_sw_processor]
	} else {
	    ::hsi::set_property -dict "NAME $params(name) $ver_string" [::hsi::get_drivers -filter "HW_INSTANCE  == $params(instance)"]
	}
    }

    #---------------------------------------------------------------------------------------#
    # Set Library Version
    # Description: Set library vesion in SW
    # Arguments  : SW Design, Name, Version
    #---------------------------------------------------------------------------------------#
    proc set_library_version { args } {
	set options {
	    {hw "hardware project" {args 1}}
	    {sw "software project" {args 1}}
	    {name "library name" {args 1}}
	    {ver "library version" {args 1}}
	}
	array set params [::xsdb::get_options args $options]

	# For changing the library version, we need to delete the library of older version
	# and add the library with newer version
	delete_library_from_sw $params(hw) $params(sw) $params(name)
	add_library_to_sw -hw $params(hw) -sw $params(sw) -name $params(name) -ver $params(ver)
    }

    #---------------------------------------------------------------------------------------#
    # Set OS Version
    # Description: Set OS vesion in SW
    # Arguments  : SW Design, Name, Version
    #---------------------------------------------------------------------------------------#
    proc set_os_version { args } {
	set options {
	    {hw "hardware project" {args 1}}
	    {sw "software project" {args 1}}
	    {name "os name" {args 1}}
	    {ver "os version" {args 1}}
	}
	array set params [::xsdb::get_options args $options]

	set_current_hw_sw $params(hw) $params(sw)

	set ver $params(ver)
	if { $ver == "latest" || $ver == ""} {
	    set libdict [get_all_libs -dict]
	    dict for { lib libdetails } $libdict {
		if { $params(name) == [dict get $libdetails name] } {
		    set ver [dict get $libdetails version]
		}
	    }
	}

	# Set the version of the OS
	::hsi::set_property -dict "NAME $params(name) VERSION $params(ver)" [::hsi::get_os]
    }

    #---------------------------------------------------------------------------------------#
    # Delete Library
    # Description: Delete Library from SW
    # Arguments  : SW Design, Name
    #---------------------------------------------------------------------------------------#
    proc delete_library_from_sw { args } {
	set hw_design [lindex $args 0]
	set sw_design [lindex $args 1]
	set name [lindex $args 2]

	set_current_hw_sw $hw_design $sw_design

	# Delete the library
	::hsi::delete_objs [::hsi::get_libs $name]
    }

    #---------------------------------------------------------------------------------------#
    # Generate BSP
    # Description: Generate BSP and write MSS with name system
    # Arguments  : SW Design, Directory for BSP generation
    #---------------------------------------------------------------------------------------#
    proc generate_bsp_sources { args } {
	set hw_design [lindex $args 0]
	set sw_design [lindex $args 1]
	set dir [lindex $args 2]

	set_current_hw_sw $hw_design $sw_design
	set osname [::hsi::get_property NAME [::hsi::get_os]]
	if { $osname == "xilkernel" } {
	    puts "warning: xilkernel is being deprecated and will not be available from 2017.1 release and beyond.\n\
	    We recommend you use FreeRTOS instead, which is available in the Xilinx SDK.\n"
	}

	::hsi::generate_bsp -dir $dir

	# Delete all the previous MSS files in the directory
	catch {file delete {*}[glob $dir/*.mss]}

	# Write a new system.mss in the directory
	::hsi::write_mss -name system -dir $dir -force
    }

    #---------------------------------------------------------------------------------------#
    # Generate Application
    # Description: Generate Application Template
    # Arguments  : App, Processor, Directory
    #---------------------------------------------------------------------------------------#
    proc generate_app_template { args } {
	set options {
	    {hw "hardware project" {args 1}}
	    {sw "software project" {default "" args 1}}
	    {app "application name" {args 1}}
	    {processor "processor name" {args 1}}
	    {dir "directory" {args 1}}
	    {os "os name" {default "standalone" args 1}}
	}
	array set params [::xsdb::get_options args $options]

	openhw $params(hw)

	# Do not use os and processor options, otherwise hsi will create a new sw design and
        # any changes made to the existing sw design will not be used for creating the app
	if { $params(sw) != "" } {
	    opensw $params(sw)
	    # SDK already generates BSP, hence no need to generate BSP again through generate_app call
	    ::hsi::generate_app -app $params(app) -dir $params(dir) -no_bsp
	} else {
	    # Linux based apps don't have any BSP
	    ::hsi::generate_app -app $params(app) -dir $params(dir)
	}
    }

    #---------------------------------------------------------------------------------------#
    # Set Config Parameters
    # Description: Set/Append the configurable parameters
    # Arguments  : Sw Design, Param, Value, type
    #---------------------------------------------------------------------------------------#
    proc set_comp_param { args } {
	set options {
	    {hw "hardware project" {args 1}}
	    {sw "software project" {args 1}}
	    {parameter "parameter name" {args 1}}
	    {value "parameter value" {args 1}}
	    {append "append property"}
	}
	array set params [::xsdb::get_options args $options]

	set_current_hw_sw $params(hw) $params(sw)

	set comp_param [::hsi::get_comp_params -filter "NAME == $params(parameter)"]
	if { [llength $comp_param] == 0 || [lsearch $comp_param $params(parameter)] == -1 } {
	    # Old versions of SW cores may not have the param defined. Do nothing if the param doesn't exist
	    return
	}
	if { $params(append) } {
	    set current_val [::hsi::get_property VALUE $comp_param]
	    append current_val " $params(value)"
	} else {
	    set current_val $params(value)
	}
	# Set the config property
	::hsi::set_property -name VALUE -value $current_val -objects $comp_param
    }

    #---------------------------------------------------------------------------------------#
    # Get BRAM Blocks
    # Description: Get all the bram memory controller cells
    # Arguments  : Hw Design
    #---------------------------------------------------------------------------------------#
    proc get_bram_blocks { args } {
	set hw_design [lindex $args 0]
	openhw $hw_design

	return [::hsi::get_cells -filter "IP_NAME == lmb_bram_if_cntlr || IP_NAME == xps_bram_if_cntlr || IP_NAME == axi_bram_ctrl || \
				    IP_NAME == plb_bram_if_cntlr || IP_NAME == opb_bram_if_cntlr || IP_NAME == isbram_if_cntlr ||
				    IP_NAME == dsbram_if_cntlr" -hierarchical]
    }

    #---------------------------------------------------------------------------------------#
    # Get HW Param Value
    # Description: Get Hw Param Value
    # Arguments  : Hw Design, Cell, Property
    #---------------------------------------------------------------------------------------#
    proc get_hw_param_value { args }  {
	set hw_design [lindex $args 0]
	set cell [lindex $args 1]
	set prop [lindex $args 2]

	openhw $hw_design

	# Get the HW Config param value
	return [::hsi::get_property CONFIG.$prop [::hsi::get_cells $cell -hierarchical]]
    }

    #---------------------------------------------------------------------------------------#
    # Get Connected Peripherals
    # Description: Get connected peripherals
    # Arguments  : Hw Design, processor instance
    #---------------------------------------------------------------------------------------#
    proc get_connected_periphs { args } {
	set hw_design [lindex $args 0]
	set proc_instance [lindex $args 1]

	openhw $hw_design

	# Get SLAVES for the processor
	::hsi::get_property SLAVES [::hsi::get_cells $proc_instance  -hierarchical]
    }

    #---------------------------------------------------------------------------------------#
    # Set PS Property
    # Description: Set PS property
    # Arguments  : Hw Design, property, value
    #---------------------------------------------------------------------------------------#
    proc set_ps_property { args } {
	set hw_design [lindex $args 0]
	set prop [lindex $args 1]
	set value [lindex $args 2]

	openhw $hw_design

	# Set the property
	::hsi::set_property CONFIG.$prop $value [hsi get_cells processing_system7_1 -hierarchical]
    }

    #---------------------------------------------------------------------------------------#
    # Write PS Init
    # Description: Write PS Init
    # Arguments  : Hw Design, Output Directory
    #---------------------------------------------------------------------------------------#
    proc write_ps_init { args } {
	set hw_design [lindex $args 0]
	set outdir [lindex $args 1]

	openhw $hw_design

	# Generate the ps init files
	set configurablecell [ ::hsi::get_cells -filter { CONFIGURABLE == true } -hierarchical]
	if { $configurablecell != "" } {
	    ::hsi::generate_target {psinit} $configurablecell -dir [file dirname $hw_design]
	}
	# Write PS configurations to the directory
	::hsi::write_ps_configuration $outdir/ps_config_params.tcl
    }

    #----------------------------------------------------------#
    # String Format Wrapper for converting to DICT/JSON
    #----------------------------------------------------------#
    proc string_format { json_fmt value } {
	if { $json_fmt } {
	    return [::json::string2json $value]
	} else {
	    return $value
	}
    }

    #----------------------------------------------------------#
    # Dict Format Wrapper for converting to DICT/JSON
    #----------------------------------------------------------#
    proc dict_format { json_fmt value } {
	if { $json_fmt } {
	    return [::json::dict2json $value]
	} else {
	    return $value
	}
    }

    #---------------------------------------------------------------------------------------#
    # Initialize Linker Gen
    # Description: Initializes the linker gen engine
    # Arguments  : Hw Design, Processor
    #---------------------------------------------------------------------------------------#
    proc lg_init { args } {
	set hw_design [lindex $args 0]
	set sw_design [lindex $args 1]

	variable lgengine

	set_current_hw_sw $hw_design $sw_design

	set lgengine [::hsi::internal::create_lg_engine lg1]
	return $lgengine
    }

    #---------------------------------------------------------------------------------------#
    # Delete Linker Gen
    # Description: Deletes the linker gen engine
    # Arguments  :
    #---------------------------------------------------------------------------------------#
    proc lg_delete { } {
	variable lgengine
	::hsi::delete_objs $lgengine
	set lgengine ""
    }

    #---------------------------------------------------------------------------------------#
    # Get Memories Linker Gen
    # Description: Get memories from the linker gen engine
    # Arguments  :
    #---------------------------------------------------------------------------------------#
    proc lg_get_memories { args } {
	set options {
	    {json "output in json format"}
	}
	array set params [::xsdb::get_options args $options]

	variable lgengine
	if { $lgengine == "" } {
	    error "LinkerGen engine is not initialized"
	}
	set retdict ""
	set ret_val ""

	# Get all the memories
	set memories [::hsi::internal::get_lg_memories -of_objects $lgengine]

	foreach mem $memories {
	    set name [::common::get_property NAME $mem]
	    set base_addr [::common::get_property BASE_ADDRESS $mem]
	    set size [::common::get_property SIZE $mem]

	    if { $params(json) == 1 } {
		dict set retdict $mem [dict_format $params(json) [dict create base_addr [string_format $params(json) $base_addr] \
								  size [string_format $params(json) $size]]]

		set ret_val [dict_format $params(json) $retdict]
	    } else {
		lappend ret_val "$name:$base_addr:$size"
	    }
	}
	return $ret_val
    }

    #---------------------------------------------------------------------------------------#
    # Get Sections Linker Gen
    # Description: Get memories from the linker gen engine
    # Arguments  : <mem_name>:<mem_size>:<assigned_mem>:<all possible memories>
    #---------------------------------------------------------------------------------------#
    proc lg_get_sections { args } {
	set options {
	    {json "output in json format"}
	}
	array set params [::xsdb::get_options args $options]

	variable lgengine
	if { $lgengine == "" } {
	    error "LinkerGen engine is not initialized"
	}
	set ret_val ""
	set retdict ""

	# Get all the memories
	set code_sections [::hsi::internal::get_lg_sections -of_objects $lgengine -filter {TYPE==CODE}]
	set data_sections [::hsi::internal::get_lg_sections -of_objects $lgengine -filter {TYPE==DATA}]

	# Code sections
	set ret_val [get_possible_memories $code_sections "CODE_MEMORY"]
	dict set retdict code_sections [string_format $params(json) $ret_val]

	# Data sections
	set ret_val [get_possible_memories $data_sections "DATA_MEMORY"]
	dict set retdict data_sections [string_format $params(json) $ret_val]

	# Stack section
	set ret_val [get_possible_heap_stack_memories "stack"]
	dict set retdict stack_section [string_format $params(json) $ret_val]

	# Heap section
	set ret_val [get_possible_heap_stack_memories "heap"]
	dict set retdict heap_section [string_format $params(json) $ret_val]

	return [dict_format $params(json) $retdict]
    }

    #----------------------------------------------------------#
    # Get all the possible memories for the section
    #----------------------------------------------------------#
    proc get_possible_memories { sections memory_type } {
	variable lgengine
	if { $lgengine == "" } {
	    error "LinkerGen engine is not initialized"
	}
	set ret_val ""
	foreach section $sections {
	    set name [::common::get_property NAME $section]
	    set guitype [::common::get_property GUI_TYPE $section]

	    # Ignore HEAP & STACK sections
	    # NOSHOW & GREY gui type sections - these sections are not displayed in SDK
	    if { ($name != "heap" && $name != "stack") && $guitype != "GREY" && $guitype != "NOSHOW" } {
		set size [::common::get_property SIZE $section]
		set mem_assigned [::common::get_property MEMORY $section]

		# Code memories - CODE_MEMORY + CODE_AND_DATA_MEMORY
		# Data memories - DATA_MEMORY + CODE_AND_DATA_MEMORY
		set mem_list [::hsi::internal::get_lg_memories -of_objects $lgengine -filter "TYPE==$memory_type || TYPE==CODE_AND_DATA_MEMORY"]
		set ret_str "$name:$size:$mem_assigned"

		foreach mem $mem_list {
		    set ret_str "$ret_str:[::common::get_property NAME $mem]"
		}
		lappend ret_val $ret_str
	    }
	}
	return $ret_val
    }

    #----------------------------------------------------------#
    # Get all the possible memories for Stack & Heap sections
    #----------------------------------------------------------#
    proc get_possible_heap_stack_memories { section_name } {
	variable lgengine
	if { $lgengine == "" } {
	    error "LinkerGen engine is not initialized"
	}

	set sec [::hsi::internal::get_lg_sections -of_objects $lgengine -filter "NAME == $section_name"]
	if { $sec != "" } {
	    set name [::common::get_property NAME $sec]
	    set size [::common::get_property SIZE $sec]
	    set mem_assigned [::common::get_property MEMORY $sec]
	}

	set ret_val "$name:$size:$mem_assigned"

	# Code memories - CODE_MEMORY + CODE_AND_DATA_MEMORY
	# Data memories - DATA_MEMORY + CODE_AND_DATA_MEMORY
	set mem_list [::hsi::internal::get_lg_memories -of_objects $lgengine -filter "TYPE==DATA_MEMORY || TYPE==CODE_AND_DATA_MEMORY"]
	foreach mem $mem_list {
	    set ret_val "$ret_val:[::common::get_property NAME $mem]"
	}
	return $ret_val
    }

    #---------------------------------------------------------------------------------------#
    # Set Section Memory
    # Description: Set the section memories in the linker gen engine
    # Arguments  : section, memory
    #---------------------------------------------------------------------------------------#
    proc lg_set_section_memory { args } {
	set options {
	    {sec "section" {args 1}}
	    {mem "memory" {args 1}}
	    {size "size of section" {args 1}}
	}
	array set params [::xsdb::get_options args $options]

	variable lgengine
	if { $lgengine == "" } {
	    error "LinkerGen engine is not initialized"
	}
	if {[info exists params(mem)]} {
	    ::hsi::set_property MEMORY $params(mem) [::hsi::internal::get_lg_sections -of_objects $lgengine -filter "NAME==$params(sec)"]
	}
	if {[info exists params(size)]} {
	    ::hsi::set_property SIZE $params(size) [::hsi::internal::get_lg_sections -of_objects $lgengine -filter "NAME==$params(sec)"]
	}
    }

    #---------------------------------------------------------------------------------------#
    # Get Section Memory
    # Description: Gets the section memories from the linker gen engine
    # Arguments  :
    #---------------------------------------------------------------------------------------#
    proc lg_get_section_memory { args } {
	set section [lindex $args 0]
	variable lgengine
	if { $lgengine == "" } {
	    error "LinkerGen engine is not initialized"
	}
	::hsi::report_property [::hsi::internal::get_lg_sections -of_objects $lgengine -filter "NAME==$section"]
    }

    #---------------------------------------------------------------------------------------#
    # Add Sections
    # Description: Add the section from the linker gen engine
    # Arguments  : section, size, memory
    #---------------------------------------------------------------------------------------#
    proc lg_add_section { args } {
	set section [lindex $args 0]
	set size [lindex $args 1]
	set memory [lindex $args 2]
	variable lgengine
	if { $lgengine == "" } {
	    error "LinkerGen engine is not initialized"
	}
	::hsi::internal::add_lg_section -name $section -size $size -mem $memory $lgengine
    }

    #---------------------------------------------------------------------------------------#
    # Get Fixed Sections
    # Description: Get the fixed sections from the linker gen engine
    #			Sections that are of GUI_TYPE==GREY are fixed sections
    #			Return Format:
    #			<mem_name>:<mem_size>:<assigned_mem>:<all possible memories>
    # Arguments  :
    #---------------------------------------------------------------------------------------#
    proc lg_get_fixed_sections { } {
	variable lgengine
	if { $lgengine == "" } {
	    error "LinkerGen engine is not initialized"
	}
	set ret_str ""
	set fixed_sections [::hsi::internal::get_lg_sections -of_objects $lgengine -filter {GUI_TYPE==GREY}]
	foreach sec $fixed_sections {
	    set name [::common::get_property NAME $sec]
	    set size [::common::get_property SIZE $sec]
	    set mem_assigned [::common::get_property MEMORY $sec]
	    set type [::common::get_property TYPE $sec]
	    set ret_val "$name:$size:$mem_assigned"
	    set mem_list [::hsi::internal::get_lg_memories -of_objects $lgengine -filter "TYPE==[append type "_MEMORY"] || TYPE==CODE_AND_DATA_MEMORY"]
	    foreach mem $mem_list {
		set ret_val "$ret_val:[::common::get_property NAME $mem]"
	    }
	    lappend ret_str $ret_val
	}
	return $ret_str
    }

    #---------------------------------------------------------------------------------------#
    # Generate Linker Script
    # Description: Generate from the linker gen engine
    # Arguments  : path
    #---------------------------------------------------------------------------------------#
    proc lg_generate { args } {
	set filepath [lindex $args 0]
	variable lgengine
	if { $lgengine == "" } {
	    error "LinkerGen engine is not initialized"
	}

	set filepath [file normalize $filepath]
	set dir [file dirname $filepath]
	set filename [file tail $filepath]

	::hsi::internal::generate_lg_script -file $filename $lgengine -dir $dir -force
    }
    
    #---------------------------------------------------------------------------------------#
    # creates linker script
    # Description : This command generates linkerscript for the active domain 
    # Arguments   : memory, section, generate
    # Type        : XSCT command
    #---------------------------------------------------------------------------------------#

    proc lscript { args } {
        variable lgengine
        variable added_sections
        if { [llength $args] == 0 } {
            error "Wrong # of args: should be \"lscript <sub-command> \[options]\""
        }
        set cmdlist {"begin" "def-mem" "generate" "-help" "memory" "section"}
        set subcmd [lindex $args 0]
        set args [lrange $args 1 end]
        switch -- $subcmd {
        
            "begin" {
                set options {
                    {app "application name" {args 1}}
                    {help "command help"}
                }
                array set params [::xsdb::get_options args $options]
                set app_list [app list -dict]
                if {[info exists  params(app) ]} {
                    set platform ""
                    dict for {key value} $app_list {
                        if { $key == $params(app)} {
                            set domain [dict get $value domain]
                            set platform [dict get $value platform]
                            puts "Using Platform: $platform, Domain: $domain"
                            platform active $platform
                        }
                    }
                    if {$platform == "" } {
                        error "Specified application $params(app) does not exist in the workspace"
                    }
                }
                set mss_path [builtin_scwutil -getmsspath]
                set retval [builtin_platform -report]
                set retdict [::json::json2dict $retval]
                dict for { key value} $retdict {
                    if { $key == "hw spec" } {
                        set xsa_path $value
                    }
                }
                set lgengine [::hsi::utils::lg_init "$xsa_path" $mss_path ]
            }
            
            
            "def-mem" {
                set options {
                    {app "application name" {args 1}}
                    {data "returns default data memory region"}
                    {code "returns default code memory region" }
                    {stack "returns default stack & heap memory"}
                    {help "command help"}
                }
                array set params [::xsdb::get_options args $options]
                if { $params(help) } {
                    return [help lscript def-mem]
                }
                if {[info exists  params(app) ]} {
                    lscript begin -app $params(app)
                }
                if { $lgengine == "" } {
                    lscript begin
                }
                set lscript_sec [::hsi::utils::lg_get_sections]
                dict for {sec_type value} $lscript_sec {
                    set section_flag 0
                    foreach sec [split $value "."] {
                        if {$section_flag == 0} {
                            set sec [split $sec :]
                            set name [lindex $sec 0]
                            if {$name == ""} {
                                continue
                            }
                            set retval [::hsi::utils::lg_get_memories -json]
                            set retdict [::json::json2dict $retval]
                            set def_mem ""
                            set def_size ""
                            set def_addr ""
                            dict for {key value} $retdict {
                                if {$def_mem == ""} {
                                    set mem_valid [lsearch $sec "*$key*"]
                                    if {$mem_valid != "-1"} {
                                        set def_mem $key
                                        set def_size [dict get $retdict $key size]
                                        set def_addr [dict get $retdict $key base_addr]
                                    }
                                } else {
                                    if { [dict get $retdict $key base_addr] < 0xFFFFFFFF && [dict get $retdict $key size] > $def_size } {
                                        set mem_valid [lsearch $sec "*$key*"]
                                        if {$mem_valid != "-1"} {
                                            set def_mem $key
                                            set def_size [dict get $retdict $key size]
                                        }
                                    }
                                }
                            }
                        }
                        set section_flag 1
                        if {$params(data) && $sec_type == "data_sections"} {
                            return $def_mem
                        } elseif {$params(code) && $sec_type == "code_sections"} {
                            return $def_mem
                        } elseif {$params(stack) && $sec_type == "stack_section"} {
                            return $def_mem
                        }
                    }
                }
                return
            }
          
          
            "generate" {
                set options {
                    {app "application name" {args 1}}
                    {name "lscript name" {args 1}}
                    {path "path " {args 1}}
                    {help "command help"}
                }
                array set params [::xsdb::get_options args $options]
                
                if { $params(help) } {
                    return [help lscript generate]
                }
                if {[info exists  params(app) ]} {
                    lscript begin -app $params(app)
                }
                if { $lgengine == "" } {
                    lscript begin
                }
                
                set lscript_sec [::hsi::utils::lg_get_sections]
                dict for {sec_type value} $lscript_sec {
                    foreach sec [split $value "."] {
                        set sec [split $sec :]
                        set name [lindex $sec 0]
                        if {$name == ""} {
                            continue
                        }
                        set size [lindex $sec 1]
                        set assigned_mem [lindex $sec 2]
                        if {$assigned_mem == ""} {
                            if {$sec_type == "data_sections"} {
                                ::hsi::utils::lg_set_section_memory -sec .$name -mem [lscript def-mem -data] -size $size
                            } elseif { $sec_type == "code_sections"} {
                                ::hsi::utils::lg_set_section_memory -sec .$name -mem [lscript def-mem -code] -size $size
                            } elseif { $sec_type == "stack_section" || $sec_type == "heap_section" } {
                                ::hsi::utils::lg_set_section_memory -sec $name -mem [lscript def-mem -stack] -size $size
                            }
                        }
                    }
                }
                if {[info exists  params(name) ]} {
                    if {[info exists  params(path) ]} {
                        ::hsi::utils::lg_generate $params(path)/$params(name).ld
                    } else {
                        ::hsi::utils::lg_generate $params(name).ld
                    }
                } else {
                    if {[info exists  params(path) ]} {
                        ::hsi::utils::lg_generate $params(path)/newlscript.ld
                    } else {
                        ::hsi::utils::lg_generate newlscript.ld
                    }
                }
                ::hsi::utils::lg_delete
                return
            }
            "-help" {
                return [help lscript]
            }
            
            "memory" {
                set options {
                    {app "application name" {args 1}}
                    {supported-mem "memory supported" }
                    {help "command help" }
                }
                array set params [::xsdb::get_options args $options]
                if { $params(help) } {
                    return [help lscript memory]
                }
                if {[info exists  params(app) ]} {
                    lscript begin -app $params(app)
                }
                if { $lgengine == "" } {
                    lscript begin
                }
                if { $params(supported-mem) } {
                    set lscript_sec [::hsi::utils::lg_get_sections]
                    set formatstr {%-20s %-22s}
                    set border "[string repeat "=" 75]\n"
                    set border2 "[string repeat "-" 75]\n"
                    set output $border
                    append output "[format $formatstr "TYPE" "SUPPORTED MEMORY"]\n"
                    append output $border
                    dict for {key value} $lscript_sec {
                        set section_flag 0
                        foreach sec [split $value "."] {
                            if {$section_flag == 0} {
                                set sec [split $sec :]
                                set name [lindex $sec 0]
                                if {$name == ""} {
                                    continue
                                }
                                set len_pos_mem [llength $sec]
                                append output "[format $formatstr "$key"  "[lindex $sec 3]"]\n"
                                for {set i 4} {$i < $len_pos_mem} {incr i} {
                                    append output "[format $formatstr ""  "[lindex $sec $i]"]\n"
                                }
                                append output $border2
                            }
                            set section_flag 1
                        }
                    }
                return $output
                }
                set retval [::hsi::utils::lg_get_memories -json]
                set retdict [::json::json2dict $retval]
                set formatstr {%-22s %-22s %-22s}
                set border "[string repeat "=" 65]\n"
                set output $border
                append output "[format $formatstr "NAME" "BASE_ADDR" "SIZE"]\n"
                append output $border
                dict for {key value} $retdict {
                    append output "[format $formatstr "$key"  [dict get $retdict $key base_addr]  [dict get $retdict $key size]]\n"
                }
                return $output
            }


            "section" {
                set options {
                    {app "application name" {args 1}}
                    {mem "memory name" {args 1}}
                    {size "memory size" {args 1}}
                    {name "sec-name" {args 1}}
                    {add "add a new section to the linkerscript"}
                    {type "type of new section to be added" {args 1}}
                    {help "command help"}
                }
                array set params [::xsdb::get_options args $options]
                if { $params(help) } {
                    return [help lscript section]
                }
                if {[info exists  params(app) ]} {
                    lscript begin -app $params(app)
                }
                if { $lgengine == "" } {
                    lscript begin
                }
                set lscript_sec [::hsi::utils::lg_get_sections]
                dict append added_sections "" "" 
                if { [info exists  params(mem) ] || [info exists params(size)] } {
                    if {$params(add)} {
                        dict for {type sec_list} $lscript_sec {
                            foreach sec $sec_list {
                                set name [lindex [split $sec ":"] 0]
                                if { [string index $name 0] == "." } {
                                    set name [string range $name 1 end]
                                }
                                if { $name == $params(name)} {
                                    error "section $name already exists"
                                }
                            }
                        }
                        if { ![info exists  params(mem) ] || ![info exists params(size)] } {
                            error "wrong args: specify both memory and size for -add"
                        } else {
                            ::hsi::utils::lg_add_section .$params(name) $params(size) $params(mem)
                        }
                        if { [info exists  params(type) ] } {
                            dict append added_sections $params(name) $params(type)
                        } else {
                            dict append added_sections $params(name) "CODE"
                        }
                        return
                    }
                    if {[info exists  params(name) ]} {
                        if {$params(name) == "stack" || $params(name) == "heap" } {
                            set name $params(name)
                        } else {
                            set name .$params(name)
                        }
                        if { [info exists params(size)] && ![info exists params(mem)]} {
                            ::hsi::utils::lg_set_section_memory -sec $name -size $params(size)
                        } elseif { [info exists params(mem)] && ![info exists params(size)]} {
                            ::hsi::utils::lg_set_section_memory -sec $name -mem $params(mem)
                        } else {
                            ::hsi::utils::lg_set_section_memory -sec $name -mem $params(mem) -size $params(size)
                        }
                        return
                    } else {
                        dict for {key value} $lscript_sec {
                            foreach sec [split $value "."] {
                                set sec [split $sec :]
                                set name [lindex $sec 0]
                                if {$name == ""} {
                                    continue
                                }
                                if {$name == "stack" || $name == "heap" } {
                                   set sec_name $name
                                } else {
                                   set sec_name .$name
                                }
                                if { [info exists params(size)] && ![info exists params(mem)]} {
                                    ::hsi::utils::lg_set_section_memory -sec $sec_name -size $params(size)
                                } elseif { [info exists params(mem)] && ![info exists params(size)]} {
                                    ::hsi::utils::lg_set_section_memory -sec $sec_name -mem $params(mem)
                                } else {
                                    ::hsi::utils::lg_set_section_memory -sec $sec_name -mem $params(mem) -size $params(size)
                                }
                            }
                        }
                        return
                    }
                }
                set formatstr {%-10s %-10s %-10s %-22s}
                set border "[string repeat "=" 65]\n"
                set output $border
                append output "[format $formatstr "TYPE" "NAME" "SIZE" "ASSIGNED MEMORY" ]\n"
                append output $border
                dict for {key value} $lscript_sec {
                    if {$key == "code_sections"} {
                        set key CODE
                    } elseif {$key == "data_sections"} {
                        set key DATA
                    } elseif {$key == "stack_section"} {
                        set key STACK
                    } elseif {$key == "heap_section"} {
                        set key HEAP
                    }
                    foreach sec [split $value "."] {
                        set sec [split $sec :]
                        set name [lindex $sec 0]
                        if {$name == ""} {
                            continue
                        }
                        set size [lindex $sec 1]
                        set assigned_mem [lindex $sec 2]
                        if {$assigned_mem == ""} {
                            set assigned_mem "default"
                        }
                        dict for {section type} $added_sections {
                            if { $name == $section} {
                                set key $type
                            }
                        }
                         
                        append output "[format $formatstr "$key" "$name" "$size" "$assigned_mem" ]\n"
                    }
                }
                return $output
            }
            

            default {
                error "Wrong sub-command, use help lscript for the list of sub-commands"
            }
        }

    }
    namespace export lscript
    ::xsdb::setcmdmeta lscript categories {projects}
    ::xsdb::setcmdmeta lscript brief {Create linker script.}
    ::xsdb::setcmdmeta lscript description {
SYNOPSIS {
    lscript <sub-command> [options]
        Create a linkerscript, or perform various other operations on
        the linker script, based on the sub-command specified.
        Following sub-commands are supported.
            memory   - List of the memories supported by the active domain.
            section  - Lists and edit the sections available.
            def-mem  - Returns default memory for the section type.
            generate - Generate a linker script.

        Type "help" followed by "lscript sub-command", or "lscript sub-command" followed
        by "-help" for more details.    
}
RETURNS {
    Depends on the sub-command. Refer to the sub-command help for details.
}
OPTIONS {
    Depends on the sub-command. Refer to the sub-command help for details.
}
EXAMPLE {
    Refer to the sub-command help for details.
}
SUBCMDS {
    memory section def-mem generate
}
}

    ::xsdb::setcmdmeta {lscript memory} brief {List supported memory.}
    ::xsdb::setcmdmeta {lscript memory} description {
SYNOPSIS {
    lscript memory [options]
        List of the memories supported by the active domain.
}
OPTIONS {
    -app <application-name>
        Name of application from workspace.
        
    -supported-mem
        Returns supported memory regions for each section.
}
RETURNS {
    List of the memories supported by the active domain in tabular format.
}
EXAMPLE {
    lscript memory
        This command returns the list of memories available in the active domain.

    lscript memory -app <application-name>
        Returns list of memories available for application specified.
        This command makes the platform and domain of the specified
        application into the active platform and domain.

    lscript memory -supported-mem
        Returns the section wise supported memories.
}
}

    ::xsdb::setcmdmeta {lscript section} brief {List the sections available.}
    ::xsdb::setcmdmeta {lscript section} description {
SYNOPSIS {
    lscript section [options]
        List, add, and edit the sections available in the active domain.
}
OPTIONS {
    -app <application-name>
        Name of application from workspace.
        
    -name <section-name>
        Name of the section to be edited.
    
    -mem <memory-region>
        Name of the memory region to be used for the section.
    
    -size <section-size>
        Size of the section.
    
    -add
        Add a new section.
        
    -type
        Type of new section to be added.
        Supported types are CODE, DATA, STACK, HEAP.
}
RETURNS {
    List of the sections with corresponding memory and size in tabular format,
    when no options or args are specified.
    Nothing, if a section sucessfully edited or added.
    Error, if the section cannot be edited or added.
}
EXAMPLE {
    lscript section
        List of the sections available in the active domain along with the type,
        size and assigned memory.

    lscript section -app <application-name>
        List of the sections available for application specified.
        This command makes the platform and domain of the specified
        application into the active platform and domain.

    lscript section -name <section-name> -mem <memory-region> -size <section-size>
        Edit the section-name with memory and size.

    lscript section -mem <memory-region> -size <section-size>
        Edit all the sections with memory and size.

    lscript section -add -name <section-name> -mem <memory-region> -size <section-size>
            -type <section-type>
        Add a new section with section-name, memory, and size.
}
}

    ::xsdb::setcmdmeta {lscript def-mem} brief {Returns the default memory region for the section type.}
    ::xsdb::setcmdmeta {lscript def-mem} description {
SYNOPSIS {
    lscript def-mem <memory-type>
        Return the default memory region of the section type.
}
OPTIONS {
    -app <application-name>
        Name of application from workspace.
        
    -code
        Return default code memory.
        
    -data
        Return default data memory.
        
    -stack
        Return default stack & heap memory.
}
RETURNS {
    Return the default memory region of the section type.
}
EXAMPLE {
    lscript def-mem -stack
        Return default stack and heap memory-region.
        
    lscript def-mem -stack -app <application-name>
        Return default stack and heap memory region for app specified.
}
}

    ::xsdb::setcmdmeta {lscript generate} brief {Generate a linker script.}
    ::xsdb::setcmdmeta {lscript generate} description {
SYNOPSIS {
    lscript generate [options]
        Generate a linker script.
}
OPTIONS {
    -app <application-name>
        Name of application from workspace.
        
    -name <linkerscript name>
        Name of the linker script file.
        The default linker script will be "newlscript.ld" if -name is not provided.
    
    -path <path>
        The directory where the linker script needs to be created,
        The default path will be pwd if -path is not provided.
}
RETURNS {
   Nothing.
}
EXAMPLE {
    lscript generate -name <linkerscript name> -path <path>
        This command generates a linkerscript with the changes at the path provided.
        Otherwise, generate a default linker script with name "newlscript.ld".

    lscript generate -app <application-name>
        This command generates the default linker script for the
        application-name specified.

}
}
    namespace ensemble create -command ::hsiutils
}

package provide hsi::utils $::hsi::utils::version
