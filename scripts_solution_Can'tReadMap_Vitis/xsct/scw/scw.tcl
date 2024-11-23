package require Tcl 8.5
package require xsdb
package require json

# set tcl_prompt1 {puts -nonewline "scw% "}

namespace eval scw {
    variable regen_done 0
    variable platformdict [dict create]

    # Temp changes to support gradle builds, which creates the libs with
    # libxv_ prefix instead of librdi_ prefix. gradle build scripts search
    # and replace "set GRADLE_BUILD 1" with "set GRADLE_BUILD 1" during build,
    # so libxv_* libs are loaded when the tool is run from gradle output
    set GRADLE_BUILD 1
    if { $GRADLE_BUILD } {
	if { $::tcl_platform(platform) == "windows" } {
	    set lib_name [join xv_scw[info sharedlibextension]]
	} else {
	    set lib_name [join libxv_scw[info sharedlibextension]]
	}
    } else {
	set lib_name [join librdi_scw[info sharedlibextension]]
    }

    set loaded 0
    set pkg_name "Rdi_scw"
    foreach d $::auto_path {
	if { ![catch {load [file join $d $lib_name] $pkg_name}] } {
	    set loaded 1
	    break
	}
    }
    if { !$loaded } {
	load $lib_name $pkg_name
    }


    proc gethwfile { hwdir } {

	set hw_spec [glob -nocomplain -directory $hwdir -type f *{.dsa,.xsa}]
	if { $hw_spec == "" } {
	    error "unable to find hardware specification file in $hwdir"
	}
	return $hw_spec
    }


    proc upgrade_spr { args } {
        set spr [lindex $args 0]
        platform read $spr
        builtin_scwutil -upgradespr
        platform write
    }
    
    proc clear_platform { args } {
	set options {
	    {name "platform name" {args 1}}
	    {outdir "platform location" {args 1}}	    
	}
	set saved_args $args
	array set params [::xsdb::get_options args $options 0]	
	builtin_scwutil -cleanplatform $params(name) -outdir $params(outdir)
    }


    proc generate_bif { args } {
	set options {
	    {xpfm "xpfmpath" {args 1}}
	    {domains "domains list " {args 1}}
	    {bifpath "bifpath" {args 1}}
	}

	set saved_args $args
	array set params [::xsdb::get_options args $options 0]

	builtin_bifgen {*}$saved_args
	set xpfmpath $params(xpfm)
	set hwfilepath [gethwfile [file join [file dirname $xpfmpath] hw ]]
	set hwdb [hsi current_hw_design]
	update_hwdb_table $hwfilepath $hwdb
    }


    proc copy_files { args } {
	if { [llength $args] != 2 } {
		  error "wrong # of args: should be \"copy_files src-dir dest-dir\""
	}
	set src [lindex $args 0]
	set dest [lindex $args 1]
	if { ![file isdirectory $src] } {
	    error "$src is not a directory"
	}
	if { [catch { tfile realpath $dest } msg] } {
	    tfile mkdir $dest
	}
	foreach f [glob -nocomplain -directory $src -type f *] {
	    tfile copy -from-host $f [file join $dest [file tail $f]]
	}
	foreach d [glob -nocomplain  -directory $src -type d *] {
	    copy_files $d [file join $dest [file tail $d]]
	}
    }


    #---------------------------------------------------------------------------------------#
    # Software platform
    # Description:  Creates a Software Platform with the given arguments
    # Arguments  :  Hardware project / Hardware Design (*.hdf / *.xml)
    # Type       :  SCW command
    #---------------------------------------------------------------------------------------#

    proc update_hwdb_table { args } {
	variable ::xsdb::designtable

	if { [llength $args] == 2 } {
	    set dsapath [file normalize [lindex $args 0]]
	    set design [lindex $args 1]
	    if { ![dict exists $designtable $dsapath] } {
	      dict set designtable $dsapath design $design
	    } else {
                dict set designtable $dsapath design $design
            }
	} else {
	    error "Wrong # args: should be \"update_hw_db <xsa file> <hwdb name> \""
	}
    }


    proc clear_hwdb_table { args } {
    variable ::xsdb::designtable
	if { [llength $args] == 1 } {
	    set dsapath [lindex $args 0]
	    if { [dict exists $designtable $dsapath] } {
		set designtable [dict remove $designtable $dsapath]
	    }
	} else {
	    error "Wrong # args: should be \"clear_hwdb_table <xsa file> \""
	}
    }


    proc isAppWithBspSettings { template } {

	set templatepath [builtin_scwutil -apptclpath $template]
	set templatedir [file dirname $templatepath]
	set templatemsspath $templatedir/$template.mss
	return [file exists $templatemsspath]
    }


    proc isAppWithLscriptSettings { template } {
	set lconstrints [::scw::get_app_linker_constraints $template [builtin_scwutil -apptclpath $template]]
	if { $lconstrints == ""} {
		return 0
	} else {
		return 1
	}
    }


    proc get_app_template { args } {
	set sdktemplate [lindex $args 0]
	if { [llength $args] == 1 } {
	    set hsitemplate ""
	    set applist [::hsi::utils::get_all_app_details -dict]
	    dict for {app details} $applist {
		if { $sdktemplate == [dict get $details userdefname] } {
		    set hsitemplate $app
		    break;
		}
	    }
	    return $hsitemplate
	} else {
	    error "Wrong # args: should be \"get_app_template  <template with space> \""
	}
    }
    proc isvalid_app_template { args } {
	set sdktemplate [lindex $args 0]
	if { [llength $args] == 1 } {
	    set applist [::hsi::utils::get_all_app_details -dict]
	    dict for {app details} $applist {
		if { $sdktemplate == $app } {
		    return 1
		}
	    }
	    return 0
	} else {
	    error "Wrong # args: should be \"isvalid_app_template  <template with space> \""
	}
    }


    proc update_swdb_table { args } {
	variable ::xsdb::swdesignmaps

	if { [llength $args] == 2 } {
	      set msspath [lindex $args 0]
	      set design [lindex $args 1]
	      if { ![dict exists $swdesignmaps $msspath] } {
		dict set swdesignmaps $msspath design $design
	      } else {
		set swdesignmaps [dict remove $swdesignmaps $msspath]
		dict set swdesignmaps $msspath design $design
	      }
	} else {
	    error "Wrong # args: should be \"update_sw_db <mss file> <swdb name> \""
	}
    }


    proc clean_swdb_table { args } {
	variable ::xsdb::swdesignmaps

	if { [llength $args] == 1 } {
	    set mss [lindex $args 0]
	    if { [dict exists $swdesignmaps $mss] } {
		set design [dict get $swdesignmaps $mss design]
		set swdesignmaps [dict remove $swdesignmaps $mss]
		::hsi::close_sw_design $design
	    } else {
		error "Cannot close sw design \'$mss\'.\nDesign is not opened in the current session.\n\n"
	    }
	} else {
	    error "Wrong # args: should be \"update_sw_db <mss file> <swdb name> \""
	}
    }


    proc get_app_linker_constraints { appname apptclpath } {
	set namespace_name [file rootname [file tail $apptclpath]]
	namespace eval ::xsdb::sdk::$namespace_name [list source $apptclpath]
	set linkerconstarint [namespace eval ::xsdb::sdk::$appname {swapp_get_linker_constraints}]
	return $linkerconstarint
    }


    proc get_ip_sub_type { ip_inst_object } {
	if { [string compare -nocase cell [common::get_property CLASS $ip_inst_object]] != 0 } {
	    error "get_mem_type expects only mem_range type object whereas $class type object is passed"
	}

	set ip_type [common::get_property CONFIG.EDK_SPECIAL $ip_inst_object]
	    if { [llength $ip_type] != 0 } {
	    return $ip_type
	}

	set ip_name [common::get_property IP_NAME $ip_inst_object]
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
	    set ip_type [common::get_property IP_TYPE $ip_inst_object]
	}
	return $ip_type
    }


    proc get_platform_data { args } {
	set options {
	    {name "name of the software platform" {args 1}}
	}

	array set params [::xsdb::get_options args $options]
	# set the given platform as the active platform.
	builtin_platform -active $params(name)

	set retval [builtin_platform -json ]
	return $retval
    }


    proc regenerate_psinit { args } {
	variable regen_done
	return
	if { $regen_done == 0 } {
	    if { [llength $args] == 1 } {
		set hdf [lindex $args 0]
		set configurablecell [::hsi::get_cells -filter { CONFIGURABLE == true } -hierarchical]
		if { $configurablecell != "" } {
		    ::hsi::generate_target {psinit} $configurablecell -dir [file dirname $hdf]
		    set regen_done 1
		}
	    } else {
		error "Wrong # args: should be \"regenerate_psinit <dir> \""
	    }
	}
    }


    proc get_hw_path {} {
	return [builtin_scwutil -gethwpath]
    }


    proc clearplatforms {} {
	set mode [builtin_scwutil -mode]
	if { $mode != "gui"} {
	    return [builtin_scwutil -clearplatforms]
	}
    }


    proc get_os_version { args } {
	if { [llength $args] == 1 } {
	    set retval [builtin_scwutil -getosver [lindex $args 0] ]
	    return $retval
	} else {
	    error "Wrong # args: should be \"get_os_version <os> \""
	}
    }


    proc set_boot_autogen { args } {
	if { [llength $args] == 1 } {
	    set retval [builtin_scwutil -bootautogen [lindex $args 0] ]
	    return $retval
	} else {
	    error "Wrong # args: should be \"set_boot_autogen <true/false> \""
	}
    }


    proc set_linux_configured { args } {
	if { [llength $args] == 1 } {
	    set retval [builtin_scwutil -linuxconfigured [lindex $args 0] ]
	    return $retval
	} else {
	    error "Wrong # args: should be \"set_linux_autogen <true/false> \""
	}
    }


    proc set_lscript_autogen { args } {
	if { [llength $args] == 1 } {
		set retval [builtin_scwutil -lscriptautogen [lindex $args 0] ]
		return $retval
	} else {
	    error "Wrong # args: should be \"set_lscript_autogen <true/false> \""
	}
    }


    proc set_prebuilt_autogen { args } {
	if { [llength $args] == 1 } {
		set retval [builtin_scwutil -prebuiltautogen [lindex $args 0] ]
		return $retval
	} else {
	    error "Wrong # args: should be \"set_lscript_autogen <true/false> \""
	}
    }


    proc scw_mode { args } {
      if { [llength $args] == 1 } {
	    set retval [builtin_scwutil -mode [lindex $args 0] ]
	    return $retval
	} else {
	    error "Wrong # args: should be \"scw_mode <mode> \""
	}
    }


    proc get_mss_path {} {
	set retval [builtin_scwutil -getmsspath]
	set desname [hsi current_sw_design]
	update_swdb_table $retval $desname
	return $retval
    }


    proc get_target {} {
	return [builtin_scwutil -gettarget]
    }


    proc clear_open_swdb {} {
	set curactiveplat ""
	set curactivesys ""
	set curactivedom ""

	if { [catch { set curactiveplat [platform active] } msg] } {
		} else {
	    set curactiveplat [platform active]
	    if { [catch { set curactivesys [sysconfig active] } msg] } {
	    } else {
		set curactivesys [sysconfig active]
		if { [catch { set curactivedom [domain active] } msg] } {
		} else {
		    set curactivedom [domain active]
		}
	    }
	}
	set platlout ""
	if { [catch { set platlout [builtin_platform -list] } msg] } {
	    return
		} else {
	    set platlout [builtin_platform -list]
	}

	set platdict [::json::json2dict $platlout]
	if { [dict size $platdict] == 0  } {
	    return
	}
	dict for {pkey pvalue} $platdict {
	    set plat $pkey
	    platform active $plat
	    set syslout ""
	    if { [catch { set syslout [builtin_system -list] } msg] } {
		continue
	    }
	    set syslout [builtin_system -list]
	    set sysdict [::json::json2dict $syslout]
	    if { [dict size $sysdict] == 0  } {
		continue
	    }
	    dict for {skey svalue} $sysdict {
		set sys $skey
		sysconfig active $sys
		set domlout ""
		if { [catch { set domlout [builtin_domain -list] } msg] } {
		    continue
		}
		set domlout [builtin_domain -list]

		set domdict [::json::json2dict $domlout]
		if { [dict size $domdict] == 0  } {
		    continue
		}
		dict for {dkey dvalue} $domdict {
		    set dom $dkey
		    domain active $dom
		    set domrepout [builtin_domain -report]
		    set domrepdict [::json::json2dict $domrepout]
		    set domos [dict get $domrepdict os]
		    if { $domos != "linux"} {
			sdx_write_mss
			builtin_scwutil -clearswdbtable
		    }
		}
	    }
	}
	if { $curactiveplat != ""} {
	    platform active $curactiveplat
	    if { $curactivesys != "" } {
		sysconfig active $curactivesys
		if { $curactivedom != "" } {
		    domain active $curactivedom
		}
	    }
	}
    }


    proc sdx_write_mss {}  {
	set retval [platform -write]
	set msspath [builtin_scwutil -savemsschanges]
	set msspath [builtin_scwutil -getmsspath]
	set desname [hsi current_sw_design]
	update_swdb_table $msspath $desname
	clean_swdb_table $msspath
	builtin_scwutil -clearswdbtable
	return $retval
    }


    proc sdx_reload_mss {}  {
	set retval [builtin_scwutil -reloadmss]
	set msspath [builtin_scwutil -getmsspath]
	set desname [hsi current_sw_design]
	update_swdb_table $msspath $desname
	clean_swdb_table $msspath
        builtin_scwutil -clearswdbtable
	return $retval
    }


    proc reload_linkgen { args } {
	set hdf [get_hw_path]
	::hsi::generate_target {psinit}  [::hsi::get_cells -filter { CONFIGURABLE == true } -hierarchical] -dir [file dirname $hdf]
	set retval [builtin_app -reload-linkgen]
	return $retval
    }


    #---------------------------------------------------------------------------------------#
    # Software platform
    # Description:  Creates a Software Platform with the given arguments
    # Arguments  :  Hardware project / Hardware Design (*.hdf / *.xml)
    # Type       :  SCW command
    #---------------------------------------------------------------------------------------#
    proc get_supported_os { args } {
	set retval [builtin_scwutil -getos]
	return $retval
    }


    proc get_supported_proc { args } {
	set options {
	    {os "name of the os" {args 1}}
	}

	array set params [::xsdb::get_options args $options]
	if { [info exists params(os) ] } {
	    set retval [builtin_scwutil -getproc $params(os)]
	    return $retval
	}
    }


    proc plnx-install-path { args } {
	set nargs [llength $args]
	if { $nargs == 0 } {
	    set retval [builtin_scwutil -plnxpath]
	    return $retval
	}
	set retval [builtin_scwutil -plnxpath [lindex $args 0]]
	return $retval
    }
    namespace export plnx-install-path


    proc removeproj { name } {
	    set mode [builtin_scwutil -mode]
	    if { $mode != "gui"} {
		    set chan [::sdk::getsdkchan]
		    set retval [::sdk::xsdk_eval $chan "XSDx" deleteProjects "o{[dict create Name s Workspace s Type s]}" e [list [dict create Name $name Workspace "false" Type "platform"]]]
		    if { [lindex $retval 0] != "" } {
		    error $retval
		    }
	    }
    }

    proc getDeviceType { } {
        if { [hsi get_cells ps7_cortexa9_0] != "" } {
            return "zynq"
        }
        if { [hsi get_cells psu_cortexa53_0] != "" } {
            return "zynqmp"
        }
        if { [hsi get_cells psv_cortexa72_0] != "" } {
            return "versal"
        }
        return "fpga"
    }
    #---------------------------------------------------------------------------------------#
    # Software platform
    # Description:  Creates a Software Platform with the given arguments
    # Arguments  :  Hardware project / Hardware Design (*.hdf / *.xml)
    # Type       :  SCW command
    #---------------------------------------------------------------------------------------#
    proc platform { args } {
	if { [llength $args] == 0 } {
	    error "Wrong # of args: should be \"platform <sub-command> \[options]\""
	}

	set pcmdlist {-generate -json -list -report -write -read -remove -active -id}
	if { [string index [lindex $args 0] 0] == "-" } {
	    if { [lsearch $args "-name"] != -1 } {
		set subcmd create
	    } elseif { [lsearch $pcmdlist [lindex $args 0]] != -1 } {
		set subcmd [string range [lindex $args 0] 1 end]
		set args [lrange $args 1 end]
	    } elseif { [lsearch $args "-help"] != -1 } {
		return [help platform]
	    } else {
		set subcmd config
	    }
	} else {
	    set subcmd [lindex $args 0]
	    set args [lrange $args 1 end]
	}

	switch -- $subcmd {
		id {
			set options {
				{detail "Returns  all IDs in detail" }
			}
			array set params [::xsdb::get_options args $options 0]
			set retval [builtin_platform -id]
			set uid_dict [::json::json2dict $retval]
			set designid [dict get $uid_dict design_id]
			set designid [format 0x%lx $designid]
			set iter 0
		    if { $params(detail)} {
				dict for {key value} $uid_dict {
					incr iter
					if {$iter == 1 } {
						continue
					}
					if {$value == 0} {
						set hex_value "N/A"
					} else {
						set hex_value [format 0x%lx $value]
					}
					dict set uid_dict $key $hex_value
				}
				return $uid_dict
			}
			return $designid
		}
	    active {
		set options {
		    {name "platform name" {args 1}}
		    {help "help command"}
		}
		array set params [::xsdb::get_options args $options 0]

		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { ![info exists params(name)] } {
		    if { [llength $args] == 1 } {
			set params(name) [lindex $args 0]
		    } elseif { [llength $args] > 1 } {
			error "Unexpected arguments: $args"
		    }
		}

		if { [info exists params(name)] } {
		    set isplatformread [builtin_platform -preactive $params(name)]
		    if { $isplatformread == "true" } {
		       set hwdb [hsi current_hw_design]
		       set dsapath [get_hw_path]
		       update_hwdb_table $dsapath $hwdb
		    }
		    return [builtin_platform -active $params(name)]
		} else {
		    return [builtin_platform -active]
		}
	    }
	    clean {
		if { [lsearch $args "-help"] != -1 } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { [llength $args] > 0 } {
		    error "Unexpected arguments: $args, should be \"platform clean\""
		}

		set args [lrange $args 1 end]		
		set workspace [::sdk::getws]
		if { $workspace != "" } {
		    set active [builtin_platform -active]
		    set platTcl [file join $workspace "$active/platform.tcl"]
		    ::fileutil::appendToFile $platTcl "platform clean\n"
		    
		    set active [builtin_platform -active]
		    if { [catch {set ret [::sdk::remove_platforms_dir [file join $workspace "$active/export/$active"]]} msg] } {
			# this can happen in case app build in xsct.
			set errorInfo ""
		    }		    
		}
		set retval [builtin_platform -clean]
		return $retval
	    }
	    config {
		set options {
		    {updatehw "new handoff file path" {args 1}}
		    {desc "description" {args 1}}
		    {arch "arch" {args 1}}
		    {samples "samples directory" {args 1}}
		    {default-domain "default domain" {args 1}}
		    {prebuilt-data "prebuilt directory" {args 1}}
		    {fsbl-elf "fsbl prebuilt binary" {args 1}}
		    {fsbl-target "fsbl target" {args 0}}
		    {pmufw-elf "pmufw prebuilt binary" {args 1}}
		    {remove-boot-bsp "remove boot bsps"}
		    {create-boot-bsp "create boot bsps"}
		    {make-local "make sw components local"}
		    {extra-compiler-flags "extra compiler flags" {args 1}}
		    {extra-linker-flags "extra linker flags" {args 1}}
		    {reset-user-defined-flags "reset user defined flags" {args 1}}
		    {report "report" {args 1}}		    
		    {help "help command"}
		}
		set saved_args $args
		array set params [::scw::get_options args [list "-extra-compiler-flags" "-extra-linker-flags"] $options 0]
		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			[lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { [builtin_platform -active] == ""} {
		    error "No active platform found"
		}

		set param ""
		set value ""
		if { [info exists params(extra-compiler-flags)] } {
		    set param [lindex $params(extra-compiler-flags) 0]
		    set apptype ""
		    if { $param == "fsbl" } {
			set apptype "-fsbloption"
		    } elseif { $param == "pmufw" } {
			set apptype "-pmufwoption"
		    } elseif { $param != "" } {
			error "Parameter specified is not supported"
		    }
		    if { [llength $saved_args] == 3 } {
			set value [lindex $saved_args  2]
		    }
		    if { [llength $saved_args] == 2 } {
			set retval [builtin_platform $apptype -report]
			set retdict [::json::json2dict $retval]
			return [dict get $retdict extra-compiler-flags]
		    }		    
		} elseif { [info exists params(extra-linker-flags)] } {
		    
		    set param [lindex $params(extra-linker-flags) 0]
		    set apptype ""
		    if { $param == "fsbl" } {
			set apptype "-fsbloption"
		    } elseif { $param == "pmufw" } {
			set apptype "-pmufwoption"
		    } elseif { $param != "" } {
			error "Parameter specified is not supported"
		    }
		    if { [llength $saved_args] == 3 } {
			set value [lindex $saved_args  2]
		    }
		    if { [llength $saved_args] == 2 } {
			set retval [builtin_platform $apptype -report]
			set retdict [::json::json2dict $retval]
			return [dict get $retdict extra-linker-flags]
		    }
		    
		} elseif { [info exists params(reset-user-defined-flags)] } {
		    set param [lindex $params(reset-user-defined-flags) 0]
		    set value [lindex $params(reset-user-defined-flags) 1]
		}

		if { $param == "fsbl" } {
		    set saved_args [linsert $saved_args 0 "-fsbloption"]
		    set fsbl_id [lsearch $saved_args "fsbl"]
		    set saved_args [lreplace $saved_args $fsbl_id $fsbl_id]
		} elseif { $param == "pmufw" } {
		    set saved_args [linsert $saved_args 0 "-pmufwoption"]
		    set pmu_id [lsearch $saved_args "pmufw"]
		    set saved_args [lreplace $saved_args $pmu_id $pmu_id]
		} elseif { $param != "" } {
		    error "Parameter specified is not supported"
		}
		if { [info exists params(report)] } {
		    if { $params(report) == "fsbl" } {
			set retval [builtin_platform -fsbloption -report]
		    } elseif { $params(report) == "pmufw" } {
			set retval [builtin_platform -pmufwoption -report]
		    } else {
			error "Parameter specified is not supported"
		    }
		    set retdict [::json::json2dict $retval]
		    set formatstr {%-25s   %s}
		    set border "[string repeat "=" 80]\n"
		    set output $border
		    append output "[format $formatstr "PROPERTY" "VALUE"]\n"
		    append output $border

		    dict for {key value} $retdict {
			if { [llength $value] > 1 } {
			    set value [join $value ","]
			}
			append output "[format $formatstr $key [string range $value 0 50]]\n"
			set len [string length $value]
			set pos 50
			while { $len - $pos > 0 } {
			    append output "[format $formatstr "" [string range $value $pos [expr $pos + 50]]]\n"
			    incr pos 51
			}
		    }
		    return $output
		}
		if { $params(create-boot-bsp) } {
		   # check if boot domains can be created.
		    builtin_scwutil -openhw
		    set hwdb [hsi current_hw_design]
		    set dsapath [get_hw_path]
		    update_hwdb_table $dsapath $hwdb

		    if { [getDeviceType] == "zynq" } {
			if { [catch { [::hsi::utils::is_app_supported_on_hw_sw -hw $dsapath -os standalone -proc ps7_cortexa9_0 -app zynq_fsbl] } msg] } {
			    error "Could not create zynq_fsbl application for the given hw design\n Details: $msg"
			}
		    }
		    if { [getDeviceType] == "zynqmp" } {			
			 set fsbltarget psu_cortexa53_0			 
			  if { [llength $saved_args] == 3 } {			  
			    set fsbltarget  [lindex $saved_args 2] 
			  }
			if { $fsbltarget != "psu_cortexa53_0"  && $fsbltarget != "psu_cortexr5_0" } {
			    error "Illegal fsbl-target  $params(fsbl-target). must be psu_cortexa53_0 / psu_cortexr5_0"
			}
			if { [info exists params(arch)]  && $params(arch) != "" } {
			    set arch_for_is_support "64"
			    if { $params(arch) == "32-bit" || $params(arch) == "32" } {
				set arch_for_is_support "32"
			    } elseif { $params(arch) == "64-bit" || $params(arch) == "64" } {
				set arch_for_is_support "64"
			    } else {
				error "Illegal arch type $params(arch). must be 32/64"
			    }
			    if { $fsbltarget == "psu_cortexr5_0" } {
				set arch_for_is_support "32"
			    }
			    if { [catch { [::hsi::utils::is_app_supported_on_hw_sw -hw $dsapath -os standalone -proc $fsbltarget -app zynqmp_fsbl -arch $arch_for_is_support] } msg] } {
				error "Could not create zynqmp_fsbl application for the given hw design\n Details: $msg"
			    }
			} else {
			    if { [catch { [::hsi::utils::is_app_supported_on_hw_sw -hw $dsapath -os standalone -proc $fsbltarget -app zynqmp_fsbl] } msg] } {
				error "Could not create zynqmp_fsbl application for the given hw design\n Details: $msg"
			    }
			}
			if { [catch { [::hsi::utils::is_app_supported_on_hw_sw -hw $dsapath -os standalone -proc psu_pmu_0 -app zynqmp_pmufw] } msg] } {
			    error "Could not create zynqmp_pmufw application for the given hw design \n Details: $msg"
			}
		    }
		}
		set retval [builtin_platform {*}$saved_args]
		if { [info exists params(updatehw)] } {		    
		    set hwdb [hsi current_hw_design]
		    set dsapath [get_hw_path]		    
		    update_hwdb_table $dsapath $hwdb
		}
		if {  $params(create-boot-bsp) } {

		    set bdomlist [builtin_platform -reportbootdoms]
		    if { $bdomlist != "" } {
			set actdom [domain active]
			foreach dom $bdomlist {
			    domain active $dom
			    set msspath [get_mss_path]
			}
			domain active $actdom
		    }
		}

		platform write		
		return $retval
	    }
	    create {
		# New platform can be created only if -name and -hw options are specified.
		# If -name is not specified, an active platform must present, in which case
		# the attributes of active platform are modified.
		# If -name is not specified and if there is no active platform, throw an error.
		set options {
		    {name "name of the software platform" {args 1}}
		    {xpfm "Platform path" {args 1}}
		    {proc "Processor" {args 1}}
		    {arch "arch" {args 1}}
		    {os "os" {args 1}}
		    {desc "description" {args 1}}
		    {hw "handoff file path" {args 1}}
		    {rm-hw "rm xsa list"  {args 1}}
		    {hw_emu "emulation hw handoff path" {args 1}}
		    {rp "slot information for dfx" {args 1}}
		    {out "output directory" {args 1}}
		    {samples "samples directory" {args 1}}
		    {prebuilt "prebuilt flow" {args 0}}
		    {no-boot-bsp "do not generate boot components" {args 0}}
		    {fsbl-target "fsbl target" {args 1}}
		    {help "command help"}
		}
		set saved_args $args
		array set params [::scw::get_options args {} $options 0]		
		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}
		set id [lsearch -exact $saved_args "-out"]
		if { $id != -1 } {
		    incr id
		    set saved_args [lreplace $saved_args $id $id [file normalize [lindex $saved_args $id]]]
		}
		if { [info exists params(xpfm)] } {
		    set workspace [getws]
		    if { ![info exists params(out)] } {
			# out is not passed. Check if workspace is set. If yes pass workspace location as out.
			if { $workspace != "" } {
			    lappend saved_args "-out"
			    lappend saved_args $workspace
			}
		    }

		    set retval [builtin_platform {*}$saved_args]
		    set hwdb [hsi current_hw_design]
		    set dsapath [get_hw_path]
		    update_hwdb_table $dsapath $hwdb

		    # handle the case of out same as ws location.
		    if { ($workspace != ""  && ![info exists params(out)] ) } {
			set platname [platform active]
			importprojects -silent $workspace/$platname
		    }
		    return $retval
		}

		if { ![info exists params(name)] } {
		    error "Specify name of the platform"
		} else {
		    if { ![info exists params(hw)]  && ![info exists params(hw_emu)] } {
			error "hw specification file not specified"
		    } else {
			# New platform declaration case. Pass all the options to the
			# builtin command to create a new platform.
			set id [lsearch -exact $saved_args "-help"]
			if { $id != -1 } {
			    set saved_args [lreplace $saved_args $id $id]
			}

			set workspace [getws]
			if { ![info exists params(out)] } {
			    # out is not passed. Check if workspace is set. If yes pass workspace location as out.
			    if { $workspace != "" } {
				lappend saved_args "-out"
				lappend saved_args $workspace
			    }
			}

			set retval [builtin_platform {*}$saved_args]			
			# In case of DC platforms, skipp updating hw db.
			if { "Datacenter" != [builtin_platform -xsatype] } {
			    set hwdb [hsi current_hw_design]
			    set dsapath [get_hw_path]
			    update_hwdb_table $dsapath $hwdb
			}
			# Creating the default system configuration.
			if { "Datacenter" == [builtin_platform -xsatype]  } {
			    set sysconfigret [builtin_system create -name "config0_0"  -display-name "Linux"  -desc "config0_0 Linux OS on x86_0" -default]
			} else {
			    set sysconfigret [builtin_system create -name $params(name) -display-name $params(name) -default]    
			}
			
			# puts "platform created."
			if { "Datacenter" != [builtin_platform -xsatype]  && "false" == [builtin_platform  -prebuilt]} {
			    #  puts " check if no bsp "
			    if { "false" == [builtin_platform  -no-boot-bsp]} {
				# check if boot domains can be created.
				if { [getDeviceType] == "zynq" } {
				    if { [catch { [::hsi::utils::is_app_supported_on_hw_sw -hw $dsapath -os standalone -proc ps7_cortexa9_0 -app zynq_fsbl] } msg] } {
					error "Could not create zynq_fsbl application for the given hw design\n Details: $msg"
				    }
				}
				if { [getDeviceType] == "zynqmp" } {
				    set fsbltarget psu_cortexa53_0				    
					if { [info exists params(fsbl-target)] } {
					    if { $params(fsbl-target) == "psu_cortexa53_0"  || $params(fsbl-target) == "psu_cortexr5_0" } {
						set fsbltarget $params(fsbl-target)
					    } else {
						error "Illegal fsbl-target  $params(fsbl-target). must be psu_cortexa53_0 / psu_cortexr5_0"
					    }					    
					}
					
				    if { [info exists params(arch)]  && $params(arch) != "" } {
					set arch_for_is_support "64"
					if { $params(arch) == "32-bit" || $params(arch) == "32" } {
					    set arch_for_is_support "32"
					} elseif { $params(arch) == "64-bit" || $params(arch) == "64" } {
					    set arch_for_is_support "64"
					} else {
					    error "Illegal arch type $params(arch). must be 32/64"
					}
					
					if { $fsbltarget == "psu_cortexr5_0" } {
					    set arch_for_is_support "32"
					}
			    
					
					if { [catch { [::hsi::utils::is_app_supported_on_hw_sw -hw $dsapath -os standalone -proc $fsbltarget -app zynqmp_fsbl -arch $arch_for_is_support] } msg] } {
					    error "Could not create zynqmp_fsbl application for the given hw design\n Details: $msg"
					}
				    } else {
					
					if { [catch { [::hsi::utils::is_app_supported_on_hw_sw -hw $dsapath -os standalone -proc $fsbltarget -app zynqmp_fsbl] } msg] } {
					    error "Could not create zynqmp_fsbl application for the given hw design\n Details: $msg"
					}
				    }
				    if { [catch { [::hsi::utils::is_app_supported_on_hw_sw -hw $dsapath -os standalone -proc psu_pmu_0 -app zynqmp_pmufw] } msg] } {
					error "Could not create zynqmp_pmufw application for the given hw design \n Details: $msg"
				    }
				}
				
				if { [info exists params(arch)]   && $params(arch) != "" } {
				    if { [info exists params(fsbl-target)] } {
					set addbdomret [builtin_platform -addbootdomains -arch $params(arch) -fsbl-target $params(fsbl-target)]
				    } else {
					set addbdomret [builtin_platform -addbootdomains -arch $params(arch) ]
				    }
				} else {
				    if { [info exists params(fsbl-target)] } {
					set addbdomret [builtin_platform -addbootdomains -fsbl-target $params(fsbl-target)]
				    } else {
					set addbdomret [builtin_platform -addbootdomains ]
				    }
				}
				set bdomlist [builtin_platform -reportbootdoms]
				if { $bdomlist != "" } {
				    set actdom [domain active]
				    foreach dom $bdomlist {
					domain active $dom
					set msspath [get_mss_path]
				    }
				    domain active $actdom
				}
				platform write
			    }
			}
			if { $workspace != ""  && ![info exists params(out)] } {
			    importprojects -silent $workspace/$params(name)
			}
			if { [info exists params(proc)]  && [info exists params(os)] } {
				set domname $params(os)_domain
				set params(proc) [get_processor_name $params(proc)]
				if { [info exists params(arch)]  && $params(arch) != "" } {
				    if { [catch { [domain create -name $domname -os $params(os) -proc $params(proc) -arch $params(arch)] } msg] } {
					platform remove $params(name)
					error "Could not create platform for $params(os) and $params(proc) with $params(arch) \n $msg"
				    }
				} else {
				    if { [catch { [domain create -name $domname -os $params(os) -proc $params(proc) ]} msg] } {
					platform remove $params(name)
					error "Could not create platform for $params(os) and $params(proc) \n $msg"
				    }
				}
				domain config -display-name "$params(os) on $params(proc)"
				set mode [builtin_scwutil -mode]

				if { $mode == "gui" && $params(os) == "linux"} {
				    ::scw::set_linux_configured "false"
				} else {
				    if { $mode != "gui" && $params(os) == "linux"} {
					::scw::set_linux_configured "true"
				    }
				}
				if { $params(os) == "linux" } {
				    ::scw::set_boot_autogen "false"
				} else {
				    ::scw::set_boot_autogen "true"
				}
				platform write
				return $msg
			}
			return $retval
		    }
		}

		if {  $params(prebuilt) == 1  } {
		    # when existing platform has to be used
		    return [builtin_platform -prebuilt]
		}
	    }
	    fsbl {
		set options {
		    {extra-compiler-flags "fsbl extra compiler flags" {args 1}}
		    {extra-linker-flags "fsbl extra linker flags" {args 1}}
		    {reset-extra-user-flags "reset the extra flags to default values" }
		    {reset-user-defined-flags "reset the extra flags to default values" }
		    {report "report"}
		    {help "help command"}
		}

		set saved_args $args
		array set params [::xsdb::get_options args $options 0]
		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			[lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { [builtin_platform -active] == ""} {
		    error "No active platform found"
		}
		if { $params(report) } {
		    set retval [builtin_platform -fsbloption -report]
		    set retdict [::json::json2dict $retval]
		    set formatstr {%-25s   %s}
		    set border "[string repeat "=" 80]\n"
		    set output $border
		    append output "[format $formatstr "PROPERTY" "VALUE"]\n"
		    append output $border

		    dict for {key value} $retdict {
			if { [llength $value] > 1 } {
			    set value [join $value ","]
			}
			append output "[format $formatstr $key [string range $value 0 50]]\n"
			set len [string length $value]
			set pos 50
			while { $len - $pos > 0 } {
			    append output "[format $formatstr "" [string range $value $pos [expr $pos + 50]]]\n"
			    incr pos 51
			}
		    }
		    return $output
		}

		#lappend saved_args "-fsbloption"
		set saved_args [linsert $saved_args 0 "-fsbloption"]
		set retval [builtin_platform {*}$saved_args]
		return $retval
	    }
	    generate {
		set options {
		    {domains "list of domains"}
		    {full "complete platform generation" }
		    {quick "quick platform generation" }
		    {help "help command"}
		}
		set saved_args $args
		if { [llength $args] == 0 } {
		    # KEYINFO  no arguments are passed to generate.
		    # this can be a call from platform project build from GUI.
		    # Should issue add to repo.
		    set workspace [::sdk::getws]
		    if { $workspace == "" } {
			set retval [builtin_platform -generate full]

			set domlist [builtin_platform -listnonlinuxdoms]
			if { $domlist != "" } {
			    set actdom [domain active]
			    foreach dom $domlist {
				domain active $dom
				set msspath [get_mss_path]
			    }
			    domain active $actdom
			}
			return $retval
		    } else {
			set retval [builtin_platform -generate full]
			set active [builtin_platform -active]
			if { [catch {set ret [::sdk::addplatforms [file join $workspace "$active/export/$active"]]} msg] } {
			    # this can happen in case app build in xsct.
			    set errorInfo ""
			}
			set domlist [builtin_platform -listnonlinuxdoms]
			if { $domlist != "" } {
			    set actdom [domain active]
			    foreach dom $domlist {
				domain active $dom
				set msspath [get_mss_path]
			    }
			    domain active $actdom
			}
			# update the platform.tcl
			set platTcl [file join $workspace "$active/platform.tcl"]
			::fileutil::appendToFile $platTcl "platform generate\n"
			return $retval
		    }

		} elseif { [lindex $args 0] == "full" || [lindex $args 0] == "-full"} {
		    # Optional argument is "-quick" this wont generate FSBL and PMUFW
		    # full will generate PMUFW and FSBL
		    # Call will be from prebuilt step. after quick.
		    # No domain additions, no need to add to repo.
		    set workspace [::sdk::getws]
		    if { $workspace == "" } {
			set retval [builtin_platform -generate full]
			set domlist [builtin_platform -listnonlinuxdoms]
			if { $domlist != "" } {
			    set actdom [domain active]
			    foreach dom $domlist {
				domain active $dom
				set msspath [get_mss_path]
			    }
			    domain active $actdom
			}
			return $retval

		    } else {
			set retval [builtin_platform -generate full]
			set active [builtin_platform -active]
			set domlist [builtin_platform -listnonlinuxdoms]
			if { $domlist != "" } {
			    set actdom [domain active]
			    foreach dom $domlist {
				domain active $dom
				set msspath [get_mss_path]
			    }
			    domain active $actdom
			}
			return $retval
		    }
		} elseif { [lindex $args 0] == "quick" || [lindex $args 0] == "-quick"} {
		    # Optional argument is "-quick" this wont generate FSBL and PMUFW
		    # Don't add to repo, as after quick JAVA code will be updating the repo.
		    return [builtin_platform -generate quick]
		} else {
		    array set params [::xsdb::get_options args $options 0]
		    if { $params(help) } {
			return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		    }
		    if { [builtin_platform -active] == ""} {
			error "No active platform found"
		    }
		    # Incremental build
		    # Should add to repo.
		    set saved_args_cp $saved_args
		    set saved_args [linsert $saved_args 0 "-generate"]
		    set retval [builtin_platform {*}$saved_args]
		    set workspace [::sdk::getws]

		    if { $workspace != "" } {
			set active [builtin_platform -active]
			# update the platform.tcl
			set platTcl [file join $workspace "$active/platform.tcl"]
			::fileutil::appendToFile $platTcl "platform generate $saved_args_cp \n"			
			if { [catch {set ret [::sdk::addplatforms [file join $workspace "$active/export/$active"]]} msg] } {
			    # this can happen in case app build in xsct.
			    set errorInfo ""
			}
			set domlist [builtin_platform -listnonlinuxdoms]
			if { $domlist != "" } {
			    set actdom [domain active]
			    foreach dom $domlist {
				domain active $dom
				set msspath [get_mss_path]
			    }
			    domain active $actdom
			}

			return $retval
		    }

		    set domlist [builtin_platform -listnonlinuxdoms]
		    if { $domlist != "" } {
			set actdom [domain active]
			foreach dom $domlist {
			    domain active $dom
			    set msspath [get_mss_path]
			}
			domain active $actdom
		    }
		    return $retval
		}

	    }
	    json {
		set args [lrange $args 1 end]
		return [builtin_platform -json]
	    }
	    list {
		set options {
			{dict "returns the platform list in tcl dict format"}
		}
		if { [lsearch $args "-help"] != -1 } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		array set params [::xsdb::get_options args $options 0]
		if { [llength $args] > 0 } {
		    error "Unexpected arguments: $args, should be \"platform list\""
		}
		set workspace [getws]
		set platform_dict ""
		if { $workspace == "" } {
		    set retval [builtin_platform -list]
		    if { $retval == "" } {
			return
		    }
		    set retdict [::json::json2dict $retval]
		    set formatstr {%-15s  %s}
		    set border "[string repeat "=" 32]\n"
		    set output $border
		    append output "[format $formatstr "NAME" "DESCRIPTION"]\n"
		    append output $border
		    set activeplat [builtin_platform -active]
		    dict for {key value} $retdict {
			if { $activeplat == $key} {
			    append output "[format $formatstr "*$key" $value]\n"
			} else {
			    append output "[format $formatstr " $key" $value]\n"
			}
		    }
		} else {
		    set formatstr {%-15s  %-13s  %s}
		    set border "[string repeat "=" 80]\n"
		    set output $border
		    append output "[format $formatstr "NAME"  "USER DEFINED"  "PATH"]\n"
		    append output $border
		    set chan [sdk::getsdkchan]

		    set user_list [sdk::xsdk_eval $chan "XSDx" getProjects "o{[dict create Type s]}" eA [list [dict create Type "platform"]]]
		    if { [lindex $user_list 0] == "" } {
			set user_list [lrange $user_list 1 end]
		    }
		    if { [lindex $user_list 0] != "" } {
			set user_projs [split [lindex $user_list 0] ";"]
		    }

		    set repo_list [sdk::xsdk_eval $chan "XSDx" getPlatforms "o{[dict create Type s]}" eA [list [dict create Type "sdk"]]]
		    if { [lindex $repo_list 0] == "" } {
			set repo_list [lrange $repo_list 1 end]
		    }
		    if { [lindex $repo_list 0] != "" } {
			set repo_projs [split [lindex $repo_list 0] ";"]
		    }

		    if { [lindex $user_list 0] == ""  && [lindex $repo_list 0] == "" } {
			return
		    }

		    if { [lindex $user_list 0] != "" } {
			foreach proj $user_projs {
			    set proj_path $workspace
			    append proj_path "/$proj/export/$proj/$proj.xpfm"
			    set proj_detail "$proj|$proj_path"
			    if { [lindex $repo_list 0] != "" } {
				if { [lsearch $repo_projs $proj_detail] != -1 } {
				    set id [lsearch -exact $repo_projs $proj_detail]
				    set repo_projs [lreplace $repo_projs $id $id]
				} else {
				    set proj_path []
				}
			    }
			    if { $params(dict) } {
				dict set platform_dict $proj "path" $proj_path  
				dict set platform_dict $proj "user_defined" "yes"
			    }
			    if { [catch {set activeplat [builtin_platform -active]} msg] } {
				append output "[format $formatstr " $proj" "YES" [string range $proj_path 0 44]]\n"
			    } else {
				if { $activeplat == $proj } {
				    append output "[format $formatstr "*$proj" "YES" [string range $proj_path 0 44]]\n"
				} else {
				    append output "[format $formatstr " $proj" "YES" [string range $proj_path 0 44]]\n"
				}
			    }
			    set len [string length $proj_path]
			    set pos 45
			    while { $len - $pos > 0 } {
				append output "[format $formatstr "" "" [string range $proj_path $pos [expr $pos + 45]]]\n"
				incr pos 46
			    }
			}
		    }

		    if { [lindex $repo_list 0] != "" } {
			foreach detail $repo_projs {
			    set proj_details [split $detail "|"]
			    set proj [lindex $proj_details 0]
			    set proj_path [lindex $proj_details 1]
			    append output "[format $formatstr " $proj" "NO" [string range $proj_path 0 44]]\n"
			    if { $params(dict) } {
				if { [dict exists $platform_dict $proj path] } {
				    dict with platform_dict $proj { lappend path $proj_path }
				} else {
				    dict set platform_dict $proj "path" $proj_path
				    dict set platform_dict $proj "user_defined" "no" 
				}
			    }
			    set len [string length $proj_path]
			    set pos 45
			    while { $len - $pos > 0 } {
				append output "[format $formatstr "" "" [string range $proj_path $pos [expr $pos + 45]]]\n"
				incr pos 46
			    }
			}
		    }
		}

		if { $params(dict) } {
		    return $platform_dict
		}
		return $output
	    }
	    pmufw {
		set options {
		    {extra-compiler-flags "pmufw extra compiler flags" {args 1}}
		    {extra-linker-flags "pmufw extra linker flags" {args 1}}
		    {reset-extra-user-flags "reset the extra flags to default values" }
		    {reset-user-defined-flags "reset the extra flags to default values" }
		    {report "report"}
		    {help "help command"}
		}

		set saved_args $args
		array set params [::xsdb::get_options args $options 0]
		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			[lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { [builtin_platform -active] == ""} {
		    error "No active platform found"
		}
		if { $params(report) } {
		    set retval [builtin_platform -pmufwoption -report]
		    set retdict [::json::json2dict $retval]
		    set formatstr {%-25s   %s}
		    set border "[string repeat "=" 80]\n"
		    set output $border
		    append output "[format $formatstr "PROPERTY" "VALUE"]\n"
		    append output $border

		    dict for {key value} $retdict {
			if { [llength $value] > 1 } {
			    set value [join $value ","]
			}
			append output "[format $formatstr $key [string range $value 0 50]]\n"
			set len [string length $value]
			set pos 50
			while { $len - $pos > 0 } {
			    append output "[format $formatstr "" [string range $value $pos [expr $pos + 50]]]\n"
			    incr pos 51
			}
		    }
		    return $output
		}
		#lappend saved_args "-pmufwoption"
		set saved_args [linsert $saved_args 0 "-pmufwoption"]
		set retval [builtin_platform {*}$saved_args]
		return $retval
	    }
	    read {
		if { [lsearch $args "-help"] != -1 } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { [llength $args] != 1 } {
		    error "Invalid arguments, should be \"platform read <platform.spr>\""
		}

		set retval [builtin_platform -read [file normalize [lindex $args 0]]]
		if { "Datacenter" != [builtin_platform -xsatype] } {
		    set hwdb [hsi current_hw_design]
		    set dsapath [get_hw_path]
		    update_hwdb_table $dsapath $hwdb
		}
		return $retval
	    }
	    remove {
		set options {
		    {name "project name" {args 1}}
		    {help "help command"}
		}
		array set params [::xsdb::get_options args $options 0]

		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}
		if { ![info exists params(name)] } {
		    if { [llength $args] == 1 } {
			set params(name) [lindex $args 0]
		    } elseif { [llength $args] == 0 } {
			set params(name) [builtin_platform -active]
		    } else {
			error "Invalid arguments, should be \"platform remove \[options\]\""
		    }
		}
		# Check if the platform name given is present in the list of platforms
		# in memory. If not, no need to do any handling of SCW level.
		# Simple remove the project.

		set platlout ""

		if { [catch { set platlout [builtin_platform -list] } msg] } {
		    removeproj $params(name)
		    return
		} else {
		    set platlout [builtin_platform -list]
		}

		set platdict [::json::json2dict $platlout]
		if { [dict size $platdict] == 0  } {
		    removeproj $params(name)
		    return
		}
		dict for {pkey pvalue} $platdict {
		    set plat $pkey
		    if { $plat == $params(name) } {
			platform active $params(name)
			set hwpath [::scw::get_hw_path]
			::scw::clear_hwdb_table $hwpath
			set workspace [sdk::getws]

			if { $workspace == "" } {
				return [builtin_platform -remove]
			} else {
				builtin_platform -remove
				removeproj $params(name)
				return
			}
		    }
		}
	    }
	    report {
		set options {
		    {name "project name" {args 1}}
		    {boot-domain "report boot-domains"}
		    {json "in json"}
		    {dict "dictionary format"}
		    {help "help command"}
		}
		array set params [::xsdb::get_options args $options 0]

		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { ![info exists params(name)] } {
		    if { [llength $args] == 1 } {
			set params(name) [lindex $args 0]
		    } elseif { [llength $args] == 0 } {
			set params(name) [builtin_platform -active]
			if { $params(name) == "" } {
			    error "No active platform exist"
			}
		    } else {
			error "Invalid arguments, should be \"platform report \[options\]\""
		    }
		}

		set workspace [sdk::getws]
		if { $workspace != "" } {
		    set chan [sdk::getsdkchan]
		    set user_list [sdk::xsdk_eval $chan "XSDx" getProjects "o{[dict create Type s]}" eA [list [dict create Type "platform"]]]
		    set user_projs [split [lindex $user_list 1] ";"]
		    set repo_list [sdk::xsdk_eval $chan "XSDx" getPlatforms "o{[dict create Type s]}" eA [list [dict create Type "sdk"]]]
		    set repo_projs [split [lindex $repo_list 1] ";"]
		    set repo_plats {}

		    foreach item $repo_projs {
			set repo_proj [split $item "|"]
			foreach value $repo_proj {
			    lappend repo_plats $value
			}
		    }

		    if { [lsearch $user_projs $params(name)] != -1 } {
			# NOP Fall through the code
		    } elseif { [lsearch $repo_plats $params(name)] != -1 } {
			return [::sdk::reportplatform $params(name)]
		    } else {
			error "Platform $params(name) does not exist"
		    }
		}

		# common code flow (with workspace and without workspace)
		set plat_active [builtin_platform -active]
		if { $params(name) != [builtin_platform -active]} {
		    builtin_platform -active $params(name)
		}
		if { $params(boot-domain) } {
		    set bdomlist [builtin_platform -reportbootdoms]
		    set retval "\["
		    for {set i 0} {$i < [llength $bdomlist]} {incr i} {
			set name [lindex $bdomlist $i]
			domain active $name
			append retval [domain report -json]
			if { $i < [expr [llength $bdomlist] -1 ] } {
			    append retval ","
			}
		    }
		    append retval "\]"
		    return $retval
		}

		set retval [builtin_platform -report]
		set retdict [::json::json2dict $retval]
		set formatstr {%-25s   %s}
		set border "[string repeat "=" 80]\n"
		set output $border
		append output "[format $formatstr "PROPERTY" "VALUE"]\n"
		append output $border

		dict for {key value} $retdict {
		    if { [llength $value] > 1 } {
			set value [join $value ","]
		    }
		    append output "[format $formatstr $key [string range $value 0 50]]\n"
		    set len [string length $value]
		    set pos 50
		    while { $len - $pos > 0 } {
			append output "[format $formatstr "" [string range $value $pos [expr $pos + 50]]]\n"
			incr pos 51
		    }
		}

		#code to extract system configurations and domain
		set syscfg [builtin_system -list]
		set syscfgd [::json::json2dict $syscfg]
		if { $syscfgd != "" } {
		    set syscfgl { }
		    dict for {key value} $syscfgd {
			lappend syscfgl $key
		    }
		    set syscfglist [join $syscfgl ","]

		    set sysactive [builtin_system -active]
		    dict for {syscfgk value} $syscfgd {
			set retval [builtin_system -active $syscfgk]
			set dom [builtin_domain -list]
			set domd [::json::json2dict $dom]
			set doml { }
			dict for {keys value} $domd {
			    lappend doml $keys
			}
			set dom1ist [join $doml ","]
			append output "[format $formatstr "Domains" [string range $dom1ist 0 59]]\n"
			set len [string length $dom1ist]
			set pos 60
			while { $len - $pos > 0 } {
			    append output "[format $formatstr "" [string range $dom1ist $pos [expr $pos + 60]]]\n"
			    incr pos 61
			}
		    }
		    builtin_platform -active $plat_active
		    builtin_system -active $sysactive
		}
		if { $params(dict) } {
		    dict append retdict "Domains" $doml
		    return $retdict
		}
		return $output
	    }
	    write {
		if { [lsearch $args "-help"] != -1 } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { [llength $args] > 0 } {
		    error "Invalid arguments, should be \"platform write\""
		}
		return [builtin_platform -write]
	    }
	    default {
		error "Wrong sub-command, use \"help platform\" for the list of sub-commands"
	    }
	}
    }
    namespace export platform
    ::xsdb::setcmdmeta platform categories {projects}
    ::xsdb::setcmdmeta platform brief {Create, configure, list, and report platforms.}
    ::xsdb::setcmdmeta platform description {
SYNOPSIS {
    platform <sub-command> [options]
        Create a platform project, or perform various other operations on
        the platform project, based on the sub-command specified.
        Following sub-commands are supported.
            active   - Set or return the active platform.
            clean    - Clean platform.
            config   - Configure the properties of a platform.
            create   - Create/define a platform.
            generate - Build the platform.
            list     - List all the platforms in workspace.
            report   - Report the details of a platform.
            read     - Read the platform settings from a file.
            remove   - Delete the platform.
            write    - Save the platform settings to a file.
        Type "help" followed by "platform sub-command", or "platform sub-command" followed
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
    active clean config create generate list report read remove write
}
}

    ::xsdb::setcmdmeta {platform active} brief {Set/get active platform.}
    ::xsdb::setcmdmeta {platform active} description {
SYNOPSIS {
    platform active [platform-name]
        Set or get the active platform. If a platform-name is specified, it is made the
        active platform. Otherwise, the name of active platform is returned. If no
        active platform exists, this command returns an empty string.
}
OPTIONS {
    None.
}
RETURNS {
    An empty string, if a platform is set as active or no active platform exists.
    The platform name, when the active platform is read.
}
EXAMPLE {
    platform active
        Return the name of the active platform.

    platform active zc702_platform
        Set zc702_platform as active platform.
}
}

    ::xsdb::setcmdmeta {platform clean} brief {Clean platform.}
    ::xsdb::setcmdmeta {platform clean} description {
SYNOPSIS {
    platform clean
        Clean the active platform in the workspace. This will
        clean all the components in platform such as FSBL, PMUFW, and so on.
}
OPTIONS {
    None.
}
RETURNS {
    Nothing. Build log will be printed on the console.
}
EXAMPLE {
    platform active zcu102
    platform clean
        Set zcu102 as the active platform and clean it.
}
}

    ::xsdb::setcmdmeta {platform config} brief {Configure the active platform.}
    ::xsdb::setcmdmeta {platform config} description {
SYNOPSIS {
    platform config [options]
        Configure the properties of active platform.
}
OPTIONS {
    -desc <description>
        Add a brief description about the platform.

    -updatehw <hw-spec>
        Update the platform to use a new hardware specification file specified
        by <hw-spec>.

    -samples <samples-dir>
        Make the application template specified in <samples-dir> part of
        the platform. This option can only be used for acceleratable applications.
        "repo -apps <platform-name>" can be used to list the application
        templates available for the given platform-name.

    -prebuilt-data <directory-name>
        For expandable platforms, pre-generated hardware data specified
        in directory-name will be used for building user applications that
        do not contain accelerators. This will reduce the build time.

    -make-local
        Make the referenced SW components local to the platform.

    -fsbl-target <processor-type>
        Processor-type for which the existing fsbl has to be re-generated.
        This option is valid only for ZU+.

    -create-boot-bsp
        Generate boot components for the platform.

    -remove-boot-bsp
        Remove all the boot components generated during platform creation.

    -fsbl-elf <fsbl.elf>
        Prebuilt fsbl.elf to be used as boot component when "remove-boot-bsp"
        option is specified.

    -pmufw-elf <pmufw.elf>
        Prebuilt pmufw.elf to be used as boot component when "remove-boot-bsp"
        option is specified.

    -extra-compiler-flags <param> <value>
        Set extra compiler flag for the parameter with a provided value.
        Only FSBL and PMUFW are the supported parameters.
        If the value is not passed, the existing value will return.

    -extra-linker-flags <param>  <value>
        Set extra linker flag for the parameter with a provided value.
        Only FSBL and PMUFW are the supported parameters.
        If the value is not passed, the existing value will return.

    -reset-user-defined-flags <param>
        Resets the extra compiler and linker flags.
        Only FSBL and PMUFW are the supported parameters.

    -report <param>
        Return the list of extra compiler and linker flags set
        to the given parameter. Only FSBL and PMUFW are the
        supported parameters.

}
RETURNS {
    Empty string, if the platform is configured successfully.
    Error string, if no platform is active or if the platform cannot be
    configured.
}
EXAMPLE {
    platform active zc702
    platform config -desc "ZC702 with memory test application"
                    -samples /home/user/newDir
        Make zc702 the active platform, configure the description of the platform,
        and make samples in the /home/user/newDir part of the platform.

    platform config -updatehw /home/user/newdesign.xsa
        Updates the platform project with the new XSA.

    platform config -fsbl-target psu_cortexr5_0
        Changes the FSBL target to psu_cortexr5_0.

    platform config -extra-compiler-flags fsbl
        Get the extra compiler flags. These are the flags added extra to the flags derived from
        the libraries, processor, and OS.

    platform config -extra-compiler-flags fsbl "-DFSBL_DEBUG_INFO [platform config
    -extra-compiler-flags fsbl]"
        Prepend -DFSBL_DEBUG_INFO to the compiler options, while building the fsbl
        application.

    platform config -report fsbl
        Return  table of extra compiler and extra linker flags that are set
        for FSBL.

    Platform config -create-boot-bsp
        Create the boot components for the platform.

    Platform config -create-boot-bsp -arch 32-bit
        Create the boot components for the platform, creating FSBL in 32-bit.
        This is valid only for Zynq UltraScale+ MPSoC based platforms.

    Platform config -remove-boot-bsp
        Remove all the boot components generated during platform creation.
}
}

    ::xsdb::setcmdmeta {platform create} brief {Create a new platform.}
    ::xsdb::setcmdmeta {platform create} description {
SYNOPSIS {
    platform create [options]
        Create a new platform by importing hardware definition file. Platform
        can also be created from pre-defined hardware platforms. Supported pre-defined
        platforms are zc702, zcu102, zc706 and zed.
}
OPTIONS {
    -name <software-platform name>
        Name of the software platform to be generated.

    -desc <description>
        Brief description about the software platform.

    -hw <handoff-file>
        Hardware description file to be used to create the platform.

    -out <output-directory>
        The directory where the software platform needs to be created.
        If the workspace is set, this option should not be used.
        Use of this option will prevent the usage of platform in Vitis IDE.

    -prebuilt
        Mark the platform to be built from already built software artifacts.
        This option should be used only if you have existing software
        platform artifacts.

    -proc <processor>
        The processor to be used; the tool will create the default domain.

    -arch <processor architecture>
        32-bit or 64-bit, this is valid only for the A53 processor.	

    -samples <samples-directory>
        Make the samples in <samples-directory>, part of the platform.

    -os <os>
        The OS to be used. The tool will create the default domain. This works in
        combination with -proc option.

    -xpfm <platform-path>
        Existing platform from which the projects have to be imported and
        made part of the current platform.

    -no-boot-bsp
        Mark the platform to build without generating boot components.

    -arch <arch-type>
        Processor architecture, <arch-type> can be 32 or 64 bits.
        This option is used to build the project with 32/64 bit toolchain.
}
RETURNS {
    Empty string, if the platform is created successfully.
    Error string, if the platform cannot be created.
}
EXAMPLE {
    platform create -name "zcu102_test" -hw zcu102
        Defines a software platform for a pre-defined hardware desciption file.

    platform create -name "zcu102_test" -hw zcu102 -proc psu_cortexa53_0 -os standalone
        Defines a software platform for a pre-defined hardware desciption file.
        Create a default domain with standalone os running on psu_cortexa53_0.

    platform create -name "zcu102_32bit" -hw zcu102 -proc psu_cortexa53_0 -arch 32-bit
     -os standalone
        Defines a software platform for a pre-defined hardware desciption file.
        Create a default domain with standalone os running on psu_cortexa53_0
        in 32-bit mode.

    platform create -name "zcu102_test" -hw zcu102 -proc psu_cortexa53 -os linux
     -arch 32-bit
        Defines a software platform for a pre-defined hardware desciption file.
        Create a default domain with linux os running on psu_cortexa53 in 32-bit.

    platform create -xpfm /path/zc702.xpfm
        This will create a platform project for the platform pointed by the xpfm file.

    platform create -name "ZC702Test" -hw /path/zc702.xsa
        Defines a software platform for a hardware desciption file.
}
}

    ::xsdb::setcmdmeta {platform generate} brief {Build a platform.}
    ::xsdb::setcmdmeta {platform generate} description {
SYNOPSIS {
    platform generate
        Build the active platform and add it to the repository. The platform
        must be created through platform create command, and must be selected
        as active platform before building.
}
OPTIONS {
    -domains <domain-list>
        List of domains which need to be built and added to the repository.
        Without this option, all the domains that are part of the plafform are
        built.
}
RETURNS {
    Empty string, if the platform is generated successfully.
    Error string, if the platform cannot be built.
}
EXAMPLE {
    platform generate
        Build the active platform and add it to repository.

    platform generate -domains a53_standalone,r5_standalone
        Build only a53_standalone,r5_standalone domains and add it
        to the repository.
}
}

    ::xsdb::setcmdmeta {platform list} brief {List the platforms.}
    ::xsdb::setcmdmeta {platform list} description {
SYNOPSIS {
    List the platforms in the workspace and repository.
}
OPTIONS {
    -dict
        List all the platforms for the workspace in Tcl dictionary format.
        Without this option, platforms are listed in tabular format.
}
RETURNS {
    List of platforms, or "No active platform present" string if no platforms exist.
}
EXAMPLE {
    platform list
        Return a list of all the platforms in the workspace and repository in tabular format.

    platform list -dict
        Return a list of all the platforms in the workspace and repository in Tcl dictionary format.
}
}

    ::xsdb::setcmdmeta {platform read} brief {Read from the platform file.}
    ::xsdb::setcmdmeta {platform read} description {
SYNOPSIS {
    platform read [platform-file]
        Reads platform settings from the platform file and makes it available for
        edit. The platform file is created during the creation of platform itself
        and it contains all details of the platform such as hardware specification file,
        processor information, and so on.
}
OPTIONS {
    None.
}
RETURNS {
    Empty string, if the platform is read successfully.
    Error string, if the platform file cannot be read.
}
EXAMPLE {
    platform read <platform.spr>
        Reads the platform from the platform.spr file.
}
}

    ::xsdb::setcmdmeta {platform remove} brief {Delete a platform.}
    ::xsdb::setcmdmeta {platform remove} description {
SYNOPSIS {
    platform remove <platform-name>
        Delete the given platform. If the platform-name is not specified, the active
        platform is deleted.
}
OPTIONS {
    None.
}
RETURNS {
    Empty string, if the platform is deleted successfully.
    Error string, if the platform cannot be deleted.
}
EXAMPLE {
    platform remove zc702
        Removes zc702 platform from the disk.
}
}

    ::xsdb::setcmdmeta {platform report} brief {Report the details of the active platform.}
    ::xsdb::setcmdmeta {platform report} description {
SYNOPSIS {
    platform report
        Returns details such as domains and processors created in the active platform.
}
OPTIONS {
    None.
}
RETURNS {
    Table with details of active platform, or error string if no platforms exist.
}
EXAMPLE {
    platform report
        Returns a table with details of the active platform.
}
}

    ::xsdb::setcmdmeta {platform write} brief {Write platform settings to a file.}
    ::xsdb::setcmdmeta {platform write} description {
SYNOPSIS {
    platform write
        Writes the platform settings to platform.spr file. It can be read back
        using the "platform read" command.
}
OPTIONS {
    None.
}
RETURNS {
    Empty string, if the platform settings are written successfully.
    Error string, if the platform settings cannot be written.
}
EXAMPLE {
    platform write
        Writes platform to platform.spr file.
}
}

    #---------------------------------------------------------------------------------------#
    # Create a system configuration
    # Description:  Creates a system configuration  with the given arguments
    # Arguments  :
    # Type       :  SCW command
    #---------------------------------------------------------------------------------------#
    proc system { args } {
	variable help_prefix

	set options {
	    {name "name of the system configuration" {args 1}}
	    {display-name "display name" {args 1}}
	    {desc "description" {args 1}}
	    {boot "boot directory" {args 1}}
	    {readme "readme file" {args 1}}
	    {default "default config" {args 0}}
	    {remove "remove the system configuration" {args 1}}
	    {active "active system configuration" }
	    {list "list the system configurations in the active platform" {args 0}}
	    {report "report" {args 0}}
	    {help "command help"}
	}

	set cmdlist {-list -report -remove -active}

	if { [string index [lindex $args 0] 0] == "-" } {
	    if { [lsearch $args "-name"] != -1 } {
		set subcmd create
	    } elseif { [lsearch $cmdlist [lindex $args 0]] != -1 } {
		set subcmd [string range [lindex $args 0] 1 end]
		set args [lrange $args 1 end]
	    } elseif { [lsearch $args "-help"] != -1 } {
		return [help sysconfig]
	    } else {
		set subcmd config
	    }
	} else {
	    set subcmd [lindex $args 0]
	    set args [lrange $args 1 end]
	}
	return [sysconfig $subcmd {*}$args]
    }
    namespace export system

    #---------------------------------------------------------------------------------------#
    # Create a system configuration
    # Description:  Creates a system configuration  with the given arguments
    # Arguments  :
    # Type       :  SCW command
    #---------------------------------------------------------------------------------------#
    proc sysconfig { args } {
	puts "WARNING: sysconfig command is DEPRECATED.\n"
	puts "Only one system configuration will be allowed in a platform."
	puts "If no system configuration is present, creating a domain will create the system configuration.\n"
	if { [llength $args] == 0 } {
	    error "Wrong # of args: should be \"sysconfig <sub-command> \[options]\""
	}

	set subcmd [lindex $args 0]

	if { [lindex $args 0] == "-help" } {
	    return [help sysconfig]
	}

	set args [lrange $args 1 end]
	switch -- $subcmd {
	    active {
		set options {
		    {name "system configuration name" {args 1}}
		    {help "help command"}
		}
		array set params [::xsdb::get_options args $options 0]

		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { ![info exists params(name)] } {
		    if { [llength $args] == 1 } {
			set params(name) [lindex $args 0]
		    } elseif { [llength $args] > 1 } {
			error "Invalid arguments, should be \"sysconfig active \[options\]\""
		    }
		}

		if { [info exists params(name)] } {
		    return [builtin_system -active $params(name)]
		} else {
		    return [builtin_system -active]
		}
	    }
	    config {
		set options {
		    {display-name "display name" {args 1}}
		    {desc "description" {args 1}}
		    {readme "readme file" {args 1}}
		    {boot "boot directory" {args 1}}
		    {bif "bif file" {args 1}}
		    {help "command help"}
		}
		set saved_args $args
		array set params [::xsdb::get_options args $options 0]
		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		set activesys [builtin_system -active]
		if { $activesys == ""} {
		    error "No active system configuration exists"
		}

		if { [info exists  params(bif)] } {
		    return [builtin_boot -bif $params(bif)]
		}

		return [builtin_system {*}$saved_args]
	    }
	    create {
		set options {
		    {name "name of the system configuration" {args 1}}
		    {desc "description" {args 1}}
		    {default "default config" {args 0}}
		    {display-name "display name" {args 1}}
		    {boot "boot directory" {args 1}}
		    {readme "readme file" {args 1}}
		    {help "command help"}
		}
		set saved_args $args
		array set params [::xsdb::get_options args $options 0]
		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { [info exists params(name)] } {
		    # create new sysconfig
		    if { ![info exists params(desc)] } {
			lappend saved_args {*}[list "-desc" $params(name)]
		    }
		    if { ![info exists params(display-name)] } {
			lappend saved_args {*}[list "-display-name" $params(name)]
		    }

		    # making latest sysconfig as default
		    lappend saved_args "-default"

		    return [builtin_system {*}$saved_args]
		} else {
		    error "Invalid arguments, specify name to create sysconfig"
		}
	    }
	    list {
		if { [lsearch $args "-help"] != -1 } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		set params(dict) 0
		if { [lsearch $args "-dict"] != -1 } {
		    set params(dict) 1
		} elseif { [llength $args] > 0 } {
		    error "Unexpected arguments: $args, should be \"sysconfig list\""
		}
		set retval [builtin_system -list]
		set retdict [::json::json2dict $retval]
		if {$retdict == ""} {
		    error "No system configuration exists, create a new one"
		}

		set sys_list {}
		if { $params(dict)} {
		    dict for {key value} $retdict {
			set sys_dict [dict create "Name" $key]
			lappend sys_list $sys_dict
		    }
		    return $sys_list
		}
		set formatstr {%-15s     %s}
		set border "[string repeat "=" 40]\n"
		set output $border
		append output "[format $formatstr "NAME" "DESCRIPTION"]\n"
		append output $border

		set activesys [builtin_system -active]
		dict for {key value} $retdict {
		    if { $activesys == $key} {
			append output "[format $formatstr "*$key" $value]\n"
		    } else {
			append output "[format $formatstr " $key" $value]\n"
		    }
		}
		return $output
	    }
	    remove {
		set options {
		    {name "project name" {args 1}}
		    {help "help command"}
		}
		array set params [::xsdb::get_options args $options 0]

		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { ![info exists params(name)] } {
		    if { [llength $args] == 1 } {
			set params(name) [lindex $args 0]
		    } elseif { [llength $args] == 0 } {
			set params(name) [builtin_system -active]
		    } else {
			error "Invalid arguments, should be \"sysconfig remove \[options\]\""
		    }
		}

		set curactive [sysconfig active]

		if { [catch { [builtin_system -active $params(name)] } msg] } {
		    error $msg
		}

		set retval [builtin_system -remove]

		if { $curactive != $params(name) } {
			sysconfig active $curactive
		}
		return $retval
	    }
	    report {
		set options {
		    {name "project name" {args 1}}
		    {help "help command"}
		}
		array set params [::xsdb::get_options args $options 0]

		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { ![info exists params(name)] } {
		    if { [llength $args] == 1 } {
			set params(name) [lindex $args 0]
			builtin_system -active $params(name)
		    } elseif { [llength $args] == 0 } {
			set params(name) [builtin_system -active]
			if { $params(name) == "" } {
			    error "No active system configuration exist"
			}
		    } else {
			error "Invalid arguments, should be \"sysconfig report \[options\]\""
		    }
		} else {
		    builtin_system -active $params(name)
		}

		set retval [builtin_system -report]
		set retdict [::json::json2dict $retval]
		set formatstr {%-15s     %s}
		set border "[string repeat "=" 80]\n"
		set output $border
		append output "[format $formatstr "PROPERTY" "VALUE"]\n"
		append output $border

		dict for {key value} $retdict {
		    append output "[format $formatstr $key $value]\n"
		}
		set platname [builtin_platform -active]
		append output "[format $formatstr "Platform" $platname]\n"
		set dom [builtin_domain -list]
		set domd [::json::json2dict $dom]
		set doml {}
		dict for {key value} $domd {
		    lappend doml $key
		}
		set domlist [join $doml ","]
		append output "[format $formatstr "Domains" [string range $domlist 0 59]]\n"
		set len [string length $domlist]
		set pos 60
		while { $len - $pos > 0 } {
		    append output "[format $formatstr "" [string range $domlist $pos [expr $pos + 60]]]\n"
		    incr pos 61
		}

		return $output
	    }
	    default {
		error "Wrong sub-command, use \"help sysconfig\" for the list of sub-commands"
	    }
	}
    }
    namespace export sysconfig

     proc get_options {argListVar hyphonOpts optList {strict 1}} {
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
	    set hyphonoptname {}
	    foreach opt $optList {
		set name -[lindex $opt 0]
		set flags [lindex $opt 2]
		
		if { [string equal -length $arglen $arg $name] } {		    
		    if { [string equal $arg $name] } {
			#Found exact match, hence resetting.
			set matchnames {}
			set matchopt {}
			lappend matchnames $name
			lappend matchopt $opt
			#puts "dbg - found match $name  with opt $opt"
			if { $hyphonoptname == "" && [lsearch $hyphonOpts $name] != -1 } {
			    set hyphonoptname $name
			}
			break
		    }
		    if { [dict exists $flags deprecated] && [dict get $flags deprecated] == 1} {
			puts "warning: $name is deprecated as it is not required, it will be removed in future"
		    }
		    lappend matchopt $opt
		    lappend matchnames $name
		}
	    }
	    #puts " dbg -- matchnames is $matchnames "
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
		    set unique 0
		    for {set i 0} {$i < $matchcount} {incr i} {
			if { $arg == [lindex $matchnames $i] } {
			    set matchopt [lindex $matchopt $i]
			    set unique 1
			    break
			}
		    }
		    if { $unique != 1 } {
			error "ambiguous option '$arg': [join $matchnames]"
		    }
		}
	    } else {
		set matchopt [lindex $matchopt 0]
	    }

	    set nargs 0
	    set flags [lindex $matchopt 2]
	    if { [dict exists $flags args] } {
		set nargs [dict get $flags args]
	    }
	    if { $nargs == 0 } {
		set value 1
	    } elseif { [llength $arglist] <= $nargs } {
		error "option [lindex $arglist 0] require $nargs arguments"
	    } elseif { $nargs == 1 } {
		set value [lindex $arglist 1]
	    } elseif { $nargs == "n" } {
		set value var_args
	    } else {
		set value [lrange $arglist 1 $nargs]
	    }
	    dict set result [lindex $matchopt 0] $value
	    set arglist [lrange $arglist 1+$nargs end]
	    if { [lsearch $hyphonOpts $hyphonoptname] != -1 } {
		set opt1 [string range $hyphonoptname 1 end]
		set temp [dict get $result $opt1]
		append temp " [lindex $arglist 0]"
		dict set result $opt1 $temp
		set arglist [lrange $arglist 1 end]
	    }
	}
	if { $strict && [llength $arglist] } {
	    error "unexpected arguments: $arglist"
	}
	return $result
    }


    #---------------------------------------------------------------------------------------#
    # Create a Domain
    # Description:  Creates a Domain with the given arguments
    # Arguments  :  Hardware project / Hardware Design (*.hdf / *.xml)
    # Type       :  SCW command
    #---------------------------------------------------------------------------------------#
    proc domain { args } {
	if { [llength $args] == 0 } {
	    error "Wrong # of args: should be \"domain <sub-command> \[options]\""
	}

	set cmdlist {-list -report -remove -active}
	if { [string index [lindex $args 0] 0] == "-" } {
	    if { [lsearch $args "-help"] != -1 } {
		return [help domain]
	    } else {
		#puts "\nWarning:: This usecase is deprecated. Use \"domain -help\" for the new usage"
		if { [lsearch $args "-name"] != -1 } {
		    set subcmd create
		} elseif { [lsearch $cmdlist [lindex $args 0]] != -1 } {
		    set subcmd [string range [lindex $args 0] 1 end]
		    set args [lrange $args 1 end]
		} else {
		    set subcmd config
		}
	    }
	} else {
	    set subcmd [lindex $args 0]
	    set args [lrange $args 1 end]
	}

	switch -- $subcmd {
	    active {
		set options {
		    {name "domain name" {args 1}}
		    {help "help command"}
		}
		array set params [::xsdb::get_options args $options 0]

		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { ![info exists params(name)] } {
		    if { [llength $args] == 1 } {
			set params(name) [lindex $args 0]
		    } elseif { [llength $args] > 1 } {
			error "Invalid arguments, should be \"domain active <options>\""
		    }
		}

		if { [info exists params(name)] } {
		    return [builtin_domain -active $params(name)]
		} else {
		    return [builtin_domain -active]
		}
	    }
	    config {
		set options {
		    {display-name "display name" {args 1}}
		    {desc "description" {args 1}}
		    {qemu-args "qemu args file" {args 1}}
		    {qemu-data "qemu data " {args 1}}
		    {pmuqemu-args "pmu quemu args file" {args 1}}
		    {pmcqemu-args "pmc quemu args file" {args 1}}
		    {generate-bif "generate bif" {args 0}}
		    {hw-boot-bin "hw flow boot.bin" {args 1}}
		    {mss "MSS file" {args 1}}
		    {runtime "runtime" {args 1}}
		    {image "Image directory" {deprecated 1 args 1}}
		    {sd-dir "SD card directory" {args 1}}
		    {bootmode "boot mode" {args 1}}
		    {rootfs "rootfs file" {args 1}}
		    {sysroot "Sysroot directory" {args 1}}
		    {sw-repo "sw repo" {args 1}}
		    {prebuilt-data "prebuilt directory" {args 1}}
		    {readme "readme file" {args 1}}
		    {inc-path "include path" {args 1}}
		    {lib-path "library path" {args 1}}
		    {auto-generate-linux "automatically create linux artifacts"}
		    {boot "boot directory" {args 1}}
		    {bif "bif file" {args 1}}
		    {help "command help"}
		}
		set saved_args $args
		array set params [::scw::get_options args "" $options 0]

		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}
		if { [info exists params(image)] } {
		    puts "warning: -image option is deprecated. Please use -sd-dir option"
		}

		set activedomain [builtin_domain -active]
		if { $activedomain == ""} {
		    error "No active domain exists"
		}
		set domainos [getosfordomain $activedomain]
		if { [info exists params(inc-path)] && [info exists params(lib-path)] } {
		    return [builtin_library -inc-path $params(inc-path) -lib-path $params(lib-path)]
		} elseif { [info exists params(inc-path)] } {
		    return [builtin_library -inc-path $params(inc-path)]
		} elseif { [info exists params(lib-path)] } {
		    return [builtin_library -lib-path $params(lib-path)]
		} elseif { [info exists params(boot)] } {
		    if { $domainos != "linux" } {
			puts "WARNING: boot option is obsolate for non-linux domains."
			return
		    }
		    return [builtin_system -boot $params(boot)]
		} elseif { [info exists params(bif)] } {
		    if { $domainos != "linux" } {
			puts "WARNING: bif option is obsolate for non-linux domains."
			return
		    }
		    return [builtin_boot -bif $params(bif)]
		}

		# To keep this code for reference. #for_config...
		if {  $params(auto-generate-linux) } {
		    if { $domainos != "linux" }  {
			error "\"auto-generate-linux\" command is valid only for domain with os as linux"
		    }
		    set imagegiven [info exists  params(image) ]
		    set isprebuilt [builtin_platform -prebuilt]
		    if { $isprebuilt == "false" } {
			if { $::tcl_platform(os) != "Linux" } {
			    error "Linux domain can not be created on Windows host machine"
			}
			set instpath [plnx-install-path]
			if { $instpath == "" } {
			    set mode [builtin_scwutil -mode]
			    if { $mode == "gui"} {
			    # this message will get printed only for GUI
			    set outmsg "To create Linux based domains, petalinux is required. Set the petalinux path in \
			    Window->Preferences->Xilinx->Project Preference(Petalinux install Location)."
			    error $outmsg
			    } else {
			    set outmsg "To create Linux based domains, petalinux is required. Set the petalinux path using \
			    the command plnx-install-path."
			    error $outmsg
			   }
			}
		    }
		}
		return [builtin_domain {*}$saved_args]
	    }
	    create {
		set options {
		    {name "name of the domain" {args 1}}
		    {desc "description" {args 1}}
		    {default "default domain " {args 0}}
		    {proc "list of processors" {args 1}}
		    {support-app "app name" {args 1}}
		    {hw-boot-bin "hw flow boot.bin" {args 1}}
		    {os "os to be hosted" {default "standalone" args 1}}
		    {arch "32/64 bit" {default "" args 1}}
		    {display-name "display name" {args 1}}
		    {runtime "runtime" {args 1}}
		    {bootmode "linux bootmode" {args 1}}
		    {rootfs "rootfs file" {args 1}}
		    {image "Image directory" {deprecated 1 args 1}}
		    {sd-dir "SD card directory" {args 1}}
		    {sysroot "Sysroot directory" {args 1}}
		    {guest-on-hypervisor "hypervisor guest" {args 0}}
		    {auto-generate-linux "automatically create linux artifacts"}
		    {help "command help"}
		}
		set saved_args $args
		array set params [::xsdb::get_options args $options 0]
		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { [info exists params(image)] } {
		    puts "warning: -image option is deprecated. Please use -sd-dir option"
		}
		# New domain can be created only if -name, -proc and -os  options are specified.
		# If -name is not specified, an active domain must present, in which case
		# the attributes of active platform are modified.
		# If -name is not specified and if there is no active platform, throw an error.
		if { ![info exists params(name)] } {
		    error "Domain name not specified"
		}
		if { ![info exists params(proc)] } {
		    error "Processor instance not specified"
		}
		if { [lsearch $saved_args "-os"] == -1 } {
		    lappend saved_args {*}[list "-os" $params(os)]
		}
		set proc_name $params(proc)
	    	set params(proc) [get_processor_name $params(proc)]
		set saved_args [regsub $proc_name $saved_args $params(proc)]
		if { [info exists params(support-app)] } {
		    if { [isvalid_app_template $params(support-app)] == 1 } {
			set app_name $params(support-app)
			# converting app name from user format to hsi format
		    } else {
		       set app_name [get_app_template  $params(support-app)]
		       if { $app_name == "" } {
			    error "Invalid application template name $params(support-app), use repo -apps \
			    to know valid app templates."
		       }
		    }
		    set saved_args [regsub $params(support-app) $saved_args $app_name]
		}
		# To keep this code for reference. #for_config...
		if { $params(auto-generate-linux) } {
		    if { $params(os) == "linux" } {
			set imagegiven [info exists  params(image)]
			set isprebuilt [builtin_platform -prebuilt]
			if { $isprebuilt == "false" } {
			    if { $::tcl_platform(os) != "Linux" } {
				error "Linux domain can not be created on Windows host machine"
			    }
			    set instpath [plnx-install-path]
			    if { $instpath == "" } {
				set mode [builtin_scwutil -mode]
				if { $mode == "gui"} {
				# this message will get printed only for GUI
				set outmsg "To create Linux based domains, petalinux is required. Set the petalinux path in \
				Window -> Preferences -> Xilinx SDx -> Platform Project -> Petalinux Install Location."
				error $outmsg
				} else {
				    set outmsg "To create Linux based domains, petalinux is required. Set the petalinux path using \
				    the command plnx-install-path."
				    error $outmsg
				}
			    }
		       }
		    }
		}
		if { $params(guest-on-hypervisor) == 1 } {
		    set map {"-guest-on-hypervisor" "-guest-on-hypervisor true"}
		    set saved_args [string map $map $saved_args]
		}

		# Set this as default domain, change later
		lappend saved_args "-default"
		if { [catch { set actsysconfig [builtin_system -active] } msg] } {
		    set sysconfigret [builtin_system create -name sysconfig1 -display-name sysconfig1 -default]
		}
		set retval [builtin_domain {*}$saved_args]
		if { $params(os) != "linux" && $params(os) != "aie_runtime" } {
		    #builtin_bsp -regenerate
		    platform generate -domains
		    platform write
		    set domlist [builtin_platform -listnonlinuxdoms]
		    if { $domlist != "" } {
			set actdom [domain active]
			foreach dom $domlist {
			    domain active $dom
			    set msspath [get_mss_path]
			}
			domain active $actdom
		    }
		    set msspath [get_mss_path]
		}
		return $retval
	    }
	    list {
	  	set options {
		    {dict "returns the domain list in tcl dict format" }
		}
		set saved_args $args
		array set params [::xsdb::get_options args $options 0]
		if { [lsearch $args "-help"] != -1 } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		
		if { [llength $args] > 0 } {
		    error "Unexpected arguments: $args, should be \"domain list\""
		}

		set retval [builtin_domain -list]
		if { $retval == "" } {
		    return "No domains exist"
		}
		set retdict [::json::json2dict $retval]

		set formatstr {%-22s %-22s %-22s}
		set border "[string repeat "=" 65]\n"
		set output $border
		append output "[format $formatstr "NAME" "PROCESSOR" "OS" ]\n"
		append output $border
		if { [catch { set activedom [builtin_domain -active] } msg] } {
			set activedom ""
		}
		dict for {key value} $retdict {
			dict with value {
				if { $activedom == $key} {
					append output "[format $formatstr "*$key"  $processor $os]\n"
				} else {
					append output "[format $formatstr "$key"  $processor $os]\n"
				}
			}
		}
		if { $params(dict)} {
			return $retdict
		}
		return $output
	    }
	    remove {
		set options {
		    {name "project name" {args 1}}
		    {help "help command"}
		}
		array set params [::xsdb::get_options args $options 0]

		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { ![info exists params(name)] } {
		    if { [llength $args] == 1 } {
			set params(name) [lindex $args 0]
		    } elseif { [llength $args] == 0 } {
			set params(name) [builtin_domain -active]
		    } else {
			error "Invalid arguments, should be \"domain remove \[options\]\""
		    }
		}

		if { [catch { [builtin_domain -active $params(name)] } msg] } {
		    error $msg
		}
		set retval [builtin_domain -remove]
		platform write
		if { [catch { [platform generate -domains] } msg] } {
		    puts "NOTE: platform has no domains to update."
		}
		platform write
		return $retval
	    }
	    report {
		set options {
		    {name "project name" {args 1}}
		    {json "json format" }
		    {dict "dictionary format"}
		    {help "help command"}
		}
		array set params [::xsdb::get_options args $options 0]

		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { ![info exists params(name)] } {
		    if { [llength $args] == 1 } {
			set params(name) [lindex $args 0]
			builtin_domain -active $params(name)
		    } elseif { [llength $args] == 0 } {
			set params(name) [builtin_domain -active]
			if { $params(name) == "" } {
			    error "No active domain exist"
			}
		    } else {
			error "Invalid arguments, should be \"domain report <options>\""
		    }
		} else {
		    builtin_domain -active $params(name)
		}

		if { $params(json) } {
		    return [builtin_domain -json]
		}
		set retval [builtin_domain -report]
		set retdict [::json::json2dict $retval]
		set formatstr {%-21s     %s}
		set border "[string repeat "=" 50]\n"
		set output $border
		append output "[format $formatstr "PROPERTY" "VALUE"]\n"
		append output $border
		dict for {key value} $retdict {
		    append output "[format $formatstr $key $value]\n"
		}

		set platname [builtin_platform -active]
		set syscfgname [builtin_system -active]
		append output "[format $formatstr "Platform" $platname]\n"

		if { $params(dict) } {
		    dict append retdict "Platform" $platname
		    return $retdict
		}

		return $output
	    }
	    default {
		error "Wrong sub-command, use \"help domain\" for the list of sub-commands"
	    }
	}
    }
    namespace export domain
    ::xsdb::setcmdmeta domain categories {projects}
    ::xsdb::setcmdmeta domain brief {Create, configure, list and report domains.}
    ::xsdb::setcmdmeta domain description {
SYNOPSIS {
    domain <sub-command> [options]
        Create a domain, or perform various other operations on the domain,
        based on the sub-command specified.
        Following sub-commands are supported.
            active - Set/get the active domain.
            config - Configure the properties of a domain.
            create - Create a domain in the active platform.
            list   - List all the domains in active platform.
            report - Report the details of a domain.
            remove - Delete a domain.
        Type "help" followed by "app sub-command", or "app sub-command" followed
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
    active config create list remove report
}
}

    ::xsdb::setcmdmeta {domain active} brief {Set/Get the active domain}
    ::xsdb::setcmdmeta {domain active} description {
SYNOPSIS {
    domain active [domain-name]
        Set or get the active domain. If domain-name is specified, it is made the
        active domain. Otherwise, the name of the active domain is returned. If no
        active domain exists, this command returns an empty string.
}
OPTIONS {
    None.
}
RETURNS {
    Empty string, if a domain is set as active or no active domain exists.
    Domain name, when active domain is read.
}
EXAMPLE {
    domain active
        Return the name of the active domain.

    domain active test_domain
        Set test_domain as active domain.
}
}

    ::xsdb::setcmdmeta {domain config} brief {Configure the active domain.}
    ::xsdb::setcmdmeta {domain config} description {
SYNOPSIS {
    domain config [options]
        Configure the properties of active domain.
}
OPTIONS {
    -display-name <display name>
        Display name of the domain.

    -desc <description>
        Brief description of the domain.

    -sd-dir <location>
        For domain with Linux as OS, use pre-built Linux images from this
        directory, while creating the PetaLinux project. This option is valid
        only for Linux domains.
    
    -generate-bif
       Generate a standard bif for the domain. Domain report shows the
       location of the generated bif. This option is valid only for Linux
       domains.
       

    -sw-repo <repositories-list>
        List of repositories to be used to pick software components like drivers
        and libraries while generating this domain. Repository list should be a
        tcl list of software repository paths.

    -mss <mss-file>
       Use mss from specified by <mss-file>, instead of generating mss file for
       the domain.

    -readme <file-name>
        Add a README file for the domain, with boot instructions and so on.

    -inc-path <include-path>
        Additional include path which should be added while building the
        application created for this domain.

    -lib-path <library-path>
        Additional library search path which should be added to the linker
        settings of the application created for this domain.

    -sysroot <sysroot-dir>
        The Linux sysroot directory that should be added to the platform. This
        sysroot will be consumed during application build.

    -boot <boot-dir>
        Directory to generate components after Linux image build.

    -bif <file-name>
        Bif file used to create boot image for Linux boot.

    -qemu-args <file-name>
        File with all PS QEMU args listed. This is used to start PS QEMU.

    -pmuqemu-args <file-name>
        File with all PMC QEMU args listed. This is used to start PMU QEMU.

    -pmcqemu-args <file-name>
        File with all pmcqemu args listed. This is used to start pmcqemu.

    -qemu-data <data-dir>
        Directory which has all the files listed in file-name provided as part of
        qemu-args and pmuqemu-args options.
}
RETURNS {
    Empty string, if the domain is configured successfully.
    Error string, if no domain is active or if the domain cannot be configured.
}
EXAMPLE {
    domain config -display-name zc702_MemoryTest
                  -desc "Memory test application for Zynq"
        Configure display name and description for
        the active domain.

    domain config -image "/home/user/linux_image/"
        Create PetaLinux project from pre-built Linux image.

    domain -inc-path /path/include/ -lib-path /path/lib/
        Adds include and library search paths to the domain's application build
        settings.
}
}

    ::xsdb::setcmdmeta {domain create} brief {Create a new domain.}
    ::xsdb::setcmdmeta {domain create} description {
SYNOPSIS {
    domain create [options]
        Create a new domain in active platform.
}
OPTIONS {
    -name <domain-name>
        Name of the domain.

    -display-name <display_name>
        The name to be displayed in the report for the domain.

    -desc <description>
        Brief description of the domain.

    -proc <processor>
        Processor core to be used for creating the domain. For SMP Linux, this
        can be a Tcl list of processor cores.

    -arch <processor architecture>
        32-bit or 64-bit. This option is valid only for A53 processors.

    -os <os>
        OS type. Default type is standalone.

    -support-app <app-name>
        Create a domain with BSP settings needed for application specified by
        <app-name>. This option is valid only for standalone domains.
        The "repo -apps" command can be used to list the available application.

    -auto-generate-linux
        Generate the Linux artifacts automatically.

    -sd-dir <location>
        For domain with Linux as OS, use pre-built Linux images from this
        directory, while creating the PetaLinux project. This option is valid
        only for Linux domains.

    -sysroot <sysroot-dir>
        The Linux sysroot directory that should be added to the platform.
        This sysroot will be consumed during application build.
}
RETURNS {
    Empty string, if the domain is created successfully.
    Error string, if the domain cannot be created.
}
EXAMPLE {
    domain create -name "ZUdomain" -os standalone -proc psu_cortexa53_0
                  -support-app {Hello World}
        Create a standalone domain and configure settings needed for
        a Hello World template application.

    domain create -name "SMPLinux" -os linux
                  -proc {ps7_cortexa9_0 ps7_cortexa9_1}
        Create a Linux domain named SMPLinux for processor cores ps7_cortexa9_0
        ps7_cortexa9_1 in the active platform.

    domain create -name a53_0_Standalone -os standalone
                -proc psu_cortexa53_0 -arch 32-bit
        Create a standalone domain for a53_0 processor for 32-bit mode.
}
}

    ::xsdb::setcmdmeta {domain list} brief {List domains.}
    ::xsdb::setcmdmeta {domain list} description {
SYNOPSIS {
    domain list
        List domains in the active platform.
}
OPTIONS {
    -dict
        List all the domains for the active platform in Tcl dictionary format.
        Without this option, domains are listed in tabular format.
}
RETURNS {
    List of domains in the active platform, or empty string if no
    domains exist.
}
EXAMPLE {
    platform active
        platform1

    domain list
        Display all the domain created in platform1 in tabular format.

    domain list -dict
        Display all the domain created in platform1 in Tcl dictionary format.      
}
}

    ::xsdb::setcmdmeta {domain remove} brief {Delete a domain.}
    ::xsdb::setcmdmeta {domain remove} description {
SYNOPSIS {
    domain remove [domain-name]
        Delete a domain from active platform. If a domain-name is not
        specified, the active domain is deleted.
}
OPTIONS {
    None.
}
RETURNS {
    Empty string, if the domain is deleted successfully.
    Error string, if the domain deletion fails.
}
EXAMPLE {
    domain remove test_domain
        Removes test_domain from the active platform.
}
}

    ::xsdb::setcmdmeta {domain report} brief {Report the details of a domain.}
    ::xsdb::setcmdmeta {domain report} description {
SYNOPSIS {
    domain report [domain-name]
        Return details such as platform, processor core, OS, and so on.
        of a domain. If domain-name is not specified, details of the active
        domain are reported.
}
OPTIONS {
    None.
}
RETURNS {
    Table with details of a domain, if domain-name or active domain exists.
    Error string, if active domain does not exist and domain-name is not
    specified.
}
EXAMPLE {
    domain report
        Return a table with details of the active domain.
}
}


    #---------------------------------------------------------------------------------------#
    # Modifies Linker flags
    # Description:  Able to modify linker flags with the given arguments
    # Arguments  :  Hardware project / Hardware Design (*.hdf / *.xml)
    # Type       :  SCW command
    #---------------------------------------------------------------------------------------#
    proc linker { args } {
	puts "WARNING: linker command is not supported. Linker settings are managed in application."
	if { [llength $args] == 0 } {
	    error "Wrong # of args: should be \"linker <sub-command> \[options]\""
	}

	if { [lindex $args 0] == "-help" } {
	    return [help linker]
	}

	set subcmd [lindex $args 0]
	set args [lrange $args 1 end]
	switch -- $subcmd {
	    config {
		set options {
		    {compiler-flags "compiler flags" {args 1}}
		    {linker-flags "linker flags" {args 1}}
		    {lscript "linker script" {args 1}}
		    {heap-size "heap size" {args 1}}
		    {stack-size "stack size" { args 1}}
		    {code-mem "code sections memory" {args 1}}
		    {data-mem "data sections memory" {args 1}}
		    {stack-heap-mem "stack and heap section's memory" {args 1}}
		    {json "report data in json" {args 0}}
		    {help "command help"}
		}
		set saved_args $args
		array set params [::xsdb::get_options args $options]

		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { [llength $saved_args] == 0 } {
		    error "Invalid arguments, should be \"linker config <options>\""
		}

		if { [info exists  params(lscript)] } {
		    return [builtin_app -lscript $params(lscript)]
		}

		return [builtin_app {*}$saved_args]
	    }
	    list-mem {
		if { [lsearch $args "-help"] != -1 } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { [llength $args] > 0 } {
		    error "Invalid arguments, should be \"linker list-mem\""
		}

		return [builtin_app -list-mem]
	    }
	    report {
		if { [lsearch $args "-help"] != -1 } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { [llength $args] > 1 } {
		    error "Invalid arguments, should be \"linker report\""
		}

		set retval [builtin_app -report]
		if { [lindex $args 0] == "-json" } {
		    return $retval
		} else {
		    set retdict [::json::json2dict $retval]
		    set formatstr {%-15s     %s}
		    set border "[string repeat "=" 40]\n"
		    set output $border
		    append output "[format $formatstr "PROPERTY" "VALUE"]\n"
		    append output $border
		    dict for {key value} $retdict {
			append output "[format $formatstr $key $value]\n"
		    }
		    return $output
		}
	    }
	    default {
		error "Wrong sub-command, use \"help linker\" for the list of sub-commands"
	    }
	}
    }
    namespace export linker


    #---------------------------------------------------------------------------------------#
    # Configure BSP
    # Description: BSP configuration parameters
    # Arguments  : Hardware Design (HDF/XML), Software Design (MSS) and other options
    # Type	 : XSCT Command
    #---------------------------------------------------------------------------------------#
    proc bsp { args } {
	if { [llength $args] == 0 } {
	    error "Wrong # of args: should be \"bsp <sub-command> \[options]\""
	}
	if { [lindex $args 0] == "-help" } {
	    return [help bsp]
	}

	set subcmd [lindex $args 0]
	set args [lrange $args 1 end]
	switch -- $subcmd {
	    config {
		set options {
		    {append "append to exisiting value"}
		    {help "help text"}
		}
		array set params [::xsdb::get_options args $options 0]

		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { ![llength $args] || [llength $args] > 2 } {
		    error "Wrong # of args: should be \"bsp config <param> <value>\""
		}

		set optionlist {}

		if { [llength $args] == 2 } {
		    if { $params(append) } {
			lappend optionlist "-append"
			lappend optionlist {*}[list [lindex $args 0] "-value" [lindex $args 1]]
		    } else {
			lappend optionlist {*}[list "-option" [lindex $args 0] "-value" [lindex $args 1]]
		    }
		} elseif { [llength $args] == 1 } {
		    if { $params(append) } {
			error "Parameter value not specified with -append"
		    }
		    lappend optionlist {*}[list "-option" [lindex $args 0]]
		}
		return [builtin_bsp -config {*}$optionlist]
	    }
	    getdrivers {
		set options {
		    {dict "return data in dict format"}
		    {help "help text"}
		}
		array set params [::xsdb::get_options args $options 1]
		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		set retval [builtin_bsp -getdrivers]
		set retdict [::json::json2dict $retval]
		if { $params(dict) } {
		    return $retdict
		}

		set formatstr {%-20s     %-20s      %-7s}
		set border "[string repeat "=" 80]\n"
		set output $border
		append output "[format $formatstr "IP" "DRIVER" "VERSION"]\n"
		append output $border
		dict for {key value} $retdict {
		    set value [split $value ":"]
		    append output "[format $formatstr $key [lindex $value 0] [lindex $value 1]]\n"
		}
		return $output
	    }
	    getlibs {
		set options {
		    {dict "return data in dict format"}
		    {help "help text"}
		}
		array set params [::xsdb::get_options args $options 1]
		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		set retval [builtin_bsp -getlibs ]
		set retdict [::json::json2dict $retval]
		if { $retdict == "" } {
		    error "No libraries added to BSP"
		}
		if { $params(dict) } {
		    return $retdict
		}

		set formatstr {%-20s     %s}
		set border "[string repeat "=" 40]\n"
		set output $border
		append output "[format $formatstr "LIBRARY" "VERSION"]\n"
		append output $border
		dict for {key value} $retdict {
		    append output "[format $formatstr $key $value]\n"
		}
		return $output
	    }
	    getos {
		set options {
		    {dict "return data in dict format"}
		    {help "help text"}
		}
		array set params [::xsdb::get_options args $options 1]
		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		set retval [builtin_bsp -getos]
		set retdict [::json::json2dict $retval]
		if { $params(dict) } {
		    return $retdict
		}

		set formatstr {%-20s     %s}
		set border "[string repeat "=" 40]\n"
		set output $border
		append output "[format $formatstr "OS" "VERSION"]\n"
		append output $border
		dict for {key value} $retdict {
		    append output "[format $formatstr $key $value]\n"
		}
		return $output
	    }
	    listparams {
		set options {
		    {proc "Processor"}
		    {os "os"}
		    {lib "library to be listed" {args 1}}
		    {help "help command"}
		}
		set saved_args $args
		array set params [::xsdb::get_options args $options 0]

		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { $params(proc) + [info exists params(lib)] + $params(os) < 1 } {
		    error "Invalid arguments, should be \"bsp listparams \[options\]\""
		}

		if { $params(proc) + [info exists params(lib)] + $params(os) > 1 } {
		    error "Conflicting options specified"
		}

		if {  $params(proc) || $params(os) } {
		    if { [llength $args] > 0 } {
			error "Invalid arguments, this option doesn't take any arguments"
		    }
		}

		if { [info exists params(lib)] } {
		    set liblist [::hsi::get_libs]
		    if { [lsearch $liblist $params(lib)] == -1 } {
			error "$params(lib) not found in the BSP"
		    }
		}

		set retval [builtin_bsp -listparam {*}$saved_args]
		set retdict [::json::json2dict $retval]
		set formatstr {%-30s  %s}
		set border "[string repeat "=" 80]\n"
		set output $border
		append output "[format $formatstr "PARAMETER" "VALUE"]\n"
		append output $border
		dict for {key value} $retdict {
		    append output "\n[format $formatstr $key [string range $value 0 49]]"
		    set len [string length $value]
		    set pos 50
		    while { $len - $pos > 0 } {
			append output "\n[format $formatstr "" [string range $value $pos [expr $pos + 50]]]"
			incr pos 51
		    }
		}
		return $output
	    }
	    regenerate {
		if { [lsearch $args "-help"] != -1 } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { [llength $args] > 0 } {
		    error "Invalid arguments, should be \"bsp regenerate \""
		}

		return [builtin_bsp -regenerate]
	    }
	    write {
		if { [lsearch $args "-help"] != -1 } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { [llength $args] > 0 } {
		    error "Invalid arguments, should be \"bsp regenerate \""
		}

		return [sdx_write_mss]
	    }
	    reload {
		if { [lsearch $args "-help"] != -1 } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { [llength $args] > 0 } {
		    error "Invalid arguments, should be \"bsp regenerate \""
		}

		return [sdx_reload_mss]
	    }
	    removelib {
		set options {
		    {name "library name" {args 1}}
		    {help "help command"}
		}
		array set params [::xsdb::get_options args $options 0]

		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { ![info exists params(name)] } {
		    if { [llength $args] == 1 } {
			set params(name) [lindex $args 0]
		    } elseif { [llength $args] == 0 } {
			error "Library name not specified"
		    } else {
			error "Unexpected arguments: $args"
		    }
		}

		set optionlist { }
		lappend optionlist {*}[list "-lib" $params(name) ]
		return [builtin_bsp -removelib {*}$optionlist]
	    }
	    setdriver {
		set options {
		    {ip "peripheral" {args 1}}
		    {driver "driver name" {args 1}}
		    {ver "driver version" {default "latest" args 1}}
		    {help "help command"}
		}
		array set params [::xsdb::get_options args $options 0]

		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { ![info exists params(ip)] || ![info exists params(driver)] } {
		    error "Invalid arguments, IP and driver names are mandatory"
		}

		set optionlist {}
		lappend optionlist {*}[list "-ip" $params(ip)  "-driver" $params(driver) "-ver" $params(ver)]
		return [builtin_bsp -setdriver {*}$optionlist]
	    }
	    setlib {
		set options {
		    {name "library name" {args 1}}
		    {ver "library version" {default "latest" args 1}}
		    {help "help command"}
		}
		array set params [::xsdb::get_options args $options 0]

		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { ![info exists params(name)] } {
		    if { [llength $args] == 1 } {
			set params(name) [lindex $args 0]
		    } elseif { [llength $args] == 0 } {
			error "Library name not specified"
		    } else {
			error "Unexpected arguments: $args"
		    }
		}
		set optionlist { }
		lappend optionlist {*}[list "-lib" $params(name) "-ver" $params(ver)]
		return [builtin_bsp -setlib {*}$optionlist]
	    }
	    setosversion {
		set options {
		    {ver "driver version" {args 1}}
		    {help "help command"}
		}
		array set params [::xsdb::get_options args $options 0]

		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { [llength $args] > 1 } {
		    error "Invalid arguments, should be \"bsp setosversion -ver \[ver\] \""
		}
		if { [info exists params(ver)] } {
		    return [builtin_bsp -setosversion $params(ver)]
		}
	    }
	    default {
		error "Wrong sub-command, use \"help bsp\" for the list of sub-commands"
	    }
	}
    }
    namespace export bsp
    ::xsdb::setcmdmeta bsp categories {projects}
    ::xsdb::setcmdmeta bsp brief {Configure BSP settings of a baremetal domain.}
    ::xsdb::setcmdmeta bsp description {
SYNOPSIS {
    bsp <sub-command> [options]
        Configure the BSP settings, including the library, driver, and OS version of a
        active domain,  based on the sub-command specified.
        Following sub-commands are supported.
            config       - Modify the configurable parameters of BSP settings.
            getdrivers   - List IP instance and its driver.
            getlibs      - List the libraries from BSP settings.
            getos        - List os details from BSP settings.
            listparams   - List the configurable parameters of os/proc/library.
            regenerate   - Regenerate BSP sources.
            reload       - Revert the BSP settings to the earlier saved state.
            write        - Save the BSP edits.
            removelib    - Remove library from bsp settings.
            setdriver    - Sets the driver for the given IP instance.
            setlib       - Sets the given library.
            setosversion - Sets version for the given OS.
        Type "help" followed by "bsp sub-command", or "bsp sub-command" followed
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
    config getdrivers getlibs getos listparams regenerate removelib setdriver setlib setosversion
}
}

    ::xsdb::setcmdmeta {bsp config} brief {Configure parameters of BSP settings.}
    ::xsdb::setcmdmeta {bsp config} description {
SYNOPSIS {
    bsp config <param> <value>
        Set/get/append value to the configurable parameters.
        If <param> is specified and <value> is not specified, return the value of
        the parameter.
        If <param>  and <value> are specified, set the value of parameter.
        Use "bsp list-params <-os/-proc/-driver>" to know configurable
        parameters of OS/processor/driver.
}
OPTIONS {
    -append <param> <value>
        Append the given value to the parameter.
}
RETURNS {
    Nothing, if the parameter is set/appended successfully.
    Current value of the paramter if <value> is not specified.
    Error string, if the parameter cannot be set/appended.
}
EXAMPLE {
    bsp config -append extra_compiler_flags "-pg"
        Append -pg to extra_compiler_flags.

    bsp config stdin
        Return the current value of stdin.

    bsp config stdin ps7_uart_1
        Set stdin to ps7_uart_1 .
}
}

    ::xsdb::setcmdmeta {bsp getdrivers} brief {List drivers.}
    ::xsdb::setcmdmeta {bsp getdrivers} description {
SYNOPSIS {
    bsp getdrivers
        Return the list of drivers assigned to IP in BSP.
}
OPTIONS {
    -dict
        Return the result as <IP-name driver:version> pairs.
}
RETURNS {
    Table with IP, its corresponding driver, and driver version.
    Empty string, if there are no IPs.
}
EXAMPLE {
    bsp getdrivers
        Return the list of IPs and their drivers.
}
}

    ::xsdb::setcmdmeta {bsp getlibs} brief {List libraries added in the BSP settings.}
    ::xsdb::setcmdmeta {bsp getlibs} description {
SYNOPSIS {
    bsp getlibs
        Display list of libraries added in the BSP settings.
}
OPTIONS {
    -dict
        Return the result as <lib-name version> pairs.
}
RETURNS {
    List of libraries. Empty string, if no libraries are added.
}
EXAMPLE {
    bsp getlibs
        Return the list of libraries added in bsp settings of active domain.
}
}

    ::xsdb::setcmdmeta {bsp getos} brief {Display OS details from BSP settings.}
    ::xsdb::setcmdmeta {bsp getos} description {
SYNOPSIS {
    bsp getos
        Displays the current OS and version.
}
OPTIONS {
    -dict
        Return the result as <os-name version> pair.
}
RETURNS {
    OS name and version.
}
EXAMPLE {
    bsp getos
        Return OS name and version from the BSP settings of the active domain.
}
}

    ::xsdb::setcmdmeta {bsp listparams} brief {List the configurable parameters of the BSP.}
    ::xsdb::setcmdmeta {bsp listparams} description {
SYNOPSIS {
    bsp listparams <option>
        List the configurable parameters of the <option>.
}
OPTIONS {
    -lib <lib-name>
        Return the configurable parameters of the library in BSP.

    -os
        Return the configurable parameters of the OS in BSP.

    -proc
        Return the configurable parameters of the processor in BSP.
}
RETURNS {
    Parameter names. Empty string, if no parameters exist.
}
EXAMPLE {
    bsp listparams -os
        List all the configurable parameters of OS in the BSP settings.
}
}

    ::xsdb::setcmdmeta {bsp removelib} brief {Remove library from BSP settings.}
    ::xsdb::setcmdmeta {bsp removelib} description {
SYNOPSIS {
    bsp removelib -name <lib-name>
        Remove the library from BSP settings of the active domain.
        The library settings will come into effect only when the platform is
        generated. Settings can also be saved by running 'bsp write' if the
        user wishes to exit xsct without generating platform and revisit later.
}
OPTIONS {
    -name <lib-name>
        Library to be removed from BSP settings. This is the default option, so
        lib-name can be directly specified as an argument without using
        this option.
}
RETURNS {
    Nothing, if the library is removed successfully.
    Error string, if the library cannot be removed.
}
EXAMPLE {
    bsp removelib -name xilffs
        Remove xilffs library from BSP settings.
}
}

    ::xsdb::setcmdmeta {bsp reload} brief {Revert to the earlier saved BSP settings.}
    ::xsdb::setcmdmeta {bsp reload} description {
SYNOPSIS {
    bsp reload
        Reloads the BSP settings to the earlier saved state.
        BSP settings are saved with bsp write command.
}
OPTIONS {
    None.
}
RETURNS {
    Nothing, if the BSP is reloaded successfully.
    Error string, if the BSP cannot be reloaded.
}
EXAMPLE {
    bsp reload
        Reloads the BSP settings to the earlier saved state.
}
}

::xsdb::setcmdmeta {bsp write} brief {Save the BSP settings.}
    ::xsdb::setcmdmeta {bsp write} description {
SYNOPSIS {
    bsp write
        Save the changes to the BSP settings so that user can revisit later.
}
OPTIONS {
    None.
}
RETURNS {
    Nothing, if the changes to the BSP settings are saved successfully.
    Error string, if the BSP settings cannot be saved.
}
EXAMPLE {
    bsp write
        Save changes to the BSP settings.
}
}

::xsdb::setcmdmeta {bsp regenerate} brief {Regenerate BSP sources.}
    ::xsdb::setcmdmeta {bsp regenerate} description {
SYNOPSIS {
    bsp regenerate
        Regenerate the sources with the modifications made to the BSP.
}
OPTIONS {
    None.
}
RETURNS {
    Nothing, if the BSP is generated successfully.
    Error string, if the BSP cannot be generated.
}
EXAMPLE {
    bsp regenerate
        Regenerate the BSP sources with the changes to the BSP settings applied.
}
}

    ::xsdb::setcmdmeta {bsp setdriver} brief {Set the driver to IP.}
    ::xsdb::setcmdmeta {bsp setdriver} description {
SYNOPSIS {
    bsp setdriver [options]
        Set specified driver to the IP core in BSP settings of active domain.
}
OPTIONS {
    -driver <driver-name>
        Driver to be assigned to an IP.

    -ip <ip-name>
        IP instance for which the driver has to be added.

    -ver <version>
        Driver version.
}
RETURNS {
    Nothing, if the driver is set successfully.
    Error string, if the driver cannot be set.
}
EXAMPLE {
    bsp setdriver -ip ps7_uart_1 -driver generic -ver 2.0
        Set the generic driver for the ps7_uart_1 IP instance for the BSP.
}
}

    ::xsdb::setcmdmeta {bsp setlib} brief {Adds the library to the BSP settings.}
    ::xsdb::setcmdmeta {bsp setlib} description {
SYNOPSIS {
    bsp setlib [options]
        Queues the library for addition to the active BSP. 'bsp write' will
        commit the queued libraries to the mss. The newly added libraries
        become available to the application projects after the platform is
        generated.
        If the user wants to build the platform from GUI without committing the
        queued libraries to mss, then the project must be cleaned first.
}
OPTIONS {
    -name <lib-name>
        Library to be added to the BSP settings. This is the default option, so
        lib-name can be directly specified as an argument without using
        this option.

    -ver <version>
        Library version.
}
RETURNS {
    Nothing, if the library is set successfully.
    Error string, if the library cannot be set.
}
EXAMPLE {
    bsp setlib -name xilffs
        Add the xilffs library to the BSP settings.
}
}

    ::xsdb::setcmdmeta {bsp setosversion} brief {Set the OS version.}
    ::xsdb::setcmdmeta {bsp setosversion} description {
SYNOPSIS {
    bsp setosversion [options]
        Set OS version in the BSP settings of the active domain.
        Latest version is added by default.
}
OPTIONS {
    -ver <version>
        OS version.
}
RETURNS {
    Nothing, if the OS version is set successfully.
    Error string, if the OS version cannot be set.
}
EXAMPLE {
    bsp setosversion -ver 6.6
        Set the OS version 6.6 in the BSP settings of the active domain.
}
}


    #---------------------------------------------------------------------------------------#
    # Configures a Library
    # Description:  Configures a Library with the given arguments
    # Arguments  :  Library/path of the library
    # Type       :  SCW command
    #---------------------------------------------------------------------------------------#
    proc library { args } {
	if { [llength $args] == 0 } {
	    error "Wrong # of args: should be \"library <sub-command> \[options]\""
	}

	if { [string index [lindex $args 0] 0] == "-" } {
	    if { [lsearch $args "-help"] != -1 } {
		return [help library]
	    } else {
		return [library_old {*}$args]
	    }
	} else {
	    set subcmd [lindex $args 0]
	    set args [lrange $args 1 end]
	}

	switch -- $subcmd {
	    build {
		set options {
		    {name "library project name" {args 1}}
		    {help "command help"}
		}
		array set params [::xsdb::get_options args $options 0]

		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { ![info exists params(name)] } {
		    if { [llength $args] == 1 } {
			set params(name) [lindex $args 0]
		    } elseif { [llength $args] != 1 } {
			error "Invalid arguments, specify name of the library project"
		    }
		}

		set chan [sdk::getsdkchan]
		sdk::xsdk_eval $chan XSDx build "o{[dict create Type s Name s]}" e [list [dict create Type "library" Name $params(name)]]
		return
	    }
	    clean {
		set options {
		    {name "library project name" {args 1}}
		    {help "command help"}
		}
		array set params [::xsdb::get_options args $options 0]

		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { ![info exists params(name)] } {
		    if { [llength $args] == 1 } {
			set params(name) [lindex $args 0]
		    } elseif { [llength $args] != 1 } {
			error "Invalid arguments, specify name of the library project"
		    }
		}

		set chan [sdk::getsdkchan]
		sdk::xsdk_eval $chan XSDx clean "o{[dict create Type s Name s]}" e [list [dict create Type "library" Name $params(name)]]
		return
	    }
	    config {
		set options {
		    {name "name of the library" {args 1}}
		    {set "set a param value" {default 0}}
		    {get "get the param value"}
		    {add "add to a param value"}
		    {remove "delete a param value"}
		    {info "more info of param value"}
		    {help "help command"}
		}
		array set params [::xsdb::get_options args $options 0]

		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { $params(set) + $params(add) + $params(remove) + $params(info) + $params(get) > 1 } {
		   error "Conflicting options specified"
		}

		set chan [sdk::getsdkchan]
		set defs [lindex [sdk::xsdk_eval $chan SDxBuildSettings getDefinitions s "eA" [list ""]] 1]
		set pnames [lsort [dict keys $defs]]
		if { [llength $args] == 0 } {
		    set result ""
		    foreach pname $pnames {
			if { $result != "" } {
			    append result "\n"
			}
			append result [format "  %-30s %s" $pname [::xsdb::dict_get_safe $defs $pname description]]
		    }
		    return $result
		}

		if { [llength $args] > 2 } {
		    error "Unexpected arguments: $args. should be \"library config \[name\] \[options\] \[value\]\""
		}
		set pname [lsearch -all -inline -glob $pnames "[lindex $args 0]*"]
		if { [llength $pname] != 1 } {
		    if { [llength $pname] == 0 } {
			set pname $pnames
		    }
		    error "Unknown or ambiguous parameter \"[lindex $args 0]\": must be [join $pname {, }]"
		}
		set pname [lindex $pname 0]

		if { [info exists params(name)] } {
		    if { [lsearch [::sdk::getprojects] $params(name)] == -1 } {
			error "Library project '$params(name)' doesn't exist in the workspace\nuse 'library list' to get a list of library projects in workspace"
		    }
		} else {
		    error "Library name not specified"
		}

		set lib_props [lindex [sdk::xsdk_eval $chan "XSDx" "reportApp" "o{AppName s}" eA [list [dict create AppName $params(name) Type library]]] 1]
		if { [::xsdb::dict_get_safe $lib_props "LibraryType"] == "Static Lib" && \
		    [lsearch [list "libraries" "library-search-path" "linker-script" "undef-compiler-symbols"] $pname] != -1 } {
		    error "\"$pname\" setting is not supported for static library \"$params(name)\""
		}

		if { [llength $args] == 2 } {
		    if { $params(info) || $params(get) } {
			error "'-info' and '-get' options are not supported while setting a parameter value"
		    }

		    set value [lindex $args 1]
		    set props [::xsdb::dict_get_safe $defs $pname props]
		    if { $params(set) + $params(add) + $params(remove) == 0 } {
			set params([lindex $props 0]) 1
		    }

		    foreach prop [array names params] {
			if { $params($prop) == 1 } {
			    if { [lsearch $props $prop] == -1 } {
				error "Parameter $pname doesn't support $prop operation"
			    }
			    sdk::xsdk_eval $chan SDxBuildSettings $prop sss e [list $params(name) $pname $value]
			    return
			}
		    }
		}

		if { $params(info) } {
		    set result [format "  %-20s : %s\n" "Possible Values" [::xsdb::dict_get_safe $defs $pname values]]
		    append result [format "  %-20s : %s\n" "Possible Operations" [regsub -all { } [::xsdb::dict_get_safe $defs $pname props] {, }]]
		    append result [format "  %-20s : %s" "Default Operation" [lindex [::xsdb::dict_get_safe $defs $pname props] 0]]
		    return $result
		}

		return [lindex [sdk::xsdk_eval $chan SDxBuildSettings get ss eA [list $params(name) $pname]] 1]
	    }
	    create {
		set options {
		    {name "project name" {args 1}}
		    {type "library type" {args 1}}
		    {platform "platform project name" {args 1}}
		    {domain "domain name" {args 1}}
		    {sysproj "system project name" {args 1}}
		    {lang "project launguage" {default "c" args 1}}
		    {help "help command"}
		}
		array set params [::xsdb::get_options args $options 0]

		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { ![info exists params(type)] } {
		    error "Invalid arguments, library type not specified"
		}

		if { $params(type) != "static" && $params(type) != "shared"} {
		    error "Invalid library type, should be either \"static\" or \"shared\""
		}

		if { [info exists params(name)] } {
		    if { [info exists params(sysproj)] } {
			if { ![info exists params(domain)] } {
			    set activedom [builtin_domain -active]
			    if { $activedom == ""} {
				error "No active domain exists, specify a domain"
			    } else {
				set params(domain) $activedom
			    }
			}
			set chan [sdk::getsdkchan]
			set ret_list [sdk::xsdk_eval $chan "XSDx" getProjects "o{[dict create Type s]}" eA [list [dict create Type system]]]
			if { [lindex $ret_list 1] != "" } {
			    set projlist [split [lindex $ret_list 1] ";"]
			    if { [lsearch $projlist $params(sysproj)] == -1 } {
				error "System project doesnt exist"
			    }
			} else {
			    error "No system project exists"
			}
		    } else {
			if { ![info exists params(platform)] } {
			    set activeplat [builtin_platform -active]
			    if { $activeplat == ""} {
				error "No active platform exists, specify a platform"
			    } else {
				set params(platform) $activeplat
			    }
			}
			if { ![info exists params(domain)] } {
			    set activedom [builtin_domain -active]
			    if { $activedom == ""} {
				error "No active domain exists, specify a domain"
			    } else {
				set params(domain) $activedom
			    }
			}
		    }
		} else {
		    error "Invalid arguments, should be \"library create \[options\]\""
		}

		if { [info exists params(name)] } {
		    if { [info exists params(platform)] && [info exists params(domain)]} {
			set fmt [dict create Name s Platform s System s Domain s Language s TemplateApp s OutPutFormat s]
			set data [dict create Name $params(name) Platform $params(platform) System "" Domain $params(domain) \
					 Language [string toupper $params(lang)] TemplateApp "" OutPutFormat $params(type)]
			set chan [sdk::getsdkchan]
			sdk::xsdk_eval $chan "XSDx" createAppFromPlatform "o{$fmt}" e [list $data]
		    } elseif { [info exists params(sysproj)] } {
			set fmt [dict create Name s Sysproj s Domain s Language s TemplateApp s OutPutFormat s]
			set data [dict create Name $params(name) Sysproj $params(sysproj) Domain $params(domain) \
					 Language [string toupper $params(lang)] TemplateApp "" OutPutFormat $params(type)]
			set chan [sdk::getsdkchan]
			sdk::xsdk_eval $chan "XSDx" createAppToSysProj "o{$fmt}" e [list $data]
		    }
		}
		return
	    }
	    list {
		if { [lsearch $args "-help"] != -1 } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { [llength $args] > 0} {
		    error "Invalid arguments, should be \"library list\""
		}

		set chan [sdk::getsdkchan]
		set ret_list [sdk::xsdk_eval $chan "XSDx" getProjects "o{[dict create Type s]}" eA [list [dict create Type library]]]

		if { [lindex $ret_list 1] != "" } {
		    regsub -all {\;} [lindex $ret_list 1] "\n" projs
		} else {
		    error "No library project exist"
		}

		set formatstr {%15s}
		set border "[string repeat "=" 40]\n"
		set output $border
		append output "     APPLICATION\n"
		append output $border
		append output $projs
		return $output
	    }
	    remove {
		set options {
		    {name "project name" {args 1}}
		    {help "help command"}
		}
		array set params [::xsdb::get_options args $options 0]

		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { ![info exists params(name)] } {
		    if { [llength $args] == 1 } {
			set params(name) [lindex $args 0]
		    } else {
			error "Invalid arguments, should be \"library remove \<options>\""
		    }
		}

		set wksp "false"
		#if { $params(workspace-only) == 1 } {
		#    set wksp "true"
		#}

		set chan [sdk::getsdkchan]
		set retval [sdk::xsdk_eval $chan "XSDx" deleteProjects "o{[dict create Name s Workspace s]}" e \
				[list [dict create Name $params(name) Workspace $wksp Type library]]]
		if { [lindex $retval 0] != "" } {
		    error $retval
		}
	    }
	    report {
		set options {
		    {name "project name" {args 1}}
		    {help "help command"}
		}
		array set params [::xsdb::get_options args $options 0]

		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { ![info exists params(name)] } {
		    if { [llength $args] == 1 } {
			set params(name) [lindex $args 0]
		    } else {
			error "Invalid arguments, should be \"library report <project-name>\""
		    }
		}

		set chan [sdk::getsdkchan]
		set retval [sdk::xsdk_eval $chan "XSDx" "reportApp" "o{AppName s}" eA [list [dict create AppName $params(name) Type library]]]
		set formatstr {%-20s   %s}
		set border "[string repeat "=" 80]\n"
		set output $border
		append output "[format $formatstr "PROPERTY" "VALUE"]\n"
		append output $border
		dict for {mainkey mainvalue} $retval {
		    dict for {key value} $mainvalue {
			append output "[format $formatstr $key [string range $value 0 54]]\n"
			set len [string length $value]
			set pos 55
			while { $len - $pos > 0 } {
			    append output "[format $formatstr "" [string range $value $pos [expr $pos + 55]]]\n"
			    incr pos 56
			}
		    }
		}
		return $output
	    }
	    default {
		error "Wrong sub-command, use \"help library\" for the list of sub-commands"
	    }
	}
    }
    namespace export library
    ::xsdb::setcmdmeta library categories {projects}
    ::xsdb::setcmdmeta library brief {Library project management}
    ::xsdb::setcmdmeta library description {
SYNOPSIS {
    library <sub-command> [options]
        Create a library project, or perform various other operations on
        the library project, based on the sub-command specified.
        Following sub-commands are supported.
            build  - Build the library project.
            clean  - Clean the library project.
            config - Configure C/C++ build settings of the library project.
            create - Create a library project.
            list   - List all the library projects in workspace.
            remove - Delete the library project.
            report - Report the details of the library project.
        Type "help" followed by "library sub-command", or "library sub-command" followed
        by "-help" for more details.
}
OPTIONS {
    Depends on the sub-command.
}
RETURNS {
    Depends on the sub-command.
}
EXAMPLE {
    See sub-command help for examples.
}
SUBCMDS {
    build clean create list remove report
}
}

    ::xsdb::setcmdmeta {library build} brief {Build library project.}
    ::xsdb::setcmdmeta {library build} description {
SYNOPSIS {
    library build -name <project-name>
        Build the library project specified by <project-name> in the workspace. "-name"
        switch is optional, so <project-name> can be specified directly, without
        using -name.
}
OPTIONS {
    -name <project-name>
        Name of the library project to be built.
}
RETURNS {
    Nothing, if the library project is built successfully.
    Error string, if the library project build fails.
}
EXAMPLE {
    library build -name lib1
        Build lib1 library project.
}
}

    ::xsdb::setcmdmeta {library clean} brief {Clean library project.}
    ::xsdb::setcmdmeta {library clean} description {
SYNOPSIS {
    library clean -name <project-name>
        Clean the library project specified by <project-name> in the workspace. "-name"
        switch is optional, so <project-name> can be specified directly, without
        using -name.
}
OPTIONS {
    -name <project-name>
        Name of the library project to be clean built.
}
RETURNS {
    Nothing, if the library project is cleaned successfully.
    Error string, if the library project build clean fails.
}
EXAMPLE {
    library clean -name lib1
        Clean lib1 library project.
}
}

    ::xsdb::setcmdmeta {library config} brief {Configure C/C++ build settings of the library.}
    ::xsdb::setcmdmeta {library config} description {
SYNOPSIS {
    Configure C/C++ build settings for the specified library.
    THe following settings can be configured for libraries:
          assembler-flags         : Miscellaneous flags for assembler
          build-config            : Get/set build configuration
          compiler-misc           : Compiler miscellaneous flags
          compiler-optimization   : Optimization level
          define-compiler-symbols : Define symbols. Ex. MYSYMBOL
          include-path            : Include path for header files
          libraries               : Libraries to be added while linking
          library-search-path     : Search path for the libraries added
          linker-misc             : Linker miscellaneous flags
          linker-script           : Linker script for linking
          undef-compiler-symbols  : Undefine symbols. Ex. MYSYMBOL

    The following settings are not supported for static libraries.
    libraries, library-search-path, linker-script, undef-compiler-symbols

    library config -name <lib-name> <param-name>
        Get the value of configuration parameter <param-name> for the
        library specified by <lib-name>.

    library config [OPTIONS] -name <lib-name> <param-name> <value>
        Set/modify/remove the value of configuration parameter <param-name>
        for the library specified by <lib-name>.
}
OPTIONS {
    -name
        Name of the library.

    -set
        Set the configuration parameter value to new <value>.

    -get
        Get the configuration parameter value.

    -add
        Append the new <value> to configuration parameter value.
        Add option is not supported for ,compiler-optimization

    -info
        Displays more information like possible values and possible
        operations about the configuration parameter. A parameter name
        must be specified when this option is used.

    -remove
        Remove <value> from the configuration parameter value.
        Remove option is not supported for assembler-flags, build-config,
        compiler-misc, compiler-optimization, linker-misc and linker-script.
}
RETURNS {
    Depends on the arguments specified.
    <none>
        List of parameters available for configuration and description of each
        parameter.

    <parameter name>
        Parameter value, or error, if unsupported parameter is specified.

    <parameter name> <parameter value>
        Nothing if the value is set successfully, or error, if unsupported
        parameter is specified.
}
EXAMPLE {
    library config -name test build-config
        Return the current build configuration for the library named test.

    library config -name test define-compiler-symbols DEBUG_INFO
        Add -DDEBUG_INFO to the compiler options, while building the test
        library.

    library config -name test -remove define-compiler-symbols DEBUG_INFO
        Remove -DDEBUG_INFO from the compiler options, while building the test
        library.

    library config -name test -set compiler-misc {-c -fmessage-length=0 -MT"$@"}
       Set {-c -fmessage-length=0 -MT"$@"} as compiler miscellaneous flags for
       the test library.

     library config -name test -append compiler-misc {-pg}
       Add {-pg} to compiler miscellaneous flags for
       the test library.

    library config -name test -info compiler-optimization
       Display more information about possible values and default values for
       compiler optimization level.
}
NOTE {
    The following settings are not supported for static libraries.
    libraries, library-search-path, linker-script, undef-compiler-symbols
}
}

    ::xsdb::setcmdmeta {library create} brief {Create a library project.}
    ::xsdb::setcmdmeta {library create} description {
SYNOPSIS {
    library create -name <project-name> -type <library-type> -platform <platform>
                   -domain <domain> -sysproj <system-project>

        Create a library project using an existing platform,
        and domain. If <platform>, <domain>,
        and <sys-config> are not specified, the active platform and
        domain are used for creating the library project.
        For creating a library project and adding it to an existing system project,
        refer to the next use case.

    library create -name <project-name> -type <library-type> -sysproj <system-project>
                   -domain <domain>

        Create a library project for domain specified by <domain> and add it to
        system project specified by <system-project>.
        If <system-project> exists, platform corresponding to
        this system project are used for creating the library project.
        If <domain> is not specified, the active domain is used.
}
OPTIONS {
    -name <project-name>
        Project name that should be created.

    -type <library-type>
        <library-type> can be 'static' or 'shared'.

    -platform <platform-name>
        Name of the platform.
        Use "repo -platforms" to list available pre-defined platforms.

    -domain <domain-name>
        Name of the domain.
        Use "platform report <platform-name>" to list the available
        domains in a platform.

    -sysproj <system-project>
        Name of the system project.
        Use "sysproj list" to know the available system projects in the workspace.

}
RETURNS {
    Nothing, if the library project is created successfully.
    Error string, if the library project creation fails.
}
EXAMPLE {
    library create -name lib1 -type static -platform zcu102 -domain a53_standalone
        Create a static library project with name 'lib1', for the platform zcu102,
        which has a domain named a53_standalone domain.

    library create -name lib2 -type shared -sysproj test_system -domain test_domain
        Create shared library project with name 'lib2' and add it to
        system project test_system.
}
}

    ::xsdb::setcmdmeta {library list} brief {List library projects.}
    ::xsdb::setcmdmeta {library list} description {
SYNOPSIS {
    List all library projects in the workspace.
}
OPTIONS {
    None.
}
RETURNS {
    List of library projects in the workspace. If no library projects exist, an empty
    string is returned.
}
EXAMPLE {
    library list
        Lists all the library projects in the workspace.
}
}

    ::xsdb::setcmdmeta {library remove} brief {Delete library project.}
    ::xsdb::setcmdmeta {library remove} description {
SYNOPSIS {
    library remove [options] <project-name>
        Delete a library project from the workspace.
}
OPTIONS {
    None.
}
RETURNS {
    Nothing, if the library project is deleted successfully.
    Error string, if the library project deletion fails.
}
EXAMPLE {
    library remove lib1
        Removes lib1 from workspace.
}
}

    ::xsdb::setcmdmeta {library report} brief {Report details of the library project.}
    ::xsdb::setcmdmeta {library report} description {
SYNOPSIS {
    library report <project-name>
        Return details such as the platform, domain, and so on of
        the library project.
}
OPTIONS {
    None.
}
RETURNS {
    Details of the library project, or error string, if library project does not exist.
}
EXAMPLE {
    app report lib1
        Return all the details of library lib1.
}
}


    proc library_old { args } {
	set options {
	    {add "name of the library" {args 1}}
	    {set "name of the library" {args 1}}
	    {remove "name of the library" {args 1}}
	    {inc-path "include path" {args 1}}
	    {lib-path "library path" {args 1}}
	    {name "name of the library to be reported" { args 1}}
	    {version "version of the library to be added" { args 1}}
	    {option "option that needs to be set" { args 1}}
	    {value "value of the option" { args 1}}
	    {report "Report the library attributes" {args 0}}
	    {list "list of libraries added" {args 0}}
	    {help "command help"}
	}
	set saved_args $args
	array set params [::xsdb::get_options args $options]
	if { $params(help) } {
	    return [help [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	set activeplatform [builtin_platform -active]
	if { $activeplatform == ""} {
	    error "Active platform does not exists, create one."
	}

	set activesystem [builtin_system -active]
	if { $activesystem == ""} {
	    error "Active system configuration does not exists, create one."
	}

	set activedomain [builtin_domain -active]
	if { $activedomain == ""} {
	    error "No active domain exists."
	}

	if { $params(list) == 1 } {
	    set retval [builtin_library -list]
	    set retdict [::json::json2dict $retval]

	    set i 0
	    set formatstr {%15s}
	    set border "[string repeat "=" 80]\n"

	    set liblist ""
	    set inclist ""
	    set repolibs ""
	    dict for {key value} $retdict {
		if { $key == $value } {
		    lappend repolibs $key
		append output "[format $formatstr $key]\n"
		} else {
		    if { $value  == "lib"} {
			if { $key != "" } {
			    lappend liblist $key
			}
		    } elseif { $value == "inc"} {
			if { $key != "" } {
			    lappend inclist $key
			}
		    }
		}
	    }
	    set itsempty 1
	    if { [llength $repolibs] != 0 } {
		set output $border
		append output "[format $formatstr "Libraries"]\n"
		append output $border
		foreach libentry $repolibs {
		    append output "[format $formatstr $libentry]\n"
		}
		set itsempty 0
	    }

	    if { [llength $liblist] != 0 } {
		append output $border
		append output "[format $formatstr "Prebuilt Libraries"]\n"
		append output $border
		foreach libentry $liblist {
		    append output "[format $formatstr $libentry]\n"
		}
		set itsempty 0
	    }

	    if { [llength $inclist] != 0 } {
		append output $border
		append output "[format $formatstr "Prebuilt Include Directories"]\n"
		append output $border
		foreach incentry $inclist {
		    append output "[format $formatstr $incentry ]\n"
		}
		set itsempty 0
	    }

	    if { $itsempty == 1 } {
		append output "No libraries added."
	    }
	    return $output
	}

	# setting options.
	if { [info exists  params(remove)] } {
	    set retval [builtin_library -remove $params(remove)]
	    return $retval
	}

	if { [info exists  params(name)] &&  [info exists  params(option)]   &&  [info exists  params(value)] } {
	    # Setting an option for the library.
	    set id [lsearch -exact $saved_args "-help"]
	    if { $id != -1 } {
		set saved_args [lreplace $saved_args $id $id]
	    }
	    set retval [builtin_library {*}$saved_args]
	    return $retval
	}
	if {[info exists  params(option)] &&  [info exists  params(name)]   &&  ![info exists  params(value)] } {
	    # Setting an option for the library.
	    set id [lsearch -exact $saved_args "-help"]
	    if { $id != -1 } {
		set saved_args [lreplace $saved_args $id $id]
	    }
	    set retval [builtin_library {*}$saved_args]
	    set retdict [::json::json2dict $retval]

	    set i 0
	    set formatstr {%-25s     %-25s     %s}
	    set border "[string repeat "=" 70]\n"
	    set output $border
	    append output "[format $formatstr "PROPERTY" "VALUE" "VALUEOPTIONS"]\n"
	    append output $border
	    set propname ""
	    set propvalue ""
	    set valueoptions ""
	    set index 0
	    dict for {key value} $retdict {
		if { $index  == 0 } {
		set propname $key
		set propvalue $value
		set index 1
		} else  {
		    set  valueoptions $value
		}
	    }

	    append output "[format $formatstr $propname $propvalue $valueoptions]\n"
	    return $output
	}

	if { $params(report) == 1 &&  [info exists  params(name)] } {
	    set id [lsearch -exact $saved_args "-help"]
	    if { $id != -1 } {
		set saved_args [lreplace $saved_args $id $id]
	    }
	    set retval [builtin_library {*}$saved_args]
	    set retdict [::json::json2dict $retval]
	    set formatstr {%-35s     %s}
	    set border "[string repeat "=" 70]\n"
	    set output $border
	    append output "[format $formatstr "PROPERTY" "VALUE"]\n"
	    append output $border
	    dict for {key value} $retdict {
		append output "[format $formatstr $key $value]\n"
	    }
	    return $output
	}

	if { [llength $saved_args] != 0 } {
	    return [builtin_library {*}$saved_args]
	}
	return
    }


    #---------------------------------------------------------------------------------------#
    # Create a driver
    # Description:  Creates a Driver with the given arguments
    # Arguments  :  peripheral
    # Type       :  SCW command
    #---------------------------------------------------------------------------------------#
    proc driver { args } {
	set options {
	    {list "list the peripheral drivers" {args 0}}
	    {peripheral "peripheral name " {args 1}}
	    {name "driver name " {args 1}}
	    {version "driver version" {args 1}}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options]
	if { $params(help) } {
	    return [help [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if {  $params(list) == 1 } {
	    set retval [builtin_driver -list]
	    set retdict [::json::json2dict $retval]

	    set i 0
	    set formatstr {%25s     %15s     %s}
	    set border "[string repeat "=" 80]\n"
	    set output $border
	    append output "[format $formatstr "Peripheral" "Driver" "Version"]\n"
	    append output $border
	    dict for {key value} $retdict {
		set incpath_libpath [ split $value ":"]
		append output "[format $formatstr $key [lindex $incpath_libpath 0] [lindex $incpath_libpath 1] ]\n"
	    }
	    return $output
	}
	# setting options.
	set optionlist {}

	if { [info exists params(peripheral)] } {
	    lappend optionlist "-peripheral"
	    lappend optionlist $params(peripheral)
	}
	if { [info exists params(name)] } {
	    lappend optionlist "-name"
	    lappend optionlist $params(name)
	}
	if { [info exists params(version)] } {
	    lappend optionlist "-version"
	    lappend optionlist $params(version)
	}

	if { [info exists params(peripheral)] && ![info exists params(name)] } {
	    set retval [builtin_driver {*}$optionlist]
	    set retdict [::json::json2dict $retval]

	    set i 0
	    set formatstr {%25s     %15s     %s}
	    set border "[string repeat "=" 80]\n"
	    set output $border
	    append output "[format $formatstr "Peripheral" "Assgined Driver" "Available Drivers"]\n"
	    append output $border
	    dict for {key value} $retdict {
		set incpath_libpath [ split $value ":"]
		append output "[format $formatstr $key [lindex $incpath_libpath 0] [join [lrange $incpath_libpath 1 end] " "]   ]\n"
	    }
	    return $output


	} elseif { [info exists params(peripheral)] && [info exists params(name)] && ![info exists params(version)] } {
	    set retval [builtin_driver {*}$optionlist]
	    set retdict [::json::json2dict $retval]

	    set i 0
	    set formatstr {%25s     %15s     %s}
	    set border "[string repeat "=" 80]\n"
	    set output $border
	    append output "[format $formatstr "Peripheral" "DriverVersion" "DriverVersion Options"]\n"
	    append output $border
	    dict for {key value} $retdict {
		set incpath_libpath [ split $value ":"]
		append output "[format $formatstr $key [lindex $incpath_libpath 0] [join [lrange $incpath_libpath 1 end] " "]   ]\n"
	    }
	    return $output
	 } else {
	       set retval [builtin_driver {*}$optionlist]
	 }
	return
    }
    namespace export driver
    ::xsdb::setcmdmeta driver brief {Driver assignment to peripherals.}
    ::xsdb::setcmdmeta driver description {
SYNOPSIS {
    Declare, list drivers associated with peripheral.
}
OPTIONS {
    -list
         List the driver assignment for all the peripherals.
}
RETURNS {
    If successful, this command returns nothing.
    Otherwise it returns an error.
}
EXAMPLE {
    driver -list
       List the driver assignment for all the peripherals.
}
}


    proc device-tree { args } {
	set options {
	    {system-user "Add system user device tree " {args 1}}
	    {include "add device tree" {args 1}}
	    {report "Report" {args 0}}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options]
	if { $params(help) } {
	    return [help [lindex [split [lindex [info level 0] 0] ::] end]]
	}
	set activedomain [builtin_domain -active]
	if { $activedomain == ""} {
	    error "No active domain exists."
	}
	set domainos [getosfordomain $activedomain]

	if { $domainos != "linux" }  {
	    error "\"device-tree\" command is valid only for domain with os as linux"
	}

       # Handling the report option.
	# Checking the boolean option report.
	if {  $params(report) == 1 } {
	    set retval [builtin_device-tree -report]
	    set retdict [::json::json2dict $retval]
	    set formatstr {%-30s     %s}
	    set border "[string repeat "=" 80]\n"
	    set output $border
	    append output "[format $formatstr "TYPE" "PATH"]\n"
	    append output $border
	    dict for {key value} $retdict {
		append output "[format $formatstr $key $value]\n"
	    }
	    return $output
	}

	 if { [info exists params(system-user) ] } {
	      return [builtin_device-tree -system-user $params(system-user)]
	}
	if { [info exists params(include) ] } {
	      return [builtin_device-tree -include $params(include)]
	}
    }
    namespace export device-tree


    proc rootfs { args } {
	set options {
	    {type "rootfs type" {args 1}}
	    {add-package "package to be added" {args 1}}
	    {list-package "package available to be added"}
	    {remove-package "remove package" {args 1}}
	    {add-app "user application to be added" {args 1}}
	    {config-file "configuration file" { args 1}}
	    {remove-config "configuration file" { args 1}}
	    {report "Report" {args 0}}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]
	if { $params(help) } {
	    return [help [lindex [split [lindex [info level 0] 0] ::] end]]
	}
	set activedomain [builtin_domain -active]
	if { $activedomain == ""} {
	    error "No active domain exists."
	}
	set domainos [getosfordomain $activedomain]

	if { $domainos != "linux" }  {
	    error "\"rootfs\" command is valid only for domain with os as linux"
	}

	# Handling the report option.
	# Checking the boolean option report.
	if {  $params(report) == 1 } {
	    set retval [builtin_rootfs -report]
	    set retdict [::json::json2dict $retval]

	    set i 0
	    set formatstr {%15s     }
	    set border "[string repeat "=" 32]\n"
	    set empty "[string repeat "" 32]\n"
	    dict for {key value} $retdict {
		if { $i == 0 } {
		    set output $border
		    append output "[format $formatstr "Added Package"]\n"
		    append output $border
		    set vallist [split $value ":"]
		    foreach pkg $vallist {
			append output "[format $formatstr $pkg]\n"
		    }
		    append output $empty
		} else {
		    append output $border
		    append output "[format $formatstr "Added Config Files"]\n"
		    append output $border
		    set vallist [split $value ":"]
		    foreach pkg $vallist {
			append output "[format $formatstr $pkg]\n"
		    }
		    append output $empty
		}
		incr i
	    }
	    return $output
	}
	if { $params(list-package) == 1 } {
	    if { [ llength $args ] > 1 } {
		error "Invalid arguments, this option can take only value value"
	    }
	    if { [ llength $args ] == 0 } {
		set retval [builtin_rootfs -list-package]
		return $retval
	    }
	    if { [ llength $args ] == 1 } {
		set retval [builtin_rootfs -list-package [lindex $args 0 ]]
		return  $retval
	    }
	}

	if { [info exists params(add-package) ] } {
	    set retval [builtin_rootfs -add-package $params(add-package)]
	    return $retval
	}
	if { [info exists params(remove-package) ] } {
	    set retval [builtin_rootfs -remove-package $params(remove-package)]
	    return $retval
	}
	if { [info exists params(add-app) ] } {
	    set retval [builtin_rootfs -add-app $params(add-app)]
	    return  $retval
	}
	if { [info exists params(config-file) ] } {
	    set retval [builtin_rootfs -config-file $params(config-file)]
	    return $retval
	}
	 if { [info exists params(remove-config) ] } {
	    set retval [builtin_rootfs -remove-config $params(remove-config)]
	    return $retval
	}
    }
    namespace export rootfs

    proc kernel { args } {
	set options {
	    {git-url "git url" {args 1}}
	    {git-tag "git tag" {args 1}}
	    {external-src "external source" {args 1}}
	    {add-module "kernel module to be included" { args 1}}
	    {config-file "active domain" {args 1}}
	    {remove-config "configuration file" { args 1}}
	    {remove-module "module" { args 1}}
	    {option "kernel option and value" {args 1}}
	    {value "kernel option value" {args 1}}
	    {report "Report the kernel attributes" {args 0}}
	    {help "command help"}
	}
	set saved_args $args
	array set params [::xsdb::get_options args $options]
	if { $params(help) } {
	    return [help [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	set activedomain [builtin_domain -active]
	if { $activedomain == ""} {
	    error "No active domain exists."
	}
	set domainos [getosfordomain $activedomain]

	if { $domainos != "linux" }  {
	    error "\"kernel\" command is valid only for domain with os as linux"
	}
	# Handling error of git_url and external-src together.
	if { [info exists params(git-url)] && [info exists params(external-src)] } {
	    error "git-url and external-src options can not be used together"
	}

	if { [info exists params(add-module) ] } {
	    return retval [builtin_kernel -add-module $params(add-module)]
	}
	# Handling the report option.
	# Checking the boolean option report.
	if {  $params(report) == 1 } {
	    set retval [builtin_kernel -report]
	    set retdict [::json::json2dict $retval]
	    set formatstr {%15s     %s}
	    set border "[string repeat "=" 32]\n"
	    set output $border
	    append output "[format $formatstr "PROPERTY" "VALUE"]\n"
	    append output $border
	    dict for {key value} $retdict {
		append output "[format $formatstr $key $value]\n"
	    }
	    return $output
	}

	if { [info exists params(option)] &&  [info exists params(value)] } {
	    # Setting an option for the kernel.
	    return retval [builtin_kernel -option $params(option) -value $params(value) ]
	}

	return retval [builtin_kernel {*}$saved_args]
    }
    namespace export kernel

    proc u-boot { args } {
	set options {
	    {git-url "name of the domain" {args 1}}
	    {git-tag "list of processors" {args 1}}
	    {external-src "external source" {args 1}}
	    {config-file "os to be hosted" { args 1}}
	    {remove-config "configuration file" { args 1}}
	    {report "Report the domain attributes" {args 0}}
	    {help "command help"}
	}
	set saved_args $args
	array set params [::xsdb::get_options args $options]
	if { $params(help) } {
	    return [help [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	set activedomain [builtin_domain -active]
	if { $activedomain == ""} {
	    error "No active domain exists."
	}
	set domainos [getosfordomain $activedomain]

	if { $domainos != "linux" }  {
	    error "\"u-boot\" command is valid only for domain with os as linux"
	}
	# Handling error of git_url and external-src together.
	if { [info exists params(git-url)] && [info exists params(external-src)] } {
	    error "git-url and external-src options can not be used together"
	}

	# Handling the report option.
	# Checking the boolean option report.
	if {  $params(report) == 1 } {
	    set retval [builtin_uboot -report]
	    set retdict [::json::json2dict $retval]
	    set formatstr {%15s     %s}
	    set border "[string repeat "=" 32]\n"
	    set output $border
	    append output "[format $formatstr "PROPERTY" "VALUE"]\n"
	    append output $border
	    dict for {key value} $retdict {
		append output "[format $formatstr $key $value]\n"
	    }
	    return $output
	}

	return [builtin_uboot {*}$saved_args]
    }
    namespace export u-boot


    proc yocto { args } {
	set options {
	    {add-recipe "name of the domain" {args 1}}
	    {tmp-dir "description" {default "" args 1}}
	    {report "Report the domain attributes" {args 0}}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options]
	if { $params(help) } {
	    return [help [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { ![info exists params(name)] } {
	    error "Domain name not specified"
	}
	if { ![info exists params(proc)] } {
	    error "Processor to be used not specified"
	}
	if { ![info exists params(os)] } {
	    error "os to be used not specified"
	}
	return [builtin_domain -name $params(name) -desc $params(desc)  -proc $params(proc) -os $params(os)]
    }
    namespace export yocto


    proc boot { args } {
	puts "\nNote:: \"boot\" command is deprecated. Use \"sysconfig config\" \
		\command for bif related settings"
	set options {
	    {bif "bif file" {args 1}}
	    {ppk "name of the domain" {args 1}}
	    {spk "description" {default "" args 1}}
	    {psk "list of processors" {args 1}}
	    {ssk "os to be hosted" { args 1}}
	    {partition "active domain" {default "" args 1}}
	    {encrypt "list the domains in the active platform" {args 0}}
	    {auth "enables the debug while building domain" {args 0}}
	    {report "Report the domain attributes" {args 0}}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options]
	if { $params(help) } {
	    return [help [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { [info exists  params(bif) ] } {
	    return [builtin_boot -bif $params(bif) ]
	}

	# Handling the report option.
	# Checking the boolean option report.
	if {  $params(report) == 1 } {
	    set retval [builtin_boot -report]
	    set retdict [::json::json2dict $retval]
	    set formatstr {%15s     %s}
	    set border "[string repeat "=" 32]\n"
	    set output $border
	    append output "[format $formatstr "PROPERTY" "VALUE"]\n"
	    append output $border
	    dict for {key value} $retdict {
		append output "[format $formatstr $key $value]\n"
	    }
	    return $output
	}
	return ""
    }
    namespace export boot

    proc getosfordomain { domain } {
	set retval [builtin_domain -report]
	set retdict [::json::json2dict $retval]
	return [dict get $retdict "os"]
    }

    proc get_processor_name { proc_name { xsa "" } } {
    if {$proc_name == "ai_engine"} {
    	return $proc_name
    }
    if {$proc_name == "x86_0"} {
    	return $proc_name
    }
    if { $xsa != "" } {
	    set predef_hw {zc702 zc706 zcu102 zcu106 zed vck190 vmk180 vck190_es1 vmk180_es1}
	    if { [lsearch -exact $predef_hw $xsa] != -1 } {
		    set xsa "$::env(XILINX_VITIS)/data/embeddedsw/lib/fixed_hwplatforms/$xsa.xsa" 
	    } 
	    openhw $xsa
	} else {
	    builtin_scwutil -openhw
	}
	
	set proc_list [::hsi::get_cells -filter {IP_TYPE == "PROCESSOR"} -hierarchical]
	set processor_match_list {}
	foreach processor $proc_list {
	    if { [string match "$proc_name" $processor] } {
		    return $processor
	    } elseif { [string match "*$proc_name*" $processor] } {
		    lappend processor_match_list $processor
		    set ip_name [::hsi::get_property IP_NAME [::hsi::get_cells $processor -hierarchical]]
		    if { [string match "$proc_name" $ip_name] } {
		    	return $proc_name
		    }
	    }
	}
	if { [llength $processor_match_list] == 1 } {
	    puts "warning: $proc_name is not available in the design, using [lindex $processor_match_list 0] instead"
	    puts "use 'getprocessors <xsa>' command to get complete list of processors available"
	    return [lindex $processor_match_list 0]
	} elseif { [llength $processor_match_list] > 1 } {
	    append msg "more than one processor found with name matching $proc_name - $processor_match_list"
	    append msg "\nplease specify complete processor instance name"
	    error $msg
	} else {
	    append msg "$proc_name is not available in the design"
	    append msg "\nuse 'getprocessors <xsa>' command to get complete list of processors available"
	    error "$msg"
	}
    }
    proc checkvalidrmxsa { args } {
	set options {
	    {hw "static xsa path" {args 1}}
	    {rm-hw "rm xsa path" {args 1}}
	    {help "command help"}
	}
	set saved_args $args	
	array set params [::xsdb::get_options args $options 0]
	if { $params(help) } {
	    return [help [lindex [info level 0] 0]]
	}
	return [builtin_scwutil {*}$saved_args]
	
    }
    namespace export checkvalidrmxsa        
    ::xsdb::setcmdmeta checkvalidrmxsa categories {projects}
    ::xsdb::setcmdmeta checkvalidrmxsa brief {Check if RM XSA is
    suitable for static XSA.}
    ::xsdb::setcmdmeta checkvalidrmxsa description {
SYNOPSIS {
    checkvalidrmxsa -hw <static hw spec file> -rm-hw <rm hw spec file>
        To check if the RM XSA is suitable to work with the static hardware XSA.
}
OPTIONS {
    None.
}
RETURNS {
    If successful, returns true if the RM hardware XSA is a fit for the static hardware XSA.
    Returns false if not.
    Otherwise, it returns an error.
}
EXAMPLE {
    checkvalidrmxsa -hw static.xsa -rm-hw rm.xsa
        Returns true if RM XSA can be used along with the static XSA.
}
}

    proc isstaticxsa { args } {
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]
	if { $params(help) } {
	    return [help [lindex [info level 0] 0]]
	}
	if { [llength $args] == 1} {
	    set hwpath [lindex $args 0]
	} else {
	    error "wrong # args: should be \"isstaticxsa <hw spec file>\""
	}
	if { [file exists $hwpath] != 1} {
	    error "hw specification file is not existing - $hwpath"
	}
	if { [file readable $hwpath] !=1 } {
	    error "hw specification file is not readble - $hwpath"
	}
	return [builtin_scwutil -isstatic $hwpath]
    }
    namespace export isstaticxsa     
    ::xsdb::setcmdmeta isstaticxsa categories {projects}
    ::xsdb::setcmdmeta isstaticxsa brief {Check if hardware design is a static XSA.}
    ::xsdb::setcmdmeta isstaticxsa description {
SYNOPSIS {
    isstaticxsa <hw spec file>
        Checks if the hardware design is a static XSA.
}
OPTIONS {
    None.
}
RETURNS {
    If successful, returns true if hardware design is static, 
    returns false if hardware design is not static.
    Otherwise, it returns an error.
}
EXAMPLE {
    isstaticxsa static.xsa
        Returns true if XSA is static.
}
}
    proc ishwexpandable { args } {
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]
	if { $params(help) } {
	    return [help [lindex [info level 0] 0]]
	}
	if { [llength $args] == 1} {
	    set hwpath [lindex $args 0]
	} else {
	    error "wrong # args: should be \"ishwexpandable <hw spec file>\""
	}
	if { [file exists $hwpath] != 1} {
	    error "hw specification file is not existing - $hwpath"
	}
	if { [file readable $hwpath] !=1 } {
	    error "hw specification file is not readble - $hwpath"
	}
	return [builtin_scwutil -ishwexpandable $hwpath]
    }
    namespace export ishwexpandable
    ::xsdb::setcmdmeta ishwexpandable categories {projects}
    ::xsdb::setcmdmeta ishwexpandable brief {Check if hardware design is expandable.}
    ::xsdb::setcmdmeta ishwexpandable description {
SYNOPSIS {
    ishwexpandable <hw spec file>
        Checks if the hardware design is expandable or fixed.
}
OPTIONS {
    None.
}
RETURNS {
    If successful, returns true if hardware design is expandable/extensible, 
    returns false if hardware design is fixed.
    Otherwise, it returns an error.
}
EXAMPLE {
    ishwexpandable system.xsa
        Returns true if XSA is expandable/extensible.
}
}

    #---------------------------------------------------------------------------------------#
    # creates device tree
    # Description : This command generates device tree for the hardware plaform
    # Arguments   :
    # Type        :  SCW command
    #---------------------------------------------------------------------------------------#

    proc createdts { args } {
	set options {
	    {git-url "git repo to be cloned" {args 1}}
	    {git-branch "git branch to be cloned" {args 1}}
	    {board "board name" {args 1}}
	    {platform-name "platform name" {args 1}}
	    {hw "handoff file path" {args 1}}
	    {out "output directory" {args 1}}
	    {zocl "zocl flag to add property to the platform"}
	    {overlay "overlay flag to add property to the platform"}
	    {dtsi "include user specified dtsi in system-top.dts" {args 1}}
	    {compile "flag to compile device tree blob"}
	    {local-repo "local repository" {args 1}}
	    {help "command help"}
	    {update "flag to update platform project with the new xsa"}
	}
	array set params [::xsdb::get_options args $options]
	if { $params(help) } {
	    return [help [lindex [split [lindex [info level 0] 0] ::] end]]
	}
	if { ![info exists params(hw)] } {
	    error "Specify HW Platform"
	} elseif { $params(update) == 0 } {
	    if { ![info exists params(platform-name)] } {
		error "Specify Platform Name"
	    }
	}
	if { [info exists params(dtsi)] && ![file exists $params(dtsi)] } {
	    error "Error: file $params(dtsi) does not exists."
	}
	set wd [pwd]
	if {[info exists params(local-repo)]} {
	    repo -set $params(local-repo)
	} elseif {$params(update) == 0 } {
	    if {[info exists params(out)]} {
		file mkdir $params(out)
		cd $params(out)
	    } else {
		if {[getws] == ""} {
		    error "Error: please set a workspace or provide -out directory"
		}
		file mkdir [getws]
		cd  [getws]
	    }
	    if {![info exists params(git-url)] } {
		set params(git-url) https://github.com/Xilinx/device-tree-xlnx.git
	    }
	    if {![info exists params(git-branch)] } {
		set params(git-branch) master
	    }
	    set dt_repo [exec find "[pwd]" -name "device-tree-xlnx"]
	    if {$dt_repo == ""} {
		puts "INFO: Downloading DTG repo from $params(git-url) to [pwd]"
		if {[catch {exec -ignorestderr git clone $params(git-url) -b $params(git-branch) } errmsg ]} {
		    cd $wd
		    error "$errmsg"
		}
	    } else {
		puts "INFO: DTG repo already exists at $dt_repo"
	    }
	    ::hsi::set_repo_path [pwd]
	}
	if {[string range $params(hw) 0 0 ] != "/"} {
	    if { [string match *.xsa $params(hw)] == 0 } {
		append params(hw) ".xsa"
	    }
	    if { [file exists $wd/$params(hw)] == 1 } {
		set params(hw) $wd/$params(hw)
	    } else {
		if { [catch { set params(hw) [glob -directory $::env(XILINX_VITIS)/data/embeddedsw/lib/fixed_hwplatforms/ $params(hw)] } errmsg ]} {
		    cd $wd
		    error "HW project $params(hw) does not exist."
		}
	    }
	}
	if { [catch {
	    hsi::open_hw_design $params(hw)
	    set family [hsi::get_property FAMILY [hsi::current_hw_design]]
	    hsi::close_hw_design [hsi::current_hw_design]

	    switch $family {
		"zynq" {
		    set def_proc "ps7_cortexa9_0"
		}
		"zynquplus" {
		    set def_proc "psu_cortexa53_0"
		}
		"versal" {
		    set def_proc "psv_cortexa72_0"
		}
		default {
		    set proc_list [getprocessors $params(hw)]
		    set def_proc [lindex $proc_list 0]
		}
	    }

	    if {$params(update)} {
		puts "INFO: Updating platform project with the new xsa"
	       platform config -updatehw $params(hw)
	    } elseif {[info exists params(out)] } {
		puts "INFO: Creating platform $params(platform-name) at $params(out)"
		platform create -name $params(platform-name) -hw $params(hw) -proc $def_proc -os device_tree -out $params(out) -no-boot-bsp
	    } else {
		puts "INFO: Creating platform $params(platform-name) at workspace [getws]"
		platform create -name $params(platform-name) -hw $params(hw) -proc $def_proc -os device_tree -no-boot-bsp
	    }
	    if {$params(zocl)} {
		hsi set_property CONFIG.dt_zocl true [hsi::get_os]
	    }
	    if {$params(overlay)} {
		hsi set_property CONFIG.dt_overlay true [hsi::get_os]
	    }
	    if {[info exists params(board)] } {
		library -name device_tree -option periph_type_overrides -value "{BOARD $params(board)}"
	    }
	    library -name device_tree -option overlay_custom_dts -value "pl-final.dts"
	    puts "INFO: Generating device tree"
	    bsp regenerate
	    platform generate
	} errmsg ] } {
	    cd $wd
	    error "$errmsg"
	}

	if { [info exists params(dtsi)] } {
	    if {[info exists params(out)]} {
		set dest $params(out)/$params(platform-name)/$def_proc/device_tree_domain/bsp
	    } else {
		set dest [getws]/$params(platform-name)/$def_proc/device_tree_domain/bsp
	    }
	    cd $wd
	    file copy -force $params(dtsi) $dest
	    set fp [open $dest/system-top.dts "a"]
	    puts $fp "#include \"[file tail $params(dtsi)]\""
	    close $fp
	}

	if {$params(compile)} {
	    if {[info exists params(out)]} {
		cd $params(out)/$params(platform-name)/$def_proc/device_tree_domain/bsp
	    } else {
		cd [getws]/$params(platform-name)/$def_proc/device_tree_domain/bsp
	    }
	    exec gcc -I my_dts -E -nostdinc -undef -D__DTS__ -x assembler-with-cpp -o system.dts system-top.dts
	    exec dtc -I dts -O dtb -o system.dtb system.dts
	}
	cd $wd
	puts "INFO: Device tree generation successful"
    }
    namespace export createdts
    ::xsdb::setcmdmeta createdts categories {projects}
    ::xsdb::setcmdmeta createdts brief {Creates device tree.}
    ::xsdb::setcmdmeta createdts description {
SYNOPSIS {
    createdts [options]
        Create a device tree for the hardware definition file.
}
OPTIONS {
    -platform-name <software-platform name>
        Name of the software platform to be generated.

    -board <board name>
        Board name for device tree to be generated.
        Board names available at <DTG Repo>/device_tree/data/kernel_dtsi.

    -hw <handoff-file>
        Hardware description file to be used to create the device tree.

    -out <output-directory>
        The directory where the software platform needs to be created.
        Workspace will be default directory, if this option is not specified.

    -local-repo <directory location>
        Location of the directory were bsp for git repo is available.
        Device tree repo will be cloned from git,
        if this option is not specified.

    -git-url <Git URL>
        Git URL of the dtg repo to be cloned.
        Default repo is https://github.com/Xilinx/device-tree-xlnx.git.

    -git-branch <Git Branch>
        Git branch to be checked out. Master branch selected default.

    -zocl
        Set zocl flag to enable zocl driver support, default set to False.
        zocl should only be used when the designs are PL enabled.
        Only master and xlnx_rel_v2021.2 branch supports zocl property.

    -overlay
        Set overlay flag to enable device-tree overlay support,
        default set to False.

    -dtsi <custom-dtsi file>
        Include custom-dtsi file in the device tree, if specified.

    -compile
        Specify this option to compile the generated dts to create dtb.
        If this option is not specified, users can manually use dts
        to compile dtb.
        For example,
        dtc -I dts -O dtb -o <file_name>.dtb <file_name>.dts
            Compile dts(device tree source) or
            dtsi(device tree source include) files.

        dtc -I dts -O dtb -f <file_name>.dts -o <file_name>.dtb
            Convert dts(device tree source) to dtb(device tree blob).

        dtc -I dtb -O dts -f <file_name>.dtb -o <file_name>.dts
            Convert dtb(device tree blob) to dts(device tree source).

    -update
        Set update flag to enable existing device tree platform
        to update with new xsa.
}
RETURNS {
    None.
}
EXAMPLE {
    createdts -hw zcu102.xsa -platform-name my_devicetree
        Create a device tree for the handoff-file with
        Default repo as "https://github.com/Xilinx/device-tree-xlnx.git"
        and deault branch as "master".

    createdts -hw zcu102.xsa -platform-name my_devicetree -git-url <Git URL>
            -git-branch <Git Branch>
        Create a device tree for the handoff-file with
        user repo as repo mentioned in <Git URL>
        and user branch as <Git Branch>.

    createdts -hw zc702.xsa -platform-name my_devicetree
            -local-repo /my_local_git_repo
        Create a device tree for the handoff-file and use the local repo.

    createdts -hw vck190.xsa -platform-name my_devicetree
            -out /device-tree_output_directory
        Create a device tree at the out directory specified
        by device-tre output directory.

    createdts -hw zcu102.xsa -platform-name my_devicetree -overlay
            -zocl -compile
        Create device tree for the handoff-file with overlay and zocl node.
        Compile flag compiles the device tree blob file from the DTS.

    createdts -hw zcu102.xsa -platform-name my_devicetree -board <Board Name>
        Creates a device tree adding board value to the library,
        Board names available at <DTG Repo>/device_tree/data/kernel_dtsi.

    createdts -update -hw newdesign.xsa
        Updates existing device tree platform with new XSA.

    createdts -hw vck190 -platform-name vck190 -out <out_dir>
            -dtsi <dir>/system-conf.dtsi
        Create device tree with custom-dtsi file included.
}
NOTE {
    This command is a shortcut for creating a device tree domain and
    generating the device tree. It clones the device tree repo, creates a
    platform with device_tree as OS, and configures and generates the
    platform to create dts.
    -zocl should only be used when the designs are PL enabled.
    Only master and xlnx_rel_v2021.2 branch supports zocl property.
}

}
}

package provide scw 0.1
