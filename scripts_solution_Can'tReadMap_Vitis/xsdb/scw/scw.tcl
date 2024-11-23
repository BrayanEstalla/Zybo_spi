package require Tcl 8.5
package require xsdb
package require json



# set tcl_prompt1 {puts -nonewline "scw% "}


namespace eval scw {
    variable help_prefix ""
    variable regen_done 0
    set loaded 0
    
    if { ![catch {load librdi_scw[info sharedlibextension]}] } {
         set loaded 1
         } else {
                puts "Failed to load the librdi_scw shared library "
             }

    #---------------------------------------------------------------------------------------#
    # Sofwtware platform
    # Description:  Creates a Software Platform with the given arguments
    # Arguments  :  Hardware project / Hardware Design (*.hdf / *.xml)
    # Type       :  SCW command
    #---------------------------------------------------------------------------------------#
    
    proc update_hwdb_table { args } {        
        variable ::xsdb::designtable
        if { [llength $args] == 2 } {
              set dsapath [lindex $args 0]
              set design [lindex $args 1]              
              if { ![dict exists $designtable $dsapath] } {
                dict set designtable $dsapath design $design                
              }
        } else {
            error "wrong # args: should be \"update_hw_db <hdf file> <hwdb name> \""
        }
        
    }
    proc get_app_linker_constraints { appname apptclpath} {	
        set namespace_name [file rootname [file tail $apptclpath]]
        namespace eval ::xsdb::sdk::$namespace_name [list source $apptclpath]	
        set linkerconstarint [namespace eval ::xsdb::sdk::$appname {swapp_get_linker_constraints}]
        return $linkerconstarint 
    }
    proc get_ip_sub_type { ip_inst_object} {
        if { [string compare -nocase cell [common::get_property CLASS $ip_inst_object]] != 0 } {
            error "get_mem_type API expect only mem_range type object whereas $class type object is passed"
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

    
    
     proc update_swdb_table { args } {        
        variable ::xsdb::swdesignmaps
        if { [llength $args] == 2 } {
              set msspath [lindex $args 0]
              set design [lindex $args 1]              
              if { ![dict exists $swdesignmaps $msspath] } {
                dict set swdesignmaps $msspath design $design
                #set d1 [dict get $swdesignmaps $msspath design]
                #puts $d1
                
              } else {
                set swdesignmaps [dict remove $swdesignmaps $msspath]
                dict set swdesignmaps $msspath design $design
              }
        } else {
            error "wrong # args: should be \"update_sw_db <mss file> <swdb name> \""
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
            error "wrong # args: should be \"update_sw_db <mss file> <swdb name> \""
        }
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
        if { $regen_done == 0 } {
            if { [llength $args] == 1 } {
                set hdf [lindex $args 0]
                set configurablecell [ ::hsi::get_cells -filter { CONFIGURABLE == true } -hierarchical]
                if { $configurablecell != "" } {		
                    ::hsi::generate_target {psinit} $configurablecell -dir [file dirname $hdf]
                    set regen_done 1
                }	    
            
            } else {
                error "wrong # args: should be \"regenerate_psinit <dir> \""
            }
            
        }	
    }
    
    
    
    proc get_hw_path { args } { 
        set retval [builtin_scwutil -gethwpath]        
        return $retval    
    }
    
    proc scw_mode { args } {       
     
      if { [llength $args] == 1 } {            
            set retval [builtin_scwutil -mode [lindex $args 0] ]
            return $retval
        } else {
            error "wrong # args: should be \"scw_mode <mode> \""
        }
    }
    
    proc get_mss_path { args } {    
        set retval [builtin_scwutil -getmsspath]
        set desname [hsi current_sw_design]
        ::scw::update_swdb_table $retval $desname
        return $retval    
    }  
    
     proc get_target { args } { 
        set retval [builtin_scwutil -gettarget]        
        return $retval    
    }   
    
    proc sdx_write_mss { }  {
        set retval [platform -write]
        set msspath [builtin_scwutil -savemsschanges]
        set msspath [builtin_scwutil -getmsspath]
        set desname [hsi current_sw_design]
        ::scw::update_swdb_table $msspath $desname
        ::scw::clean_swdb_table $msspath
        return $retval
        
    }
    proc sdx_reload_mss { }  {
        set retval [builtin_scwutil -reloadmss]
        set msspath [builtin_scwutil -getmsspath]
        set desname [hsi current_sw_design]
        ::scw::update_swdb_table $msspath $desname
        ::scw::clean_swdb_table $msspath
        return $retval        
    }
    
    
    proc reload_linkgen { args } {
       
       set hdf [::scw::get_hw_path]
       ::hsi::generate_target {psinit}  [ ::hsi::get_cells -filter { CONFIGURABLE == true } -hierarchical] -dir [file dirname $hdf]
        set retval [builtin_app -reload-linkgen]
        return $retval
    }  
    #---------------------------------------------------------------------------------------#
    # Sofwtware platform
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
            set retval [builtin_scwutil -getproc $params(os) ]
            return $retval
         }
    
    }
    
    proc plnx-install-path { args } {
        
        set nargs [llength $args]
        
        if { $nargs == 0} {
            set retval [builtin_scwutil -plnxpath]
            return $retval
        }        
        set retval [builtin_scwutil -plnxpath [lindex $args 0]]
        return $retval
    }
    namespace export plnx-install-path
    
    #---------------------------------------------------------------------------------------#
    # Sofwtware platform
    # Description:  Creates a Software Platform with the given arguments
    # Arguments  :  Hardware project / Hardware Design (*.hdf / *.xml)
    # Type       :  SCW command
    #---------------------------------------------------------------------------------------#
    proc platform { args } {
        variable help_prefix

        set options {
            {name "name of the software platform" {args 1}}
            {desc "description" {args 1}}
            {hw "handoff file path" {args 1}}
            {updatehw "new handoff file path" {args 1}}            
            {out "output directory" {args 1}}
            {make-local "Make referenced resources local" {args 0}}
            {samples "samples directory" {args 1}}
            {type "platform type" {default "SDx" args 1}}
            {active "active platform" }
            {generate "generate the platform" }
            {quick "quick platform generation" }
            {list "list the software platforms" {args 0}}
            {report "Report the platform attributes" {args 0}}
            {remove "remove the platform" {args 1}}
            {write "Write the platform" {args 0}}
            {prebuilt "Prebuilt flow" {args 0}}
            {json "Give Platform data in Json format" {args 0}}
            {read "Read the platform" {args 1}}
            {help "command help"}
        }

        array set params [::xsdb::get_options args $options 0]
        if { $params(help) } {
            return [help [subst $help_prefix][lindex [split [lindex [info level 0] 0] ::] end]]
        }
        
        
        
         if { $params(active) == 1 } {            
            if { [ llength $args ] > 1 } {
                error "Invalid arguments, this option can take only value value"
            }
            if { [ llength $args ] == 0 } {
                set retval [builtin_platform -active]                           
                return $retval                
            }
            if { [ llength $args ] == 1 } {
                 set retval [builtin_platform -active [lindex $args 0 ]]
                 return  $retval
            }
        }

         if {  $params(list) == 1 } {

            set retval [builtin_platform -list]
            
            set retdict [::json::json2dict $retval]

            set i 0
            set formatstr {%15s     %s}
            set border "[string repeat "=" 32]\n"
            set output $border
            append output "[format $formatstr "NAME" "DESCRIPTION"]\n"
            append output $border
            dict for {key value} $retdict {
                append output "[format $formatstr $key $value]\n"
            }
            return $output
            
        }
         if {  $params(json) == 1 } {
            set retval [builtin_platform -json]
            
            return $retval
         }
        if {  $params(write) == 1 } {
           # puts "going to write platform "
            set retval [builtin_platform -write]
           # puts "write completed"
            return $retval
        }
        if {  [info exists params(remove) ]  } {
            
            if { [catch { [builtin_platform -active $params(remove)] } msg] } {               
                error $msg
            }
    
            set retval [builtin_platform -remove]            
            return $retval
        }
        if { [info exists params(updatehw) ] } {           
            set retval [builtin_platform -updatehw $params(updatehw) ]            
            return $retval            
        }
        
        if { [info exists params(read) ] } {
           
            set retval [builtin_platform -read $params(read) ]
            set hwdb [hsi current_hw_design]
            set dsapath [::scw::get_hw_path]
            ::scw::update_hwdb_table $dsapath $hwdb
            return $retval
            
        }
        
         # Handling the report option.
        # Checking the boolean option report.
        if {  $params(report) == 1 } {
            
            set retval [builtin_platform -report]
            set retdict [::json::json2dict $retval]

            set i 0
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
         if { $params(generate) == 1 } {            
            if { [ llength $args ] > 1 } {
                error "Invalid arguments, this option can take only one value"
            }            
            if { $params(quick) != 1 } {
                set retval [builtin_platform -generate full]                           
                return $retval                
            } else {
                set retval [builtin_platform -generate quick]                           
                return $retval                
            }
            
        }
        
        if {  $params(make-local) == 1  } {
            set retval [builtin_platform -make-local]
            platform -write
            return $retval
        }

         # New platform can be added only with a combination of -name and -hw option.
         # In case of invocations, without name, active platform must present.
         # and in that case, it will modify the active platform attributes.
         # i.e in invocations without name, if no active platform present, should through an error.

         # Error handling the no -name invocation and no active present case.
        if { ![info exists params(name)] } {
            set activeplat [builtin_platform -active]
            if { $activeplat == ""} {
                  error "Platform does not exists, please create a new platform."
            } else {                
                if { [info exists  params(samples) ] } {                    
                  set retval [builtin_platform -samples $params(samples)]            
                  return $retval
                }
                if { [info exists  params(desc) ] } {                    
                  set retval [builtin_platform -desc $params(desc)]            
                  return $retval
                }
            # Modifying the platform case.
            }

        }
        
        if { [info exists params(name)] && ![info exists params(hw)] } {

            error "To declare a new platform -hw option is mandatory. Please pass the Handoff file."
        } elseif { [info exists params(name)] && [info exists params(hw)] } {

         # New platform declaration case. Handling the new platform addition.
         # Passing all the provided options to the builtin to create a new platform.

            set optionlist { }

                  lappend optionlist "-name"
                  lappend optionlist $params(name)
                  lappend optionlist "-hw"
                  lappend optionlist $params(hw)

            if { [info exists  params(desc) ] } {
                  lappend optionlist "-desc"
                  lappend optionlist $params(desc)
            }
            if { [info exists  params(out) ] } {
                  lappend optionlist "-out"
                  lappend optionlist $params(out)
            }
            if { [info exists  params(samples) ] } {
                  lappend optionlist "-samples"
                  lappend optionlist $params(samples)
            }
            if { $params(prebuilt) == 1} {
                  lappend optionlist "-prebuilt"                  
            }
            if { [info exists  params(sw-repo) ] } {
                  lappend optionlist "-sw-repo"
                  lappend optionlist $params(sw-repo)
            }
            if { [info exists  params(type) ] } {
                  lappend optionlist "-type"
                  lappend optionlist $params(type)
            }
            set retval [builtin_platform {*}$optionlist]
            set hwdb [hsi current_hw_design]
            
            set dsapath [::scw::get_hw_path]
            ::scw::update_hwdb_table $dsapath $hwdb
            return $retval
        }
        if {  $params(prebuilt) == 1  } {
            set retval [builtin_platform -prebuilt]            
            return $retval
        }
        
        return


    }
      ::xsdb::setcmdmeta scw brief {List all SCW commands}
    namespace export platform
    ::xsdb::setcmdmeta platform categories {scw}
    ::xsdb::setcmdmeta platform brief {Specifying and modifying the Platform}
    ::xsdb::setcmdmeta platform description {
SYNOPSIS {
Used for :-
         - Declare a new platform
         - Modify the existing platform attributes.
         - List the existing attributes
         - Generate the platform
         - Read from existing platform file
         - Writes the platform file.
         - Creates a Software Platform based on the arugments passed.
}
OPTIONS {
    -name <software-platform name>
        Software Platform with name that should be created

    -desc <description>
        A Brief description about the software platform being created

    -hw <handoff file>
        Handoff file which needs to be used to create the software platform

    -out <Output directory>
        The directory where the software platform artifacts needs to be created

    -samples <samples directory>
        To make the samples in the provided directory part of the platform
    
    -remove <name of the platform>
        To remove the platform from the session
    
    -prebuilt
        To mark the platform to be built from already built sw artifacts
    
    -json
        Return the json string of the platform
    
    -make-local
        Makes the referenced SW Components local to the platform.
    
    -active <active platform>
        If working on multiple platforms, this command can help set the active platform.

    -generate
        Generates the platform.
        
    -write
        Writes the platform to the persistence file. Can be read back useing read option.
    
    -read <platform file>
        Reads from the platform from the persistence.
    
    -list
        Lists the platforms in the SCW Session.

    -report
        Reports the active platform attributes.


}
RETURNS {
    If successful, this command returns nothing.
    Otherwise it returns an error.
}
EXAMPLE {
    platform -name "ZC702Test" -hw /home/chaitany/Desktop/zc702.hdf
       Declares the software platform with the given HDF.

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
        array set params [::xsdb::get_options args $options 0]
        if { $params(help) } {
            return [help [subst $help_prefix][lindex [split [lindex [info level 0] 0] ::] end]]
        }

          set activeplat [builtin_platform -active]
            if { $activeplat == ""} {
                  error "No active system configuration  exists, as there is no active platform"
            }
            
          if {  $params(list) == 1 } {

            set retval [builtin_system -list]
            
            set retdict [::json::json2dict $retval]

            set i 0
            set formatstr {%15s     %s}
            set border "[string repeat "=" 32]\n"
            set output $border
            append output "[format $formatstr "NAME" "DESCRIPTION"]\n"
            append output $border
            dict for {key value} $retdict {
                append output "[format $formatstr $key $value]\n"
            }
            return $output
            
        }
        
         if { $params(active) == 1 } {            
            if { [ llength $args ] > 1 } {
                error "Invalid arguments, this option can take only value value"
            }
            if { [ llength $args ] == 0 } {
                set retval [builtin_system -active]                           
                return $retval                
            }
            if { [ llength $args ] == 1 } {
                 set retval [builtin_system -active [lindex $args 0 ]]
                 return  $retval
            }
        }        

        # Handling the report option.
        # Checking the boolean option report.
        if {  $params(report) == 1 } {

            set retval [builtin_system -report]
            set retdict [::json::json2dict $retval]

            set i 0
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
        
        if { [info exists params(name)]  } {
            # create new sysconfig  case.
             set optionlist { }

            lappend optionlist "-name"
            lappend optionlist $params(name)
            lappend optionlist "-desc"
            if { [info exists params(desc)]} {                    
                lappend optionlist $params(desc)
            } else {
                lappend optionlist $params(name)                    
            }
            lappend optionlist "-display-name"
            if { [info exists params(display-name)]} {                    
                lappend optionlist $params(display-name)
            } else {
                lappend optionlist $params(name)                    
            }
            if { [info exists params(default)]} {
                lappend optionlist "-default"
            }
            if { [info exists  params(boot) ] } {
                lappend optionlist "-boot"
                lappend optionlist $params(boot)
            }
            if { [info exists  params(readme) ] } {
                lappend optionlist "-readme"
                lappend optionlist $params(readme)
            } 
            set retval [builtin_system {*}$optionlist]
            return $retval

         }
         
        if { [info exists  params(boot) ] } {                  
            set retval [builtin_system -boot $params(boot)]            
            return $retval
        }
        if { [info exists  params(readme) ] } {                  
            set retval [builtin_system -readme $params(readme)]            
            return $retval
        }
        if { [info exists  params(desc) ] } {                    
            set retval [builtin_system -desc $params(desc)]            
            return $retval
        }
        if { [info exists  params(display-name) ] } {                    
            set retval [builtin_system -display-name $params(display-name)]            
            return $retval
        }

        if {  [info exists params(remove) ]  } {            
            if { [catch { [builtin_system -active $params(remove)] } msg] } {               
                error $msg
            }    
            set retval [builtin_system -remove]            
            return $retval
        }
            
    }
 namespace export system
    ::xsdb::setcmdmeta system categories {scw}
    ::xsdb::setcmdmeta system brief {Specifying and modifying the system configuration}
    ::xsdb::setcmdmeta system description {
SYNOPSIS {
         Uses :-
         - Declares a system configuration.
         - Used to list the attributes of the system configuration.
         - Modify the attributes of existing system configuration.
         - Find active system configuration.
         - Set active system configuration.
         - Can mark the system configuration as default.
}
OPTIONS {
    -name <system configuration name>
         Domain with name that should be created
         
    -display-name <display name>
         The name to be displayed for the system configuration.

    -desc <description>
         A Brief description about the system configuration being created
  
    -active <active system configuration>
         Sets the active system configuration
         
    -boot <boot directory>
         The boot directory in the case of platform built from pre-built sw artifacts.
    
    -readme <readme file>
         Readme file

    -list
         Lists the system configuration for the active platform.

    -report
         Reports the active system configuration attributes.

}
RETURNS {
    If successful, this command returns nothing.
    Otherwise it returns an error.
}
EXAMPLE {
    system -name "sysconfig1" -desc "SMP Linux on A53, FreeRTOS On R5_0"
        Creates the a system configuration in platform.

    system -active
        Lists the active system configuration

    system -active sysconfig1
        Makes the sysconfig1 as active system configuration

    system -report
        Reports the system configuration properties.

}
}
 #---------------------------------------------------------------------------------------#
    # Create a Domain
    # Description:  Creates a Domain with the given arguments
    # Arguments  :  Hardware project / Hardware Design (*.hdf / *.xml)
    # Type       :  SCW command
    #---------------------------------------------------------------------------------------#
    proc domain { args } {
        variable help_prefix

        set options {
            {name "name of the domain" {args 1}}
            {display-name "display name" {args 1}}
            {desc "description" {args 1}}
            {default "default domain " {args 0}}
            {proc "list of processors" {args 1}}
            {sw-repo "sw repo" {args 1}}
            {support-app "app name" {args 1}}
            {mss "MSS file" {args 1}}
            {arch "architecture" {args 1}}
            {guest-on-hypervisor "hypervisor guest" {args 0}}
            {qemu-args "qemu args file" {args 1}}
            {qemu-data "qemu data " {args 1}}
            {pmuqemu-args "pmu quemu args file" {args 1}}
            {prebuilt-data "prebuilt directory" {args 1}}
            {image "Image directory" {args 1}}
            {readme "readme file" {args 1}}
            {os "os to be hosted" {args 1}}
            {runtime "runtime" {args 1}}
            {active "active domain" }
            {list "list the domains in the active platform" {args 0}}
            {remove "remove" {args 1}}
            {report "report" {args 0}}
            {enable-debug "enables the debug while building domain" {args 0}}
            {help "command help"}
        }
        array set params [::xsdb::get_options args $options 0]
        if { $params(help) } {
            return [help [subst $help_prefix][lindex [split [lindex [info level 0] 0] ::] end]]
        }

         set activeplat [builtin_platform -active]
            if { $activeplat == ""} {
                  error "No active platform  exists"
            }
        
         set activesys [builtin_system -active]
            if { $activesys == ""} {
                  error "No active system  exists"
            }
        
            
          set activeplat [builtin_platform -active]
            if { $activeplat == ""} {
                  error "No active domain exists, as there is no active platform"
            }
        if {  $params(list) == 1 } {

            set retval [builtin_domain -list]
            
            set retdict [::json::json2dict $retval]

            set i 0
            set formatstr {%15s     %s}
            set border "[string repeat "=" 32]\n"
            set output $border
            append output "[format $formatstr "NAME" "DESCRIPTION"]\n"
            append output $border
            dict for {key value} $retdict {
                append output "[format $formatstr $key $value]\n"
            }
            return $output
            
        }
         # Handling the active option.
         
          if { $params(active) == 1 } {            
            if { [ llength $args ] > 1 } {
                error "Invalid arguments, this option can take only value value"
            }
            if { [ llength $args ] == 0 } {
                set retval [builtin_domain -active]                           
                return $retval                
            }
            if { [ llength $args ] == 1 } {
                 set retval [builtin_domain -active [lindex $args 0 ]]
                 return  $retval
            }
        }
        
        # Handling the report option.
        # Checking the boolean option report.
        if {  $params(report) == 1 } {

            set retval [builtin_domain -report]
            set retdict [::json::json2dict $retval]

            set i 0
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

        

         # New Domain can be added only with a combination of -name  -proc and -os option.
         # In case of invocations, without name, active domain must present.
         # and in that case, it will modify the active domain attributes.
         # i.e in invocations without name, if no active domain present, should through an error.

         # Error handling the no -name invocation and no active present case.
        if { ![info exists params(name)] } {
            set activedomain [builtin_domain -active]
            if { $activedomain == ""} {
                  error "Active Domain does not exists, please create one."
            } else {
            # Modifying the domain case.
                if { [info exists  params(runtime) ] } {                  
                  set retval [builtin_domain -runtime $params(runtime)]            
                  return $retval
                }
                if { [info exists  params(image) ] } {                  
                  set retval [builtin_domain -image $params(image)]            
                  return $retval
                }
                 if { [info exists  params(readme) ] } {                  
                  set retval [builtin_domain -readme $params(readme)]            
                  return $retval
                }
                 if { [info exists  params(mss) ] } {                  
                  set retval [builtin_domain -mss $params(mss)]            
                  return $retval
                }
                if { [info exists  params(desc) ] } {                  
                  set retval [builtin_domain -desc $params(desc)]            
                  return $retval
                }
                if { [info exists  params(display-name) ] } {                  
                  set retval [builtin_domain -display-name $params(display-name)]            
                  return $retval
                }
                if { [info exists  params(sw-repo) ] } {                  
                  set retval [builtin_domain -sw-repo $params(sw-repo)]            
                  return $retval
                }
                if { [info exists  params(qemu-args) ] } {                  
                  set retval [builtin_domain -qemu-args $params(qemu-args)]            
                  return $retval
                }
                if { [info exists  params(qemu-data) ] } {                  
                  set retval [builtin_domain -qemu-data $params(qemu-data)]            
                  return $retval
                }
                if { [info exists  params(pmuqemu-args) ] } {                  
                  set retval [builtin_domain -pmuqemu-args $params(pmuqemu-args)]            
                  return $retval
                }
                 if { [info exists  params(prebuilt-data) ] } {                  
                  set retval [builtin_domain -prebuilt $params(prebuilt-data)]            
                  return $retval
                }

                if {  [info exists params(remove) ]  } {
                    
                    if { [catch { [builtin_domain -active $params(remove)] } msg] } {               
                        error $msg
                    }
            
                    set retval [builtin_domain -remove]            
                    return $retval
                }
                
                
                  puts "Domain modified"
                  return
            }

        }

         if { [info exists params(name)]  && [info exists params(proc)] && [info exists params(os)] } {
              # create new domain case.
               set optionlist { }

                  lappend optionlist "-name"
                  lappend optionlist $params(name)
                  lappend optionlist "-proc"
                  lappend optionlist $params(proc)
                  lappend optionlist "-os"
                  lappend optionlist $params(os)

            if { [info exists  params(desc) ] } {
                  lappend optionlist "-desc"
                  lappend optionlist $params(desc)
            }
            if { [info exists  params(display-name) ] } {
                  lappend optionlist "-display-name"
                  lappend optionlist $params(display-name)
            }
            if { [info exists  params(arch) ] } {
                  lappend optionlist "-arch"
                  lappend optionlist $params(arch)
            }
            if {  $params(guest-on-hypervisor) == 1 } {
                lappend optionlist "-guest-on-hypervisor"
                lappend optionlist "true"
                
            }            
            if { [info exists  params(mss) ] } {
                  lappend optionlist "-mss"
                  lappend optionlist $params(mss)
            }
            if { [info exists  params(image) ] } {
                  lappend optionlist "-image"
                  lappend optionlist $params(image)
            }
            if { [info exists  params(runtime) ] } {
                  lappend optionlist "-runtime"
                  lappend optionlist $params(runtime)
            }
            if { [info exists  params(enable-debug) ] } {
                  lappend optionlist "-enable-debug"
                  lappend optionlist $params(enable-debug)
            }
            if { [info exists  params(sw-repo) ] } {
                  lappend optionlist "-sw-repo"
                  lappend optionlist $params(sw-repo)
            }
            if { [info exists  params(support-app) ] } {
                  lappend optionlist "-support-app"
                  lappend optionlist $params(support-app)
                  
                  lappend optionlist "-lconstraints"
                  set tempswdes [hsi create_sw_design sdxtempdes -proc $params(proc) -os $params(os) -app $params(support-app) ]
                  lappend optionlist [get_app_linker_constraints $params(support-app) [builtin_scwutil -apptclpath $params(support-app) ]]
                  hsi close_sw_design [hsi current_sw_design]
            }            
            if { [info exists params(default)]} {
                    lappend optionlist "-default"
            }
            if { $params(os) == "linux" } {
                set imagegiven [info exists  params(image) ]
                set isprebuilt [platform -prebuilt]
                
                if { $isprebuilt == "false" } {
                    
                      if { $::env(RDI_PLATFORM) != "lnx64" } {
                            error "Linux based domain can not be created on windows host machine"                    
                        }
                  
                    set instpath [plnx-install-path]
                    if { $instpath == "" } {
                        
                        set mode [builtin_scwutil -mode]
                        
                        if { $mode == "gui"} {                        
                            set outmsg "To create Linux based domains, petalinux is required. Please set the petalinux path in \
                            Window -> Preferences -> Xilinx SDx -> Platform Project -> Petalinux Install Location."
                            error $outmsg
                        } else {
                            set outmsg "To create Linux based domains, petalinux is required. Please set the petalinux path using the command plnx-install-path."
                            error $outmsg
                        }
                    }
                }                  
            }
            # puts "Going to create the domain"
            set retval [builtin_domain {*}$optionlist]
            # puts "domain created"
            return $retval
            

         } else {              
         
                  if { [info exists params(name)] && [ ![info exists params(proc)] ||  ![info exists params(os)] ] } {
                      error "To declare a new domain -proc and -os options are mandatory"
                  }
                  set activedomain [builtin_domain -active]
                   if { $activedomain == ""} {
                        error "Active domain is required to modify the properties."
                  } else {
                           # Handle modification of properties.

                  }
         }
        return

    }
    namespace export domain
    ::xsdb::setcmdmeta domain categories {scw}
    ::xsdb::setcmdmeta domain brief {Specifying and modifying the Domain}
    ::xsdb::setcmdmeta domain description {
SYNOPSIS {
         Uses :-
         - Declares a domain.
         - Used to list the attributes of the domain
         - Modify the attributes of existing domain
         - Find active domain
         - Set active domain
         - Can mark the domain to be debug built
}
OPTIONS {
    -name <domain name>
         Domain with name that should be created
         
    -display-name <display name>
         The name to be displayed for the domain.

    -desc <description>
         A Brief description about the domain being created

    -proc <processor list>
         Processor/Processors to be used for the domain

    -os <os>
         Operating system to be used for the domain
     
    -image <linux image location>
         For Linux based domain tool picks the image from the given directory
         and will be creating the petalinux project.
         
    -sw-repo <Repositories list >
        List of repositories which must be set
        
    -mss <mss file path>
         For mss based domain tool picks the mss from the given file
         and will not generate the mss file.
         
    -active <active domain>
         Sets the active domain

    -list
         Lists the domains for the active platform.

    -report
         Reports the active domain attributes.

    -enable-debug
         Builds the domain by enabling debug
}
RETURNS {
    If successful, this command returns nothing.
    Otherwise it returns an error.
}
EXAMPLE {
    domain -name "SMPLinux" -os linux -proc ps7_cortexa9_0
        Creates the a linux domain in the platform.

    domain -active
        Lists the active domain

    domain -active SMPLinux
        Makes the SMPLinux as active domain

    domain -report
        Reports the domain properties.

}
}

    #---------------------------------------------------------------------------------------#
    # Create a Domain
    # Description:  Creates a Domain with the given arguments
    # Arguments  :  Hardware project / Hardware Design (*.hdf / *.xml)
    # Type       :  SCW command
    #---------------------------------------------------------------------------------------#
    proc app { args } {
        variable help_prefix

        set options {
            {compiler-flags "compiler flags" {args 1}}
            {linker-flags "linker flags" {args 1}}
            {lscript "linker script" {args 1}}
            {heap-size "heap size" {args 1}}
            {stack-size "stack size" { args 1}}
            {code-mem "code sections memory" {args 1}}
            {data-mem "data sections memory" {args 1}}
            {list-mem "list the available memories" {args 0}}
            {stack-heap-mem "stack and heap section's memory" {args 1}}
            {report "report" {args 0}}
            {json "report data in json" {args 0}}
            {help "command help"}
        }
        array set params [::xsdb::get_options args $options]
        if { $params(help) } {
            return [help [subst $help_prefix][lindex [split [lindex [info level 0] 0] ::] end]]
        }

        set activedomain [builtin_domain -active]
        if { $activedomain == ""} {
           error "No active domain exists."
        }         
         
        # Handling the report option.
        # Checking the boolean option report.
        if {  $params(report) == 1 } {
            set retval [builtin_app -report]
            set retdict [::json::json2dict $retval]
            if { $params(json) == 1 } {                
                return $retval
            } else {
                set i 0
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
        }
        if {  $params(list-mem) == 1 } {
            set retval [builtin_app -list-mem]
            return $retval
        }
        if { [info exists  params(lscript) ]  } {
            set retval [builtin_app -lscript $params(lscript) ]
            return $retval
        }                 
        # setting options.
        set optionlist { }
          
        if { [info exists  params(compiler-flags) ] } {
            lappend optionlist "-compiler-flags"
            lappend optionlist $params(compiler-flags)
        }
        if { [info exists  params(linker-flags) ] } {
            lappend optionlist "-linker-flags"
            lappend optionlist $params(linker-flags)
        }
        if { [info exists  params(heap-size) ] } {
            lappend optionlist "-heap-size"
            lappend optionlist $params(heap-size)
        }
        if { [info exists  params(stack-size) ] } {
            lappend optionlist "-stack-size"
            lappend optionlist $params(stack-size)
        }
        if { [info exists  params(code-mem) ] } {
            lappend optionlist "-code-mem"
            lappend optionlist $params(code-mem)
        }
        if { [info exists  params(data-mem) ] } {
            lappend optionlist "-data-mem"
            lappend optionlist $params(data-mem)
        }
        if { [info exists  params(stack-heap-mem) ] } {
            lappend optionlist "-stack-heap-mem"
            lappend optionlist $params(stack-heap-mem)
        }
        
        set retval [builtin_app {*}$optionlist]

        return $retval        
    }
    namespace export app
    ::xsdb::setcmdmeta app categories {scw}
    ::xsdb::setcmdmeta app brief {Specifying the application settings}
    ::xsdb::setcmdmeta app description {
SYNOPSIS {
         Uses :-
         - Declares the application option to be used.
         - Reports the option set
}
OPTIONS {
    -compiler-flags <compile flags>
         The compiler flags which need to be appended to all the application sources.

    -linker-flags <linker flags>
         Linker flags to be used while linking the application
         
    -lscript <linker script file>
         Picks the linker script from the given file. And will not attempt to create the
         linker script.

    -heap-size <heap size>
         The heap size the application should use.

    -stack-size <stack size>
         The stack size the application should use.

    -code-mem <memory>
         The memory where the code segments should be placed.

    -data-mem <memory>
         The memory where the data segments should be placed.

    -stack-heap-mem <memory>
         The memory where the stack and heap should be placed.

    -report
         Reports the application option set.

}
RETURNS {
    If successful, this command returns nothing.
    Otherwise it returns an error.
}
EXAMPLE {
    app -heap-size 0x20000
       Sets the heap size for the application to be 0x20000

    app -compiler-flags "-DUSE_STUB"
       Appends the given compiler flags for the application compiler flags.

}
}


    #---------------------------------------------------------------------------------------#
    # Create a Domain
    # Description:  Creates a Domain with the given arguments
    # Arguments  :  Hardware project / Hardware Design (*.hdf / *.xml)
    # Type       :  SCW command
    #---------------------------------------------------------------------------------------#
    proc library { args } {
        variable help_prefix

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
        array set params [::xsdb::get_options args $options]
        if { $params(help) } {
            return [help [subst $help_prefix][lindex [split [lindex [info level 0] 0] ::] end]]
        }
         set activeplatform [builtin_platform -active]
            if { $activeplatform == ""} {
                  error "Active platform does not exists, please create one."
            }
        set activesystem [builtin_system -active]
            if { $activesystem == ""} {
                  error "Active system configuration does not exists, please create one."
            }
         set activedomain [builtin_domain -active]
         if { $activedomain == ""} {
                error "No active domain exists."
         }
        if {  $params(list) == 1 } {

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
                append output "no libraries added."
            }
            return $output            
        }
        
         set nameUsed 0
         set optionUsed 0
         set valueUsed 0
         set optionlist { }
         
         if { [info exists  params(name) ] } {
             lappend optionlist "-name"
             lappend optionlist $params(name)
             set nameUsed 1
         }
         if { [info exists  params(option) ] } {
             lappend optionlist "-option"
             lappend optionlist $params(option)
             set optionUsed 1
         }
         if { [info exists  params(value) ] } {
             lappend optionlist "-value"
             lappend optionlist $params(value)
             set valueUsed 1
         }
         
                 
         # setting options.
       
         set addUsed 0
         set incPathUsed 0
         set libPathUsed 0
         set setUsed 0
         if { [info exists  params(remove) ] } {
            lappend optionlist "-remove"
            lappend optionlist $params(remove)             
            set retval [builtin_library -remove $params(remove)]            
            return $retval
         }
         
         if { [info exists  params(add) ] } {
             lappend optionlist "-add"
             lappend optionlist $params(add)
             set addUsed 1
         }
         if { [info exists  params(set) ] } {
             lappend optionlist "-set"
             lappend optionlist $params(set)
             set setUsed 1
         }
          if { [info exists  params(version) ] } {
             lappend optionlist "-version"
             lappend optionlist $params(version)
             set addUsed 1
         }
         if { [info exists  params(inc-path) ] } {
             lappend optionlist "-inc-path"
             lappend optionlist $params(inc-path)
             set incPathUsed 1
         }
         if { [info exists  params(lib-path) ] } {
             lappend optionlist "-lib-path"
             lappend optionlist $params(lib-path)
             set libPathUsed 1
         }         
         
         if { $optionUsed == 1 &&  $nameUsed == 1   &&  $valueUsed == 1  } {
            
            # Setting an option for the library.
            set retval [builtin_library {*}$optionlist]
            puts $retval
            return            
         }
         if { $optionUsed == 1 &&  $nameUsed == 1   &&  $valueUsed == 0  } {
            
            # Setting an option for the library.
            set retval [builtin_library {*}$optionlist]
            
            
                set retdict [::json::json2dict $retval]
    
                set i 0
                set formatstr {%25s     %25s     %s}
                set border "[string repeat "=" 80]\n"
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
            return            
         }
         
       
          if { $params(report) == 1 &&  $nameUsed == 1    } {
            
                lappend optionlist "-report"
                set retval [builtin_library {*}$optionlist]
                set retdict [::json::json2dict $retval]
    
                set i 0
                set formatstr {%50s     %s}
                set border "[string repeat "=" 80]\n"
                set output $border
                append output "[format $formatstr "PROPERTY" "VALUE"]\n"
                append output $border
                dict for {key value} $retdict {
                    append output "[format $formatstr $key $value]\n"
                }
                return $output
            
            
            
            
          }
         
         if { [llength $optionlist] != 0 } {                  
            set retval [builtin_library {*}$optionlist]
            return  $retval
         }
         return
    }
    namespace export library
    ::xsdb::setcmdmeta library categories {scw}
    ::xsdb::setcmdmeta library brief {Modifying the libraries to be added and library options}
    ::xsdb::setcmdmeta library description {
SYNOPSIS {
         Uses :-
         - Used to add a library, both mld based and non-mld based,
         - Set library option.
         - List the libraries
         - Report the library options set
}
OPTIONS {
    -add <library name>
         Make the library available to the application.

    -inc-path <include path>
         The addition include path which should be added to the application.

    -lib-path <library path>
         The additional library path which should be added to the linker settings of the application.

    -name <name of the library>
         Report the library option set, used along with -report option.

    -list
         Lists the libraries added for the application.

    -report
         Reports the library options, will be used along with -name option.

}
RETURNS {
    If successful, this command returns nothing.
    Otherwise it returns an error.
}
EXAMPLE {
    library -name xiliffs
       Makes the xiliffs library available for the application.

    library -name opencv_face -inc-path /proj/opencv/include -lib-path /proj/opencv/lib
       Makes the libopencv_face.so available for the application. Adds the include and the lib path to the domain's
       application build settings.

    library -name xiliffs -report
       Reports the xiliffs library options set.

}
}


    #---------------------------------------------------------------------------------------#
    # Create a Domain
    # Description:  Creates a Domain with the given arguments
    # Arguments  :  Hardware project / Hardware Design (*.hdf / *.xml)
    # Type       :  SCW command
    #---------------------------------------------------------------------------------------#
    proc driver { args } {
        variable help_prefix

        set options {
            {list "list the peripheral drivers" {args 0}}
            {peripheral "peripheral name " {args 1}}
            {name "driver name " {args 1}}
            {version "driver version" {args 1}}
            {help "command help"}
        }
        array set params [::xsdb::get_options args $options]
        if { $params(help) } {
            return [help [subst $help_prefix][lindex [split [lindex [info level 0] 0] ::] end]]
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
        set periphused 0
        set nameused 0
        set versionused 0 
         # setting options.
          set optionlist { }
          
         if { [info exists  params(peripheral) ] } {
             lappend optionlist "-peripheral"
             lappend optionlist $params(peripheral)
             set periphused 1
         }
         if { [info exists  params(name) ] } {
             lappend optionlist "-name"
             lappend optionlist $params(name)
             set nameused 1
         }
         if { [info exists  params(version) ] } {
             lappend optionlist "-version"
             lappend optionlist $params(version)
             set versionused 1
         }         
         
         if {  $periphused == 1 &&  $nameused == 0 }  {
            
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
                  
            
         } elseif { $periphused == 1 && $nameused == 1 && $versionused == 0 } {
            
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
    ::xsdb::setcmdmeta driver categories {scw}
    ::xsdb::setcmdmeta driver brief {Driver assignment to peripherals}
    ::xsdb::setcmdmeta driver description {
SYNOPSIS {
         Uses :-
         - Declares the driver to be used for a peripheral
         - List the driver assignment
}
OPTIONS {
    -list
         List the driver assignment for all the peripherals

}
RETURNS {
    If successful, this command returns nothing.
    Otherwise it returns an error.
}
EXAMPLE {
    driver -list
       List the driver assignment for all the peripherals

}
}
#---------------------------------------------------------------------------------------#
    # Modify device-tree
    # Description:  Creates a Domain with the given arguments
    # Arguments  :  Hardware project / Hardware Design (*.hdf / *.xml)
    # Type       :  SCW command
    #---------------------------------------------------------------------------------------#
    proc device-tree { args } {
        variable help_prefix

        set options {
            {system-user "Add system user device tree " {args 1}}
            {include "add device tree" {args 1}}                       
            {report "Report" {args 0}}
            {help "command help"}
        }
        array set params [::xsdb::get_options args $options]
        if { $params(help) } {
            return [help [subst $help_prefix][lindex [split [lindex [info level 0] 0] ::] end]]
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

            set i 0
            set formatstr {%30s     %s}
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
            
              set retval [builtin_device-tree -system-user $params(system-user)]
              puts $retval
              return                        
        }
        if { [info exists params(include) ] } {
            
              set retval [builtin_device-tree -include $params(include)]
              puts $retval
              return                        
        }
    }
    namespace export device-tree
    ::xsdb::setcmdmeta device-tree categories {scw}
    ::xsdb::setcmdmeta device-tree brief {Specifying the Device Tree for Linux}
    ::xsdb::setcmdmeta device-tree description {
SYNOPSIS {
         Uses :-
          - add a device tree to include.
          - specify the top level device tree.
}
OPTIONS {
    -system-user <path to the top device tree>
         Specify the user top device tree

    -include <path to the device tree>
         Specify the device tree to be included.
         
    -report
         Reports the device tree settings.
}
RETURNS {
    If successful, this command returns nothing.
    Otherwise it returns an error.
}
EXAMPLE {
    device-tree -include /proj/test/pl.dts
       Add the pl.dts to the included device tree.

    device-tree -system-user /proj/test/design1.dtsi
       Add the design1.dtsi as the top level user device tree.    

}
}

    #---------------------------------------------------------------------------------------#
    # Create a Domain
    # Description:  Creates a Domain with the given arguments
    # Arguments  :  Hardware project / Hardware Design (*.hdf / *.xml)
    # Type       :  SCW command
    #---------------------------------------------------------------------------------------#
    proc rootfs { args } {
        variable help_prefix

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
            return [help [subst $help_prefix][lindex [split [lindex [info level 0] 0] ::] end]]
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
    ::xsdb::setcmdmeta rootfs categories {scw}
    ::xsdb::setcmdmeta rootfs brief {Specifying the Linux Root File System}
    ::xsdb::setcmdmeta rootfs description {
SYNOPSIS {
         Uses :-
         - Rootfs type
         - Add package
         - List the package which can be added
         - Add user application
         - Use a config file
         - Report the properties
}
OPTIONS {
    -type <type of the rootfs>
         Change the rootfs type the mentioned type.

    -add-package <package name>
         Add the given package to the rootfs.
         
    -list-package <matching string>
         Returns the package names with the given string as substring.
         If matching string is empty returns all the packges available.

    -add-app <application path>
         Add the application at given path to be part of the rootfs build.

    -config-file <config file>
         The configuration file to be used for the rootfs build

    -report
         Reports the active domain attributes.
}
RETURNS {
    If successful, this command returns nothing.
    Otherwise it returns an error.
}
EXAMPLE {
    rootfs -type initramfs
       Makes the rootfs type as initramfs

    rootfs -add-package yavta
       Adds the yavta package to rootfs

    rootfs -list-package yav
       Lists the packages with yav as substring.

}
}

    #---------------------------------------------------------------------------------------#
    # Create a Domain
    # Description:  Creates a Domain with the given arguments
    # Arguments  :  Hardware project / Hardware Design (*.hdf / *.xml)
    # Type       :  SCW command
    #---------------------------------------------------------------------------------------#
    proc kernel { args } {
        variable help_prefix

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
        array set params [::xsdb::get_options args $options]
        if { $params(help) } {
            return [help [subst $help_prefix][lindex [split [lindex [info level 0] 0] ::] end]]
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
            
              set retval [builtin_kernel -add-module $params(add-module)]
              puts $retval
              return                        
        }
        # Handling the report option.
        # Checking the boolean option report.
        if {  $params(report) == 1 } {

            set retval [builtin_kernel -report]
            set retdict [::json::json2dict $retval]

            set i 0
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
                 
         # setting options.
          set optionlist { }
          
         if { [info exists  params(git-url) ] } {
             lappend optionlist "-git-url"
             lappend optionlist $params(git-url)
         }
         if { [info exists  params(git-tag) ] } {
             lappend optionlist "-git-tag"
             lappend optionlist $params(git-tag)
         }
         if { [info exists  params(external-src) ] } {
             lappend optionlist "-external-src"
             lappend optionlist $params(external-src)
         }
         if { [info exists  params(add-module) ] } {
             lappend optionlist "-add-module"
             lappend optionlist $params(add-module)
         }
         if { [info exists  params(config-file) ] } {
             lappend optionlist "-config-file"
             lappend optionlist $params(config-file)
         }
         if { [info exists  params(remove-config) ] } {
             lappend optionlist "-remove-config"
             lappend optionlist $params(remove-config)
         }
          if { [info exists  params(remove-module) ] } {
             lappend optionlist "-remove-module"
             lappend optionlist $params(remove-module)
         }
         
         
         set optionUsed 0
         set valueUsed 0
         
         if { [info exists  params(option) ] } {             
             set optionUsed 1
         }
         if { [info exists  params(value) ] } {
             lappend optionlist "-value"             
             set valueUsed 1
         }
         
         
         if { $optionUsed == 1 &&  $valueUsed == 1  } {
            
            # Setting an option for the kernel.
            set retval [builtin_kernel -option $params(option) -value $params(value) ]
            puts $retval
            return            
         }
         
         
         
         set retval [builtin_kernel {*}$optionlist]

         puts $retval
         return
   

    }
    namespace export kernel
    ::xsdb::setcmdmeta kernel categories {scw}
    ::xsdb::setcmdmeta kernel brief {Specifying the Linux Kernel}
    ::xsdb::setcmdmeta kernel description {
SYNOPSIS {
         Uses :-
         - Set the git repo details
         - Add kernel module
         - Use custom config file
         - Set the kernel arguments
         - Report
}
OPTIONS {
    -git-url <git url>
         The GIT URL from where the kernel need to be picked up.
         If no git-branch or git-tag is specified, tool will pick from master branch.

    -git-tag <git tag>
         Used along with git-url to specify the git tag to be used.

    -external-src <directory>
         Source directory to be used as Kernel sources.
         
    -add-module <name> <path>
         The additional kernel module to be included as part of the build.

    -config-file <kernel configuration file>
         The configuration file to be used whilie building the kernel.

    -option <option name> <value>
         Specify the kernel option and its value.

    -report
         Reports the kernel option set.

}
RETURNS {
    If successful, this command returns nothing.
    Otherwise it returns an error.
}
EXAMPLE {
    kernel -option baseaddr 0x2000000
       Changes the kernel baseaddr

    kernel -config-file /proj/test/mykernel.config
       Uses the given config file to configure the kernel.

}
}

    #---------------------------------------------------------------------------------------#
    # Create a Domain
    # Description:  Creates a Domain with the given arguments
    # Arguments  :  Hardware project / Hardware Design (*.hdf / *.xml)
    # Type       :  SCW command
    #---------------------------------------------------------------------------------------#
    proc u-boot { args } {
        variable help_prefix

        set options {
            {git-url "name of the domain" {args 1}}            
            {git-tag "list of processors" {args 1}}
            {external-src "external source" {args 1}}
            {config-file "os to be hosted" { args 1}}
            {remove-config "configuration file" { args 1}}
            {report "Report the domain attributes" {args 0}}
            {help "command help"}
        }
        array set params [::xsdb::get_options args $options]
        if { $params(help) } {
            return [help [subst $help_prefix][lindex [split [lindex [info level 0] 0] ::] end]]
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

            set i 0
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
                 
         # setting options.
          set optionlist { }
          
         if { [info exists  params(git-url) ] } {
             lappend optionlist "-git-url"
             lappend optionlist $params(git-url)
         }
         if { [info exists  params(git-tag) ] } {
             lappend optionlist "-git-tag"
             lappend optionlist $params(git-tag)
         }
         if { [info exists  params(external-src) ] } {
             lappend optionlist "-external-src"
             lappend optionlist $params(external-src)
         }
         if { [info exists  params(config-file) ] } {
             lappend optionlist "-config-file"
             lappend optionlist $params(config-file)
         }
         if { [info exists  params(remove-config) ] } {
             lappend optionlist "-remove-config"
             lappend optionlist $params(remove-config)
         }
         
         
         set retval [builtin_uboot {*}$optionlist]

         puts $retval
         return
   

    }
    namespace export u-boot
    ::xsdb::setcmdmeta u-boot categories {scw}
    ::xsdb::setcmdmeta u-boot brief {Specifying the Linux u-boot }
    ::xsdb::setcmdmeta u-boot description {
SYNOPSIS {
         Uses :-
         -  Set git repo details
         -  Ret custom user configfile
         -  Report
}
OPTIONS {
    -git-url <git url>
         The GIT URL from where the u-boot need to be picked up.
         If no git-branch or git-tag is specified, tool will pick from master branch.

    -git-branch <git branch>
         Used along with git-url to specify the git branch to be used.

    -git-tag <git tag>
         Used along with git-url to specify the git tag to be used.

    -external-src <directory>
         Source directory to be used as U-boot sources.
         
    -config-file <u-boot configuration file>
         The configuration file to be used whilie building the u-boot.

    -report
         Reports the u-boot option set.

}
RETURNS {
    If successful, this command returns nothing.
    Otherwise it returns an error.
}
EXAMPLE {
    u-boot -git-url https://github.com/Xilinx/u-boot-xlnx.git
       Makes the u-boot to be picked up from the master branch of the given git url.

}
}

    #---------------------------------------------------------------------------------------#
    # Create a Domain
    # Description:  Creates a Domain with the given arguments
    # Arguments  :  Hardware project / Hardware Design (*.hdf / *.xml)
    # Type       :  SCW command
    #---------------------------------------------------------------------------------------#
    proc yocto { args } {
        variable help_prefix

        set options {
            {add-recipe "name of the domain" {args 1}}
            {tmp-dir "description" {default "" args 1}}
            {report "Report the domain attributes" {args 0}}
            {help "command help"}
        }
        array set params [::xsdb::get_options args $options]
        if { $params(help) } {
            return [help [subst $help_prefix][lindex [split [lindex [info level 0] 0] ::] end]]
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
        set retval [builtin_domain -name $params(name) -desc $params(desc)  -proc $params(proc) -os $params(os)]
        puts $retval
        return

    }
    if { 0 } {
    namespace export yocto
    ::xsdb::setcmdmeta yocto categories {scw}
    ::xsdb::setcmdmeta yocto brief {Yocto based settings for Linux domain}
    ::xsdb::setcmdmeta yocto description {
SYNOPSIS {
         Used to :
         - add Yocto Recipe
         - set yocto tmp dir
         - report
}
OPTIONS {
    -add-recipe <recipt path>
         Path of the yocto recipe to be used while building the linux

    -tmp-dir <temporary directory for yocto>
         The tempororary directory to be used by Yocto.

    -report
         Reports the yocto based settings made.

}
RETURNS {
    If successful, this command returns nothing.
    Otherwise it returns an error.
}
EXAMPLE {
    yocto -add-recipe /proj/test/poky-jethro-14.0.0/meta/recipes-core/images/core-image-minimal.bb
       Makes the linux build to apply the given yocto recipe.

}
}
    }
    #---------------------------------------------------------------------------------------#
    # Create a Domain
    # Description:  Creates a Domain with the given arguments
    # Arguments  :  Hardware project / Hardware Design (*.hdf / *.xml)
    # Type       :  SCW command
    #---------------------------------------------------------------------------------------#
    proc boot { args } {
        variable help_prefix

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
            return [help [subst $help_prefix][lindex [split [lindex [info level 0] 0] ::] end]]
        }

         set activeplat [builtin_platform -active]
            if { $activeplat == ""} {
                  error "No active platform  exists"
            }
        
         set activesys [builtin_system -active]
            if { $activesys == ""} {
                  error "No active system  exists"
            }
                  
             if { [info exists  params(bif) ] } {
                set retval [builtin_boot -bif $params(bif) ]
                
                return $retval
            }
            
             # Handling the report option.
        # Checking the boolean option report.
        if {  $params(report) == 1 } {

            set retval [builtin_boot -report]
            set retdict [::json::json2dict $retval]

            set i 0
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
    ::xsdb::setcmdmeta boot categories {scw}
    ::xsdb::setcmdmeta boot brief {Specifying the boot settings for the Platform}
    ::xsdb::setcmdmeta boot description {
SYNOPSIS {
         Uses :-
         - Set the authentication and encryption settings
         - Mark the paritions to be encrypted or authenticated
         - Report the settings
}
OPTIONS {
    
    -bif <bif file>
         uses bif file provided. Will not generate the bif file.
    
    -ppk <ppk file>
         ppk file to be used for bootgen

    -spk <spk file>
         spk file to be used for bootgen

    -psk <psk file>
         psk file to be used for bootgen

    -ssk <ssk file>
         ssk file to be used for bootgen

    -partition <partition>
         The parition for which attributes are to be changed.

    -encrypt
         Encrypt the given partition. Partition which needs to be encrypted is specified with -partition command.

    -auth
         Authenticate the given partition. Partition which needs to be authenticated is specified with -partition command.

    -report
         Report the boot settings for the active platform.
}
RETURNS {
    If successful, this command returns nothing.
    Otherwise it returns an error.
}
EXAMPLE {
    boot -ppk /proj/test/test.ppk
       Makes the bootgen to use the given ppk file.

    boot -partition bitstream -encrypt
       Makes the bitstream to be encrypted in the boot image.

}
}

 #---------------------------------------------------------------------------------------#
    # Isolate domains using XMPU/XPPU 
    # Description:  Isolate a Domain with the given arguments
    # Arguments  :  Hardware project / Hardware Design (*.hdf / *.xml)
    # Type       :  SCW command
    #---------------------------------------------------------------------------------------#
    proc isolate { args } {
        variable help_prefix

        set options {
            {master "name of the dma capable master" {args 1}}
            {slave "name of the peripheral/memory segment" {args 1}}
            {tz "Set the Trust settings of the segment" {args 1}}
            {wrallowed "Allow read write access to the segment" {args 1}}
            {sa "Start address of segment" {args 1}}
            {size "size of the segment" {args 1}}
            {id "unique id of the segment" {args 1}}
            {list "list all system platform resources" {args 1}}
            {report "report" {args 0}}
            {json "json" {args 0}}
            {remove "remove" {args 0}}
            {reload "reload" {args 0}}
            {help "command help"}
        }
        array set params [::xsdb::get_options args $options]
        if { $params(help) } {
            return [help [subst $help_prefix][lindex [split [lindex [info level 0] 0] ::] end]]
        }

          set activeplat [builtin_platform -active]
            if { $activeplat == ""} {
                  error "No active domain exists, as there is no active platform"
            }

        # Handling the report option.
        # Checking the boolean option report.
        if {  $params(report) == 1 } {
            
            #puts "going to get isolate report json"

            set retval [builtin_isolate -report]
            #puts "executed report"
            set retdict [::json::json2dict $retval]

            if {  $params(json) == 1 } {
            #  puts "returning the value "
              return $retval
            }
            

            set i 0
            set formatstr {%15s     %15s    %10s    %10s    %10s    %15s    %15s    %15s    %15s}
            set border "[string repeat "=" 158]\n"
            set output $border
            #append output "[format $formatstr "Name" "Start Address" "Size" "Unit" "TZSetting" "Access Settings" "End Address" "type"]\n"
            append output "[format $formatstr "Name" "Start Address" "Size" "Unit" "TZSetting" "Access Settings" "Type" "MemType" "SEG_ID"]\n"
            append output $border
                    set len [llength [dict get [::json::json2dict $retval] Masters] ]
                    append output "[format $formatstr "\[Masters\]" "" "" "" "" "" "" "" ""]\n"
                for {set i 0} {$i < $len} {incr i} {
                    set name [dict get [lindex [dict get [::json::json2dict $retval] Masters] $i] segName]
                    set sa [dict get [lindex [dict get [::json::json2dict $retval] Masters] $i] segStartAddress]
                    set size [dict get [lindex [dict get [::json::json2dict $retval] Masters] $i] segSize]
                    set unit [dict get [lindex [dict get [::json::json2dict $retval] Masters] $i] segUnit]
                    set tz [dict get [lindex [dict get [::json::json2dict $retval] Masters] $i] segTZSetting]
                    set access [dict get [lindex [dict get [::json::json2dict $retval] Masters] $i] segAccessSetting]
                    set type [dict get [lindex [dict get [::json::json2dict $retval] Masters] $i] segType]
                    set memtype [dict get [lindex [dict get [::json::json2dict $retval] Masters] $i] memType]
                    set id [dict get [lindex [dict get [::json::json2dict $retval] Masters] $i] segId]
                    append output "[format $formatstr $name $sa $size $unit $tz $access $type $memtype $id]\n"
                }
                    append output "\n[format $formatstr "\[Slaves\]" "" "" "" "" "" "" "" ""]\n"
                    set len [llength [dict get [::json::json2dict $retval] Slaves] ]
                for {set i 0} {$i < $len} {incr i} {
                    set name [dict get [lindex [dict get [::json::json2dict $retval] Slaves] $i] segName]
                    set sa [dict get [lindex [dict get [::json::json2dict $retval] Slaves] $i] segStartAddress]
                    set size [dict get [lindex [dict get [::json::json2dict $retval] Slaves] $i] segSize]
                    set unit [dict get [lindex [dict get [::json::json2dict $retval] Slaves] $i] segUnit]
                    set tz [dict get [lindex [dict get [::json::json2dict $retval] Slaves] $i] segTZSetting]
                    set access [dict get [lindex [dict get [::json::json2dict $retval] Slaves] $i] segAccessSetting]
                    set type [dict get [lindex [dict get [::json::json2dict $retval] Slaves] $i] segType]
                    set memtype [dict get [lindex [dict get [::json::json2dict $retval] Slaves] $i] memType]
                    set id [dict get [lindex [dict get [::json::json2dict $retval] Slaves] $i] segId]
                    append output "[format $formatstr $name $sa $size $unit $tz $access $type $memtype $id]\n"
                }
            return $output
        }

        

         # Isolation Domain can be added only with a combination of -name ,  -SA and -size option.
        set optionlist { }

            if {  $params(reload) == 1 } {
            
              lappend optionlist "-reload"
              lappend optionlist $params(reload)
              set retval [builtin_isolate {*}$optionlist]
              return $retval
        }

         if { [info exists params(list) ] } {
            
             set formatstr {%-25s%-25s%-25s%-25s%-25s%-25s}
              lappend optionlist "-list"
              lappend optionlist $params(list)
            if {  $params(json) == 1 } {
                  lappend optionlist "-json"
                  lappend optionlist $params(json)
            }
              set retval [builtin_isolate {*}$optionlist]
              #set retval [builtin_isolate -list $params(list)]
            if {  $params(json) == 1 } {
            #  puts $retval
              return $retval
            }
            
              #set strRet [split $retval " "]
              set strRet [regexp -all -inline {\S+} $retval]
              set output "" 
              foreach {i j k l m n}  $strRet {
                  append output "[format $formatstr $i $j $k $l $m $n]\n"
              }
              #puts $output
              return $output                       
        }

         if { [info exists params(slave) ] && [info exists params(sa) ] && [info exists params(size) ] } {
              # create new domain case.

                  lappend optionlist "-sa"
                  lappend optionlist $params(sa)
                  lappend optionlist "-size"
                  lappend optionlist $params(size)
                  lappend optionlist "-slave"
                  lappend optionlist $params(slave)

            if { [info exists  params(tz) ] } {
                  lappend optionlist "-tz"
                  lappend optionlist $params(tz)
            }
            if { [info exists  params(wrallowed) ] } {
                  lappend optionlist "-wrallowed"
                  lappend optionlist $params(wrallowed)
            }
            if { [info exists  params(id) ] } {
                  lappend optionlist "-id"
                  lappend optionlist $params(id)
            }

            if {  $params(json) == 1 } {
                  lappend optionlist "-json"
                  lappend optionlist $params(json)
            }


            
            set retval [builtin_isolate {*}$optionlist]
           # puts " Completed the isolate execution"            
            #puts $retval
            return $retval;

         } else {
             if {![info exists params(id) ] && [info exists params(slave) ] && [info exists params(sa) ] &&  ![info exists params(size) ] } {
                 error "Adding isolation to a memory segment requires both Start Address -sa and size -size options as mandatory"
                 return;
             }
             if {![info exists params(id) ] && [info exists params(slave) ] && ![info exists params(sa)] &&  [info exists params(size) ] } {
                 error "Adding isolation to a memory segment requires both Start Address -sa and Size -size options as mandatory"
                 return;
             }
             if { [info exists params(master) ] && ([info exists params(wrallowed)] || [info exists params(size)] || [info exists params(sa)] || [info exists params(id)]) } {
                 error "Master Protection supports only -tz option."
                 return;
             }

            if { $params(remove) == 1 } {
                  lappend optionlist "-remove"
                  lappend optionlist $params(remove)
            }

            if { [info exists params(slave)] } {
                  lappend optionlist "-slave"
                  lappend optionlist $params(slave)
            }      
            if { [info exists params(master)] } {
                  lappend optionlist "-master"
                  lappend optionlist $params(master)
            }      
            if { [info exists  params(tz) ] } {
                  lappend optionlist "-tz"
                  lappend optionlist $params(tz)
            }
            if { [info exists  params(wrallowed) ] } {
                  lappend optionlist "-wrallowed"
                  lappend optionlist $params(wrallowed)
            }
            if { [info exists  params(id) ] } {
                  lappend optionlist "-id"
                  lappend optionlist $params(id)
                  if { [info exists  params(sa) ] } {
                      lappend optionlist "-sa"
                      lappend optionlist $params(sa)
                  }
                  if { [info exists  params(size) ] } {
                      lappend optionlist "-size"
                      lappend optionlist $params(size)
                  }
            }

            if {  $params(json) == 1 } {
                  lappend optionlist "-json"
                  lappend optionlist $params(json)
            }
            set retval [builtin_isolate {*}$optionlist]

            #puts $retval
            return $retval;
         }

        return

    }
    namespace export isolate 
    ::xsdb::setcmdmeta isolate categories {scw}
    ::xsdb::setcmdmeta isolate brief {Specifying and modifying the Domain}
    ::xsdb::setcmdmeta isolate description {
SYNOPSIS {
         Uses :-
         - Isolate domains.
         - Multi domain creation using isolation settings.
         - Used to isolate peripheral, memory segments and masters
         - Modify the attributes of security settings 
}
OPTIONS {
    -slave <isolate name>
         Isolate peripheral/memory segment for the existing domain

    -master <isolate name>
         Make DMA master accessible to existing isolated objects

    -sa <start address>
         Start address of the memory segment to be isolated

    -size <start address>
         Size of the memory segment to be isolated

    -id <segment id>
         Unique id of the memory segment to be isolated

    -tz <Trust Zone setting>
         TrustZone setting to be applied for segment

    -wrallowed <Read/Write Access>
         Read/Write access to be applied for segment

    -remove
         Remove the isolated segment.

    -list <list platform resources>
         List platform resources

    -report
         Reports the isolated domain subsystems.

}
RETURNS {
    If successful, this command returns nothing.
    Otherwise it returns an error.
}
EXAMPLE {
    isolate -slave "psu_uart_0" -tz Secure 
        Isolate peripheral for the active domain in the platform and set attribute TZsetting as Secure.

    isolate -master "psu_sd_1" -tz Secure 
        Make DMA capable master SD accessible to doamin's isolated objects set set attribute TZsetting as Secure.

    isolate -name "psu_ddr_0" -sa 0x00000000 -size 1MB
        Isolate memory segments for the active domain having start address and size

    isolate -report
        Reports the isolation properties.

}
}

   proc getosfordomain { domain } {
         set retval [builtin_domain -report]
         set retdict [::json::json2dict $retval]
         return [dict get $retdict "os"]
   }



}

# unknown proc from rdi to support partial commands in non-interactive mode
rename unknown scw::tcl::unknown
proc unknown { args } {
    # auto expand command (CR576119)
    set commands {}
    set name [lindex $args 0]
    if { ![catch {set candidates [info commands $name*]}] } {
	foreach candidate $candidates {
	    if { [string first $name $candidate] == 0 } {
		lappend commands $candidate
	    }
	}
    }
    if {[llength $commands] == 1} {
	# found a match
	set ret [catch {uplevel 1 [lreplace $args 0 0 [lindex $commands 0]]} result]
    } elseif {[llength $commands]} {
	# more than one match
	# still call scw::tcl::unknown in case this is an system command
	set ret [catch {uplevel 1 [list scw::tcl::unknown {*}$args]} result]
	if {$ret!=0} {
	    # if this was a system command and if it failed, then prepend
	    # the system error to the ambiguous error. checking for system
	    # comamnd is too much, just check if error is different from
	    # the standard ambiguous error (cr619468)
	    set system_error ""
	    if {[string first "ambiguous" $result] == -1} {
		set system_error "$result\n"
	    }
	    return -code error "${system_error}ambiguous command name \"$name\": [lsort $commands]"
	}
    } else {
	# call scw::tcl::unknown
	set ret [catch {uplevel 1 [list scw::tcl::unknown {*}$args]} result]
    }
    return -code $ret $result
}

package provide scw 0.1
