package require Tcl 8.5

namespace eval ::sdtgen {
    variable version 1.0

    set sdt_path $env(XILINX_VITIS)
    source $sdt_path/data/system-device-tree-xlnx/device_tree/data/device_tree.tcl

    # Export specific commands from device_tree.tcl
    namespace export set_dt_param
    namespace export generate_sdt
    namespace export get_dt_param
    namespace export copy_hw_files

    ::xsdb::setcmdmeta sdtgen brief {System Device Tree}

    ::xsdb::setcmdmeta {sdtgen set_dt_param} categories {sdtgen}
    ::xsdb::setcmdmeta {sdtgen set_dt_param} brief {Set the device tree parameters like xsa, board and ouput directory}
    ::xsdb::setcmdmeta {sdtgen set_dt_param} description {[sdtgen::set_dt_param -help]}

    ::xsdb::setcmdmeta {sdtgen generate_sdt} categories {sdtgen}
    ::xsdb::setcmdmeta {sdtgen generate_sdt} brief {Generate system device tree}
    ::xsdb::setcmdmeta {sdtgen generate_sdt} description {[sdtgen::generate_sdt -help]}

    ::xsdb::setcmdmeta {sdtgen get_dt_param} categories {sdtgen}
    ::xsdb::setcmdmeta {sdtgen get_dt_param} brief {Return the device tree parameters like xsa, board and ouput directory}
    ::xsdb::setcmdmeta {sdtgen get_dt_param} description {[sdtgen::get_dt_param -help]}

    ::xsdb::setcmdmeta {sdtgen copy_hw_files} categories {sdtgen}
    ::xsdb::setcmdmeta {sdtgen copy_hw_files} brief {Copy hardware artifacts into given output directory}
    ::xsdb::setcmdmeta {sdtgen copy_hw_files} description {[sdtgen::copy_hw_files -help]}

    namespace ensemble create -command ::sdtgen
}

package provide sdtgen $::sdtgen::version
