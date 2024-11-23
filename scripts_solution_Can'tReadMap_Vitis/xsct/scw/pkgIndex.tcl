if {![package vsatisfies [package provide Tcl] 8.5]} {
    # PRAGMA: returnok
    return
}

package ifneeded scwutil 0.1 [list source [file join $dir scwutil.tcl]]
package ifneeded scw 0.1 [list source [file join $dir scw.tcl]]

