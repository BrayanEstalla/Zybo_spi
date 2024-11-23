#!/bin/bash

#
# This script will start or stop an Xvfb display. 
# Note: This workflow only runs on Linux due to Xvfb supporting Linux only.
#
# Arguments taken:
#    start - start an Xvfb display
#    stop - kill the Xvfb display associated with this session
#

#
# Global vars
#
XVFB_DISP=
XVFB_DISP_FILE=/tmp/X.xvfb-display.txt
XVFB_LOG_FILE=/tmp/X.setup-log.txt
AUTH_FILE=/tmp/X.xvfb-auth.txt
randomIndex=0
DBUS_EXISTS=0

[ -n "$XSCT_FAST_TRAP" ] && trap 'trap - SIGTERM && kill -0 $$ && kill -- -$$' SIGINT SIGTERM SIGUSR2

#
# Cleanup a Xvfb display
# Used when a signal is trapped
# Background: A kill by SIGUSR2 will cause Xvfb to leave the /tmp/.X$DISPLAY-lock file
#
cleanupXvfb() {    
    LOG=/tmp/X.xvfb-cleanup_$USER.txt
    
    if [ -f $XVFB_DISP_FILE ] ; then
        XVFBDISPLAY=`cat $XVFB_DISP_FILE`
    else
        echo "Error: xvfb-display.txt not found, exiting."
        exit 1
    fi
    XVFB_LOCK=/tmp/.X$XVFBDISPLAY-lock
    XVFB_LOCK_UNIX=/tmp/.X11-unix/X$XVFBDISPLAY

    #
    # Only run if there is a lockfile present since Xvfb will be killed by LSF
    #
    if [ -f $XVFB_LOCK ] ; then
        rm -f $LOG
        touch $LOG

        echo `date` "Cleaning up Xvfb processes and files due to SIGUSR2" >> $LOG

        #
        # Try to kill Xvfb gracefully if it has not already been killed
        #
        XVFB_PID=`ps -ef -u $USER | grep Xvfb | grep ":$XVFBDISPLAY " | grep -v grep | awk '{print $2}'`

        if [ x$XVFB_PID != x ] ; then
            echo "kill $XVFB_PID" >> $LOG
            kill $XVFB_PID >> $LOG
        fi

        #
        # This is what we really want to cleanup
        #
        echo "rm -f $XVFB_LOCK" >> $LOG
        rm -f $XVFB_LOCK >> $LOG
        echo "rm -f $XVFB_LOCK_UNIX" >> $LOG
        rm -f $XVFB_LOCK_UNIX >> $LOG
    fi
	if [ "$DBUS_EXISTS" = "1" ]; then
	    # Clean up dbus-daemon and dbus-launch	
	    DBUS_DAEMON_PID=`sed -e 's,.*DBUS_SESSION_BUS_PID=,,g' -e 's,\;.*,,g' $DBUS_INFO_FILE`
	    #echo "Killing dbus daemon pid $DBUS_DAEMON_PID"
	    ps $DBUS_DAEMON_PID > /dev/null
	    if [ "$?" == "0" ] ; then
		kill $DBUS_DAEMON_PID
	    fi
    
	    DBUS_LAUNCH_PID=`cat $DBUS_PID_FILE`
	    #echo "Killing dbus launch pid $DBUS_LAUNCH_PID"
	    ps $DBUS_LAUNCH_PID > /dev/null
	    # Dbus is getting cleaned automatically, as it was started as session.
	    #if [ "$?" == "0" ] ; then
		#kill $DBUS_LAUNCH_PID
	    #fi
	    rm -Rf $DBUS_INFO_FILE
	    rm -Rf $DBUS_PID_FILE
	fi
	
        rm -Rf $XVFB_LOG_FILE
        rm -Rf $AUTH_FILE
        rm -Rf $XVFB_DISP_FILE		
        
        rm -Rf $LOG
}

#
# Find a X display to use
#
startXvfb() {
	
    attempts=0
    attempts_max=40

    rm -f $AUTH_FILE
    rm -f $XVFB_DISP_FILE
   
    #
    # This is required to give access to X sessions from the localhost only
    #
    echo localhost > $AUTH_FILE
    XVFB=`which Xvfb`

    #
    # Select a random display to start the Xvfb display on.
    # 
    while [ $attempts -le $attempts_max ]; do
        i=$[100 + $[ RANDOM % 100 ]] 
        if [ -S /tmp/.X11-unix/X$i -o -f /tmp/.X$i-lock ] ; then
            sleep 0.25
            let attempts++
        else
            XVFB_DISP=$i
			if [ ! -x $XVFB ]; then
				echo "ERROR: Xvfb is not available on the system"
				exit 1			
			fi
            #
            # Attempt to create the xvfb display
            #
	    $XVFB +extension RANDR -extension GLX :$XVFB_DISP -screen 0 1900x1025x16 -nolisten inet6 -fp unix/:7100,built-ins,/usr/share/vnc/fonts/ -auth $AUTH_FILE > $XVFB_LOG_FILE 2>&1 &
            #
            # Sleep 5 seconds, there can be a delay between the command being
            # fired off and the display becoming active due to system load
            # and other factors
            #
            sleep 5
            #
            # Test if the display used is already in use, if so try another.
            #
            grep 'Server is already active for display' $XVFB_LOG_FILE > /dev/null 2>&1
            if [ $? -eq 0 ] ; then
                let attempts++
                continue
            fi

            grep 'server already running' $XVFB_LOG_FILE > /dev/null 2>&1
            if [ $? -eq 0 ] ; then
                let attempts++
                continue
            fi
            #
            # Test if there was an error creating the display
            #
            grep 'Fatal server error' $XVFB_LOG_FILE > /dev/null 2>&1
            if [ $? -eq 0 ] ; then
                echo "Error starting Xvfb, please review $XVFB_LOG_FILE."
                echo "Exiting."
                exit 1
            fi
            break
        fi
        if [ $attempts -ge $attempts_max ] ; then
            echo "Error in setting up Xvfb, exhausted available display range and no valid displays were available."
            echo "Exiting."
            exit 1
        fi
    done
    
    echo $XVFB_DISP > $XVFB_DISP_FILE
	

    #
    # Wait to catch a SIGUSR2 so we can cleanup cleanly
    #
    [ -z "$XSCT_FAST_TRAP" ] && trap "{ cleanupXvfb; exit 1; }" SIGINT SIGTERM SIGUSR2

    wait
}

#
# Kill the active Xvfb display, display info is contained in the cwd
#
stopXvfb() {
	
	
    XVFBDISPLAY=`cat $XVFB_DISP_FILE`	
    XVFBPID=`ps -ef -u $USER | grep Xvfb | egrep ":$XVFBDISPLAY |:$XVFBDISPLAY$" | awk '{print $2}'`	
    if [ "x$XVFBPID" != x ] ; then
        #echo " killing the xvfb process  $XVFBPID"
        kill -9 $XVFBPID > $XVFB_LOG_FILE 2>&1                        
        # Clean up dbus-daemon and dbus-launch
	if [ "$DBUS_EXISTS" = "1" ]; then
	    DBUS_DAEMON_PID=`sed -e 's,.*DBUS_SESSION_BUS_PID=,,g' -e 's,\;.*,,g' $DBUS_INFO_FILE`
	    #echo "Killing dbus daemon pid $DBUS_DAEMON_PID"
	    kill $DBUS_DAEMON_PID
    
	    DBUS_LAUNCH_PID=`cat $DBUS_PID_FILE`
	    #echo "Killing dbus pid $DBUS_LAUNCH_PID"
	    kill $DBUS_LAUNCH_PID
	    rm -Rf $DBUS_INFO_FILE
	    rm -Rf $DBUS_PID_FILE
	fi	
        rm -Rf $XVFB_LOG_FILE
        rm -Rf $AUTH_FILE
        rm -Rf $XVFB_DISP_FILE		        
    fi
}



#
# Main
#
if [ ! -z "$1" -o "$1" = "start" -o "$1" = "stop" ] ; then	
    if [[ $RDI_PLATFORM = *lnx* ]] ; then		
		randomIndex=$2
		XVFB_DISP_FILE=/tmp/X.xvfb-display_$randomIndex.txt
		XVFB_LOG_FILE=/tmp/X.setup-log_$randomIndex.txt
		AUTH_FILE=/tmp/X.xvfb-auth_$randomIndex.txt
		DBUS_INFO_FILE=/tmp/dbus-info_$randomIndex.txt
		DBUS_PID_FILE=/tmp/dbus-launch-pid_$randomIndex.txt
		
		DBUS_LAUNCH=`which dbus-launch`
		if [ $? -ne 0 ]; then		    
		    DBUS_EXISTS=0
		else
		    DBUS_EXISTS=1
		fi
	
        if [ "$1" = start ] ; then
            startXvfb
        fi
        
        if [ "$1" = stop ] ; then
            stopXvfb
        fi
    else
        echo "Error: Xvfb is only supported on Linux"
        exit 1
    fi
else
    echo "Usage: $0 [start|stop]"
    echo "Start or stop an Xvfb session with a random display"
    exit 1
fi

exit 0
