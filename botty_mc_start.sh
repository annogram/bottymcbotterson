#!/bin/bash

# Sample launcher script
# Can be added as a generic launcher to be executed by the deployment scripts
# Should be added to the distribution zip file and referenced in appspec.yml

APPHOME=/home/ubuntu/botty_mc_botterson
cd $APPHOME

BASE=$APPHOME
PID=$BASE/my_app.pid
LOG=$BASE/my_app.log

CMD="$BASE/discord-bot-exe" # Point to your program run command
COMMAND="$CMD"

USR=`whoami`

# Gets the current status of the application
status() {

    if [ -f $PID ]
    then
        PIDN=`cat $PID`
        WC=`ps aux | grep $PIDN | grep -v grep | wc -c`
        if [ $WC -gt 0 ]
        then
            # Application is running
            echo "Status: Running: [$( cat $PID )]"
            ps -ef | grep -v grep | grep $( cat $PID )
        else
            # Has a PID file but is not running
            echo "Status: Stopped - Deleting Dangling PID file"
            rm $PID
        fi
    else
        echo "Status: Stopped"
    fi
}

is_running() {
    WC=0
    if [ -f $PID ]
    then
        PIDN=`cat $PID`
        WC=`ps aux | grep $PIDN | grep -v grep | wc -c`
    fi

    if [ $WC -gt 0 ]
    then
        echo 1
    else
        if [ -f $PID ]
        then
            rm $PID
        fi

        echo 0
    fi
}

start() {
    if [ -f $PID ]
    then
        echo
        echo "Already running. [$( cat $PID )]"
    else
        echo "Starting... "
        echo "Starting in $( pwd )... " >> $LOG
        touch $PID
        if nohup $COMMAND >>$LOG 2>&1 & # if nohup $COMMAND >>$LOG 2>&1 &
        then
            echo $! >$PID
            echo "Done."
            echo "-----------------------------------------------------" >>$LOG
            echo "$(date '+%Y-%m-%d %X'): Starting Application [$( cat $PID )]" >>$LOG
            echo "-----------------------------------------------------" >>$LOG
        else
            echo "Error starting... "
            rm $PID
        fi
    fi
}

kill_cmd() {
    SIGNAL=""; MSG="Killing "
    while true
    do
        LIST=`ps -ef | grep -v grep | grep $CMD | grep -w $USR | awk '{print $2}'`
        if [ "$LIST" ]
        then
            echo; echo "$MSG $LIST" ; echo
            echo $LIST | xargs kill $SIGNAL
            sleep 2
            SIGNAL="-9" ; MSG="Killing $SIGNAL"
            if [ -f $PID ]
            then
                rm $PID
            fi
        else
           echo; echo "All killed..." ; echo
           break
        fi
    done
}

stop() {
    echo "Stopping... "

    if [ -f $PID ]
    then
        if kill $( cat $PID )
        then echo "Done."
             echo "$(date '+%Y-%m-%d %X'): STOP" >>$LOG
        fi
        rm $PID
        kill_cmd
    else
        echo "No pid file. Already stopped?"
    fi
}

case "$1" in
    'start')
            start
            ;;
    'stop')
            stop
            ;;
    'restart')
            stop ; echo "Restarting..."; sleep 1 ;
            start
            ;;
    'status')
            status
            ;;
    'is_running')
            is_running
            ;;
    *)
            echo
            echo "Usage: $0 { start | stop | restart | status | is_running }"
            echo
            exit 1
            ;;
esac

exit 0
