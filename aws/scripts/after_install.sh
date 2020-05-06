#!/bin/bash
HOME=/home/ec2-user
LOG=$HOME/deploy.log

/bin/echo "$(date '+%Y-%m-%d'): ** After Install Hook Started **" >> $LOG
/bin/echo "$(date '+%Y-%m-%d'): Changing owner and group of application... " >> $LOG

# verify that the application directory has the correct owner/group
/usr/bin/sudo /bin/chown -R ec2-user:ec2-user /home/ec2-user/botty_mc_botterson

echo -e "Done" >> $LOG

/bin/echo "$(date '+%Y-%m-%d %X'): ** After Install Hook Completed **" >> $LOG