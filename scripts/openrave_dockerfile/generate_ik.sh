#/usr/bin/sh
#python `openrave-config --python-dir`/openravepy/_openravepy_/ikfast.py --robot=robot_wrapper.xml --savefile=ik_ForearmRehabSimplifiedRounded.cpp --freeindex=5 --iktype=transform6d
#openrave.py --database inversekinematics --robot=robot_wrapper.xml --iktype=transform6d --iktests=1000 --freejoint=ElasticHingePart2_joint --freeinc=0.01
#openrave.py --database inversekinematics --robot=robot_wrapper.xml --iktype=transform6d --manipname=ForearmRehabIKFast
#openrave.py --database inversekinematics --robot=robot_wrapper.xml --iktype=transform6d --manipname=ForearmRehabIKFast --usecached --iktests=100000
#openrave.py --database inversekinematics --robot=robot_wrapper.xml --manipname=ForearmRehabIKFast --show
openrave.py --database inversekinematics --robot=robot_wrapper.xml --manipname=ForearmRehabIKFast --getfilename
#openrave.py --database kinematicreachability --robot=robot_wrapper.xml --gethas --getfilename --manipname=ForearmRehabSimplified
#openrave.py --database kinematicreachability --robot=robot_wrapper.xml --manipname=ForearmRehabIKFast --maxradius=0.60 --xyzdelta=0.015 --quatdelta=0.2
#openrave.py --database kinematicreachability --robot=robot_wrapper.xml --manipname=ForearmRehabIKFast --maxradius=0.60 --xyzdelta=0.1 --quatdelta=0.5 --usefreespace
#openrave.py --database kinematicreachability --robot=robot_wrapper.xml --manipname=ForearmRehabIKFast --show --showscale=60
