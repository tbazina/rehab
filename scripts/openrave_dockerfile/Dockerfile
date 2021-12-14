# Enable GUI applications inside a Docker container
# http://fabiorehm.com/blog/2014/09/11/running-gui-apps-with-docker/
FROM fixed-openrave:custom

# Replace 1000 with your user / group id
RUN export uid=1000 gid=1000 && \
    mkdir -p /home/developer && \
    echo "developer:x:${uid}:${gid}:Developer,,,:/home/developer:/bin/bash" >> /etc/passwd && \
    echo "developer:x:${uid}:" >> /etc/group && \
    echo "developer ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/developer && \
    chmod 0440 /etc/sudoers.d/developer && \
    mv /openrave /home/developer/openrave && \
    cp /ros_entrypoint.sh /home/developer/ros_entrypoint.sh && \
    cp -a /root/. /home/developer/ && \
    chown ${uid}:${gid} -R /home/developer

USER developer
ENV HOME /home/developer
