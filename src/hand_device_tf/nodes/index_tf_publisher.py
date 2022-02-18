#!/usr/bin/env python
## Publish device coordinates related to index finger rehabilitation

from ntpath import join
import rospy
import traceback
import tf2_ros
import numpy as np
# from godirect_ros.msg import GripForce
from sensor_msgs.msg import JointState
from std_msgs.msg import Float64

def publish_index_tf():
  """
  Publish transformations between index finger device joints:
  - root is R_IndexCMC
  - U_IndexMCP
  - R_IndexPIP
  - R_IndexDIP

  Approximation plane is used to define linked frames.
  """

  ind_joints = ['IndexCMC_joint', 'IndexMCP_AA_joint', 'IndexMCP_FE_joint',
                       'IndexPIP_joint', 'IndexDIP_joint']
  ind_joint_lower = {
    'IndexMCP_AA_joint': -0.2617993877991494,
    'IndexMCP_FE_joint': -0.17453292519943295
  }
  ind_joint_upper = {
    'IndexMCP_AA_joint': 0.2617993877991494,
    'IndexMCP_FE_joint': 1.5707963267948966
  }
  
  # Step close to 1° (0.01745 radians)
  # angle_step = 0.01745
  angle_step = 0.01745
  mcp_aa_len = round((
    ind_joint_upper['IndexMCP_AA_joint'] - ind_joint_lower['IndexMCP_AA_joint']
    ) / angle_step)
  mcp_fe_len = round((
    ind_joint_upper['IndexMCP_FE_joint'] - ind_joint_lower['IndexMCP_FE_joint']
    ) / angle_step)
  
  # Creating a grid
  mcp_aa = np.linspace(
    ind_joint_lower['IndexMCP_AA_joint'], ind_joint_upper['IndexMCP_AA_joint'],
    mcp_aa_len
    )
  mcp_fe = np.linspace(
    ind_joint_lower['IndexMCP_FE_joint'], ind_joint_upper['IndexMCP_FE_joint'],
    mcp_fe_len
    )
  mcp_aa_grid, mcp_fe_grid = np.meshgrid(mcp_aa, mcp_fe)

# Messages queue size
  queue_size = 10

  # Publish joint
  
  # Lookup transform
  
  # Initialize node and Publisher
  rospy.init_node(
    'index_tf_publisher',
    anonymous=False,
    log_level=rospy.DEBUG,
    )
  try:
    # Initialize publishers with queue size
    # tf_pub = rospy.Publisher('index_tf', GripForce, queue_size=queue_size)
    joint_pub = rospy.Publisher('joint_states_experiment', JointState, queue_size=queue_size)
    ####################################
    # Acquire one message from joint_states topic
    joint_msg = rospy.wait_for_message('joint_states', JointState, timeout=None)
    rospy.logdebug(joint_msg)
    aa_ind = joint_msg.name.index('IndexMCP_AA_joint')
    fe_ind = joint_msg.name.index('IndexMCP_FE_joint')
    rospy.logdebug(f'AA index: {aa_ind}, FE index: {fe_ind}')
    rospy.logdebug(f'Position class: {type(joint_msg.position)}')
    positions = list(joint_msg.position)
    rospy.logdebug(f'positions: {positions}')
    rate = rospy.Rate(10)
    for aa in range(mcp_aa_len):
      for fe in range(mcp_fe_len):
        if not rospy.is_shutdown():
          joint_msg.header.seq += 1
          joint_msg.header.stamp = rospy.Time.now()
          positions[aa_ind], positions[fe_ind] = mcp_aa_grid[fe, aa], mcp_fe_grid[fe, aa]
          joint_msg.position = positions
          # Publish joint states + transforms 
          joint_pub.publish(joint_msg)
          # tf_pub.publish()
          rate.sleep()
  except rospy.ROSInterruptException:
    rospy.logwarn('User interrupted execution!')
  # except rospy.ROSException:
  #   rospy.logerr("Could not get parameter names!")
  except Exception as e:
    rospy.logerr(f'Caught Exception: {e}\nExiting!')
    rospy.logerr(traceback.format_exc())