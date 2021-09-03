#!/usr/bin/env python

from __future__ import print_function
import kdl_parser_py.urdf
import rospkg
import PyKDL

rospack = rospkg.RosPack()
urdf_path = '/'.join([rospack.get_path('rehab'), 'urdf/RehabConcept/rehab_concept.urdf'])
print(urdf_path)
kdl_tree = kdl_parser_py.urdf.treeFromFile(urdf_path)[1]
print(kdl_tree)
chain = kdl_tree.getChain('BaseLink_joint', 'Act1_joint')
print(chain)
print(chain.getNrOfJoints())