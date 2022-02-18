## ! DO NOT MANUALLY INVOKE THIS setup.py, USE CATKIN INSTEAD

from setuptools import setup
from catkin_pkg.python_setup import generate_distutils_setup

setup_args = generate_distutils_setup(
    packages=['hand_device_tf', 'hand_device_tf.nodes'],
    package_dir={'': 'src'}
)

setup(**setup_args)