#/usr/bin/python
import argparse
import xml.etree.ElementTree as ET

class AddNormalsSDF:
  def __init__(self, input_file, output_file) -> None:
    self.input_file = input_file
    self.output_file = output_file
    # Normals from Blender for each joint
    # Blender ---> SDF mapping
    # (x, y, z) -> (z, y, -x)
    self.normals = {
      'ThumbTMC_FE_joint': '0.049 0.665 -0.745',
      'ThumbTMC_AA_joint': '-0.496 0.731 -0.468',
      'ThumbMCP_joint': '0.092 0.204 -0.975',
      'ThumbIP_joint': '0.051 0.480 -0.876',
      'IndexCMC_joint': '0.973 0.124 -0.194',
      'IndexMCP_AA_joint': '-0.058 0.086 0.995',
      'IndexMCP_FE_joint': '0.999 0.046 0.022',
      'IndexPIP_joint': '0.992 0.104 -0.066',
      'IndexDIP_joint': '0.997 0.073 -0.026',
      'MiddleCMC_joint': '1.000 0.006 -0.003',
      'MiddleMCP_AA_joint': '0.059 0.056 0.997',
      'MiddleMCP_FE_joint': '0.997 -0.077 0.026',
      'MiddlePIP_joint': '0.998 0.064 0.026',
      'MiddleDIP_joint': '0.991 0.129 0.029',
      'RingCMC_joint': '0.984 -0.068 0.164',
      'RingMCP_AA_joint': '-0.386 0.034 0.922',
      'RingMCP_FE_joint': '0.966 -0.083 0.243',
      'RingPIP_joint': '0.981 -0.063 0.183',
      'RingDIP_joint': '0.973 -0.039 0.226',
      'LittleCMC_joint': '0.959 -0.172 0.224',
      'LittleMCP_AA_joint': '-0.620 0.021 0.784',
      'LittleMCP_FE_joint': '0.978 -0.064 0.198',
      'LittlePIP_joint': '0.894 -0.254 0.368',
      'LittleDIP_joint': '0.897 -0.171 0.408',
    }

  def set_normals_and_save(self):
    """
    Parse input file and change xyz tags of specified joints to corresponding
    normals.
    """
    self.sdf_tree = ET.parse(self.input_file)
    sdf = self.sdf_tree.getroot()
    print('Setting normals:')
    i = 0
    for joint in sdf.iter('joint'):
      joint_name = joint.get('name')
      if joint_name in self.normals.keys():
        axis = joint.find('axis')
        xyz = axis.find('xyz')
        xyz_text = self.normals[joint_name]
        xyz.text = xyz_text
        i += 1
        print(f'{i}: \t{joint_name} --> {xyz.text}')
    self.sdf_tree.write(
      self.output_file,
      encoding='utf8',
      xml_declaration=True,
      method='xml'
      )
    print(f'Output file: {self.output_file}')

if __name__ == '__main__':
  parser = argparse.ArgumentParser(
    description=
   'Configure and capture EMG signal from Shimmer3 device. '
   '\nFirst specify the serial port of the device you wish to connect to! '
   '\nExample usage:'
   '\n\tshimmer_capture_EMG.py /dev/rfcomm0',
   formatter_class=argparse.RawDescriptionHelpFormatter
  )
  parser.add_argument('input_file', help='model.sdf file location for conversion')
  parser.add_argument(
    '-o', '--output_file', help='name of output file with proper normals'
    )
  parser.set_defaults(output_file='model-normals.sdf')
  args = parser.parse_args()
  SetNormals = AddNormalsSDF(
    input_file=args.input_file, output_file=args.output_file
    )
  # Set normals and save file
  SetNormals.set_normals_and_save()