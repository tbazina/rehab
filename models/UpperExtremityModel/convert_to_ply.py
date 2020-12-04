#!/usr/bin/env python

import vtk
import os

def get_folders():
    import argparse
    description = 'Convert .vtp, .stl, .vtk, .g ---> .ply file.'
    epilogue = '''
   '''
    parser = argparse.ArgumentParser(description=description, epilog=epilogue)
    parser.add_argument('-i', dest='input', help='Input folder with 3D files',
                        nargs='?', required=True, type=str)
    parser.add_argument('-o', dest='output',
                        help='Output folder to store .obj files (MUST EXIST)',
                        nargs='?', required=True, type=str)
    args = parser.parse_args()
    return args

def ReadPolyData(file_name):
    path, extension = os.path.splitext(file_name)
    extension = extension.lower()
    if extension == ".ply":
        reader = vtk.vtkPLYReader()
        reader.SetFileName(file_name)
        reader.Update()
        #  poly_data = reader.GetOutput()
    elif extension == ".vtp":
        reader = vtk.vtkXMLPolyDataReader()
        reader.SetFileName(file_name)
        reader.Update()
        #  poly_data = reader.GetOutput()
    elif extension == ".stl":
        reader = vtk.vtkSTLReader()
        reader.SetFileName(file_name)
        reader.Update()
        #  poly_data = reader.GetOutput()
    elif extension == ".vtk":
        reader = vtk.vtkpoly_dataReader()
        reader.SetFileName(file_name)
        reader.Update()
        #  poly_data = reader.GetOutput()
    elif extension == ".g":
        reader = vtk.vtkBYUReader()
        reader.SetGeometryFileName(file_name)
        reader.Update()
        #  poly_data = reader.GetOutput()
    else:
        # Return a None if the extension is unknown.
        poly_data = None
    print 'Read file: {}'.format(file_name)
    return reader

def WritePlyFile(input_poly_data, file_entry, out_folder):
    # Write the ply file to disk
    plyWriter = vtk.vtkPLYWriter()
    filename, extension = os.path.splitext(file_entry)
    filename = ''.join([filename, '.ply'])
    plyWriter.SetFileName(os.path.join(out_folder, filename))
    plyWriter.SetInputConnection(input_poly_data.GetOutputPort())
    plyWriter.Write()
    print 'Written file: {}'.format(os.path.join(out_folder, filename))


def main():
    # Get output folder name -> folders.output and folders.input
    folders = get_folders()

    # Files to convert from input folder
    print "Output folder: {}".format(folders.output)
    print "Input folder: {}".format(folders.input)
    for entry in os.listdir(folders.input):
        if os.path.isfile(os.path.join(folders.input, entry)):
            _, extension = os.path.splitext(entry)
            extension = extension.lower()
            if extension in ['.vtp', '.stl', '.vtk', '.g']:
                print "Converting file: {}".format(entry)
                in_file = ReadPolyData(os.path.join(folders.input, entry))
                WritePlyFile(in_file, entry, folders.output)
                print '___________________________________'


if __name__ == '__main__':
    main()
