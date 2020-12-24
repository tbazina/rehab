# UpperExtremity URDF Export from Blender
1. Rotate model (Euler coordinates of root link) -> y-axis points upoward
2. Export URDF
3. Indent XML in Gvim  :'<,'>!xmllint --format -
3. Manually edit URDF and rotate ground_joint by 1.570796327 (roll).

# RehabConcept
## Configuring robot model in Blender
- Steps to assign new segment to visual:
  1. Select visual geometry
  2. Move cursor to selected
  3. IF present, remove Child Of object constraint.
  3. Select child visual than shift + select parent visual and than
  shift + ctrl + P
  4. Make parent without inverse
  5. Select child visual, shift + S and move to cursor (offset)
  6. Add child segment and copy paste Transform coordinates to Euler (in
     meters)
  7. Euler coordinates axes not matching Blender axes
    - x Blender -> x Euler
    - -y Blender -> z Euler
    - z Blender -> y Euler
  8. After setting coordinates press Z to wireframe and check if segment is
     located precisely inside cursor.

## URDF Export from Blender
1. Rotate model (Euler coordinates of root link) -> y-axis points upoward
2. Export URDF
3. Indent XML in Gvim  :'<,'>!xmllint --format -
3. Manually edit URDF and rotate BaseLink_joint by 1.570796327 (roll).
