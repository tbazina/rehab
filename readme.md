# UpperExtremity URDF Export from Blender
1. Rotate model (Euler coordinates of root link) -> y-axis points upoward
2. Export URDF
3. Indent XML in Gvim  :'<,'>!xmllint --format -
3. Manually edit URDF and rotate ground_joint by 1.570796327 (roll).

# RehabConcept
## Configuring robot model in Blender
- Euler coordinates axes not matching Blender axes
  - x Blender -> x Euler
  - -y Blender -> z Euler
  - z Blender -> y Euler


## URDF Export from Blender
1. Rotate model (Euler coordinates of root link) -> y-axis points upoward
2. Export URDF
3. Indent XML in Gvim  :'<,'>!xmllint --format -
3. Manually edit URDF and rotate BaseLink_joint by 1.570796327 (roll).
