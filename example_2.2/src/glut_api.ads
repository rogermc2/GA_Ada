
with GL.Types; use  GL.Types;

package GLUT_API is

   procedure GLUT_Solid_Cube (Size : Double);
   pragma Import (StdCall, GLUT_Solid_Cube, "glutSolidCube");

   procedure GLUT_Solid_Cone (Base, Height : Double; Slices, Stacks : Int);
   pragma Import (StdCall, GLUT_Solid_Cone, "glutSolidCone");

   procedure GLUT_Solid_Dodecahedron;
   pragma Import (StdCall, GLUT_Solid_Dodecahedron, "glutSolidDodecahedron");

   procedure GLUT_Solid_Icosahedron;
   pragma Import (StdCall, GLUT_Solid_Icosahedron, "glutSolidIcosahedron");

   procedure GLUT_Solid_Octahedron;
   pragma Import (StdCall, GLUT_Solid_Octahedron, "glutSolidOctahedron");

   procedure GLUT_Solid_Sphere (Radius : Double; Slices, Stacks : Int);
   pragma Import (StdCall, GLUT_Solid_Sphere, "glutSolidSphere");

   procedure GLUT_Solid_Teapot (Size : Double);
   pragma Import (StdCall, GLUT_Solid_Teapot, "glutSolidTeapot");

   procedure GLUT_Solid_Tetrahedron;
   pragma Import (StdCall, GLUT_Solid_Tetrahedron, "glutSolidTetrahedron");

   procedure GLUT_Solid_Torus (Inner_Radius, Outer_Radius : Double;
                               Sides, Rings : Int);
   pragma Import (StdCall, GLUT_Solid_Torus, "glutSolidTorus");

end GLUT_API;
