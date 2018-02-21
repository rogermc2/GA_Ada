
with GL.Types;

package GLUT_API is

   procedure GLUT_Cube (Size : GL.Types.Double);
   pragma Import (StdCall, GLUT_Cube, "glutCubeTeapot");

   procedure GLUT_Solid_Teapot (Size : GL.Types.Double);
   pragma Import (StdCall, GLUT_Solid_Teapot, "glutSolidTeapot");

end GLUT_API;
