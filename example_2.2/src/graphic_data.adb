
package body Graphic_Data is

   procedure GLUT_Solid_Teapot (Size : GL.Types.Double);
   pragma Import (StdCall, GLUT_Solid_Teapot, "glutSolidTeapot");

   procedure Solid_Teapot (Size : GL.Types.Single) is
   begin
      GLUT_Solid_Teapot (GL.Types.Double (Size));
   end Solid_Teapot;

end Graphic_Data;
