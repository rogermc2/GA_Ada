
with GL.Types;

with GA_Maths;
with E2GA;
with E3GA;

package GL_Util is
   function Rotor_To_GL_Matrix (R : E3GA.Rotor) return  GL.Types.Singles.Matrix4;
   procedure Rotor_GL_Multiply (R : E3GA.Rotor; GL_Matrix : in out GL.Types.Singles.Matrix4);
   function To_GL (V3 : E3GA.Vector_3D) return GL.Types.Doubles.Vector3;
   function To_GL (V3 : E3GA.Vector_3D) return GL.Types.Singles.Vector3;
   function To_GL (V2 : E2GA.Vector_MV) return GL.Types.Singles.Vector3;
   procedure Viewport_Coordinates (Pt_World : GA_Maths.Array_3D;
                                   Model_View_Matrix,
                                   Projection_Matrix : GL.Types.Singles.Matrix4;
                                   Coords : out GL.Types.Singles.Vector2);

end GL_Util;
