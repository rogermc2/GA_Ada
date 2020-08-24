
with GL.Types; use GL.Types;

with GA_Maths;
with Multivectors;

package GL_Util is

   subtype Pick_Window is Singles.Vector4;

   function Frustum_Near return Single;
   function Frustum_Far return Single;
   function Frustum_Width return Single;
   function Frustum_Height return Single;
   procedure Load_Pick_Matrix;
   function OpenGL_Pick return Pick_Window;
   function Pick_Active return Boolean;
   function Pick_Window_Size return Int;
   procedure Pick_Matrix (Centre_X, Centre_Y : GL.Types.Size;
                          Width, Height      : GL.Types.Size);
   procedure Print_GL_Int3_Array
     (Name : String; anArray : GL.Types.Ints.Vector3_Array);
   function Rotor_To_GL_Matrix (R : Multivectors.Rotor)
                                 return GL.Types.Singles.Matrix4;
   procedure Rotor_GL_Multiply (R         : Multivectors.Rotor;
                                GL_Matrix : in out GL.Types.Singles.Matrix4);
   function To_GL (V3 : Multivectors.Multivector) return GL.Types.Doubles.Vector3;
   function To_GL (V3 : Multivectors.Multivector) return GL.Types.Singles.Vector3;
   function To_GL (V3 : GA_Maths.Float_3D) return GL.Types.Singles.Vector3;
   procedure Viewport_Coordinates (Pt_World          : GA_Maths.Float_3D;
                                   Model_View_Matrix,
                                   Projection_Matrix : GL.Types.Singles.Matrix4;
                                   Coords            : out GL.Types.Singles.Vector2);
end GL_Util;
