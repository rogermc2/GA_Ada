
with GL.Types; use GL.Types;

with GA_Maths;
with E2GA;
with E3GA;
with Multivector;

package GL_Util is
   type Pick_Window is array (1 .. 4) of Single;
   type GL_Pick is record
      Pick_Active      : Boolean := False;  --  set to true during picking
      --  set to picking window (x, y, w, h) during picking
      OpenGL_Pick      : Pick_Window := (others => 0.0);
      --  Must be set correctly by caller of pick() to get correct distances returned
      Frustum_Near     : Single := 1.0;
      --  Must be set correctly by caller of pick() to get correct distances returned
      Frustum_Far      : Single := 100.0;
      --  not required for pick(), provided for completenes
      FrustumWidth     : Single := 0.0;
      --  not required for pick(), provided for completenes
      Frustum_Height   : Single := 0.0;
      Pick_Window_Size : Int := 4;
   end record;

   procedure GL_Color_3fm (R, G, B : GL.Types.Single);
   procedure Load_Pick_Matrix;
   function Rotor_To_GL_Matrix (R : Multivector.Rotor) return  GL.Types.Singles.Matrix4;
   procedure Rotor_GL_Multiply (R : Multivector.Rotor; GL_Matrix : in out GL.Types.Singles.Matrix4);
   function To_GL (V3 : Multivector.Vector) return GL.Types.Doubles.Vector3;
   function To_GL (V3 : Multivector.Vector) return GL.Types.Singles.Vector3;
--     function To_GL (V2 : Multivector.Vector) return GL.Types.Singles.Vector3;
   procedure Viewport_Coordinates (Pt_World : GA_Maths.Array_3D;
                                   Model_View_Matrix,
                                   Projection_Matrix : GL.Types.Singles.Matrix4;
                                   Coords : out GL.Types.Singles.Vector2);
end GL_Util;
