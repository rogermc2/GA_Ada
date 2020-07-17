
with GL.Types; use GL.Types;

with GA_Maths;
with Multivectors;

package GL_Util is
    subtype Pick_Window is Singles.Vector4;

    type GL_Pick is private;

--      function From_GL (V3 : GL.Types.Singles.Vector3) return C3GA.Vector_E3;
    --     procedure GL_Color_3fm (R, G, B : GL.Types.Single);
    procedure Load_Pick_Matrix;
    procedure Pick_Matrix (Centre_X, Centre_Y : GL.Types.Size;
                           Width, Height : GL.Types.Size);
    procedure Print_GL_Int3_Array
      (Name : String; anArray : GL.Types.Ints.Vector3_Array);
    function Rotor_To_GL_Matrix (R : Multivectors.Rotor)
                                 return GL.Types.Singles.Matrix4;
    procedure Rotor_GL_Multiply (R : Multivectors.Rotor;
                                GL_Matrix : in out GL.Types.Singles.Matrix4);
    function To_GL (V3 : Multivectors.Multivector) return GL.Types.Doubles.Vector3;
    function To_GL (V3 : Multivectors.Multivector) return GL.Types.Singles.Vector3;
    function To_GL (V3 : GA_Maths.Float_3D) return GL.Types.Singles.Vector3;
    procedure Viewport_Coordinates (Pt_World : GA_Maths.Float_3D;
                                    Model_View_Matrix,
                                    Projection_Matrix : GL.Types.Singles.Matrix4;
                                    Coords : out GL.Types.Singles.Vector2);

private
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
end GL_Util;
