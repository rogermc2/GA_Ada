
with GL.Types;

with Glfw.Windows;

with Maths;

package Pick_Manager is
    type GL_Pick is private;
    type Orientation is private;
    type Orientation_Array is array (GL.Types.Int range <>) of Orientation;

    procedure Init_Pick_Manager;
    procedure Pick (Window              : in out Glfw.Windows.Window;
                    Positions         : GL.Types.Singles.Vector3_Array;
                    Orientations      : Orientation_Array;
                    Indices_Size      : GL.Types.Int;
                    View_Matrix, Projection_Matrix : GL.Types.Singles.Matrix4);
    function Pick_Active return Boolean;
private
    type Orientation is record
        Angle : Maths.Radian;
        Axis  : GL.Types.Singles.Vector3;
    end record;

    type GL_Pick is record
        Pick_Active      : Boolean := False;  --  set to true during picking
        --  set to picking window (x, y, w, h) during picking
        OpenGL_Pick      : GL.Types.Int_Array (1 .. 4) := (0, 0, 0, 0);
        --  Must be set correctly by caller of pick() to get correct distances returned
        Frustum_Near     : GL.Types.Single := 1.0;
        --  Must be set correctly by caller of pick() to get correct distances returned
        Frustum_Far      : GL.Types.Single := 100.0;
        --  not required for pick(), provided for completenes
        FrustumWidth     : GL.Types.Single := 0.0;
        --  not required for pick(), provided for completenes
        Frustum_Height   : GL.Types.Single := 0.0;
        Pick_Window_Size : GL.Types.Int := 4;
    end record;

end Pick_Manager;
