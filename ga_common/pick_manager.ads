
with GL.Types;

with Glfw.Windows;

with Maths;

package Pick_Manager is
    type Orientation is private;
    type Orientation_Array is array (GL.Types.Int range <>) of Orientation;

    procedure Init_Pick_Manager;
    procedure Pick (Window              : in out Glfw.Windows.Window;
                    Positions         : GL.Types.Singles.Vector3_Array;
                    Orientations      : Orientation_Array;
                    Indices_Size      : GL.Types.Int;
                    View_Matrix       : GL.Types.Singles.Matrix4;
                    Projection_Matrix : GL.Types.Singles.Matrix4);
    function Pick_Active return Boolean;
private
    type Orientation is record
        Angle : Maths.Radian;
        Axis  : GL.Types.Singles.Vector3;
    end record;

end Pick_Manager;
