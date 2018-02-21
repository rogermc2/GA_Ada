
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GL.Types;

with Multivectors;

package Graphic_Data is

   Num_Names   : Integer := 9;
   Model_Names : array (1 .. Num_Names) of Unbounded_String :=
     (To_Unbounded_String ("teapot"),
      To_Unbounded_String ("cube"),
      To_Unbounded_String ("sphere"),
      To_Unbounded_String ("cone"),
      To_Unbounded_String ("torus"),
      To_Unbounded_String ("dodecahedron"),
      To_Unbounded_String ("octahedron"),
      To_Unbounded_String ("tetrahedron"),
      To_Unbounded_String ("icosahedron"));

   procedure Get_GLUT_Model_2D (Model_Name : Ada.Strings.Unbounded.Unbounded_String;
                                Model_Rotor : Multivectors.Rotor);
   procedure Solid_Cube (Size : Float);
   procedure Solid_Sphere (Radius : Float; Slices, Stacks : Integer);
   procedure Solid_Teapot (Size : Float);

end Graphic_Data;
