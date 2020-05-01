
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GL.Objects.Programs;
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

   GLUT_Read_Exception : Exception;

   procedure Get_GLUT_Model_2D (Render_Program : GL.Objects.Programs.Program;
                                Model_Name : Ada.Strings.Unbounded.Unbounded_String;
                                Model_Rotor : Multivectors.Rotor);
   procedure Solid_Cube (Size : Float);
   procedure Solid_Cone (Base, Height : Float; Slices, Stacks : Integer);
   procedure Solid_Dodecahedron;
   procedure Solid_Icosahedron;
   procedure Solid_Octahedron;
   procedure Solid_Sphere (Radius : Float; Slices, Stacks : Integer);
   procedure Solid_Teapot (Size : Float);
   procedure Solid_Tetrahedron;
   procedure Solid_Torus (Inner_Radius, Outer_Radius : Float;
                          Sides, Rings : Integer);

end Graphic_Data;
