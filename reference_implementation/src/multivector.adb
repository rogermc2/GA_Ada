
package body Multivector is

   function Get_Basis_Vector (Index : Integer) return Multivector is
      BB : constant GA_Maths.Basis_Blade := New_Basis_Blade (Index);
      MV : Multivector;
   begin
      MV.Blades.Append (BB);
      return MV;
   end Get_Basis_Vector;

   --  -------------------------------------------------------------------------

end Multivector;
