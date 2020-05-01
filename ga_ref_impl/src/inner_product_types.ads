
with Multivectors;

package Inner_Product_Types is

   function Factorize_Multivector (MV  : Multivectors.Multivector;
                                   Scale : out Float)
                                   return Multivectors.Multivector;
   function Factorize_Blades (MV_B  : Multivectors.Multivector;
                              Scale : out Float)
                              return Multivectors.Multivector_List;
   function Factorize_Blade_Fast (MV_B : Multivectors.Multivector; Scale : out Float)
                                  return Multivectors.Multivector_List;

    Inner_Product_Types_Exception : Exception;

end Inner_Product_Types;
