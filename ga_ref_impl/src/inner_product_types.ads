
with Multivectors;

package Inner_Product_Types is

   function Factorize_Blade (MV_B  : Multivectors.Multivector;
                             Scale : out Float)
                             return Multivectors.Multivector_List;
   function Factorize_Blade_Fast (MV_B : Multivectors.Multivector; Scale : out Float)
                                  return Multivectors.Multivector_List;

    Inner_Product_Types_Exception : Exception;

end Inner_Product_Types;
