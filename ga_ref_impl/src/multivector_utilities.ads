
with Multivectors;

package Multivector_Utilities is

    MV_Utilities_Exception : Exception;

    function Factorize_Blades (MV_B : Multivectors.Multivector; Scale : out Float)
                              return Multivectors.Multivector_List;
    function Factorize_Blade_Fast (MV_B  : Multivectors.Multivector;
                                   Scale : out Float)
                                   return Multivectors.Multivector_List;
   function Reflect (MV : Multivectors.Multivector;
                     DP: Multivectors.Dual_Plane)
                     return Multivectors.Multivector;
   function Rotate (MV : Multivectors.Multivector;
                    aVersor: Multivectors.TR_Versor)
                     return Multivectors.Multivector;
end Multivector_Utilities;
