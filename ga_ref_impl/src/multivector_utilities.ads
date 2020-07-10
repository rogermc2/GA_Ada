
with Multivectors;

package Multivector_Utilities is

    MV_Utilities_Exception : Exception;

    function Factorize_Multivector (MV_B : Multivectors.Multivector;
                                    Scale : out float)
                                    return Multivectors.Multivector_List;
   function Factorize_Multivector_Fast (MV_B  : Multivectors.Multivector;
                                        Scale : out Float)
                                        return Multivectors.Multivector_List;

end Multivector_Utilities;
