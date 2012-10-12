with ENV_Test;
with NetworkManagement; use NetworkManagement;
with NetworkNode; use NetworkNode;

package EXP_Test is

   TestNetwork : SMSNetwork_Pointer;
   Env : SMSNetwork_Pointer;
   
   procedure SingleSeedNode (InputRegister : in InputInterfaceArray_Pointer;
                             OutputRegister : in OutputInterfaceArray_Pointer;
                             NodePool : in NodeArray_Pointer);
   procedure LinkToEnvPackage (InputRegister : in InputInterfaceArray_Pointer;
                               OutputRegister : in OutputInterfaceArray_Pointer;
                               NodePool : in NodeArray_Pointer);
   procedure setupGeneticCode;
                               
   procedure setup;
   procedure start;




   outputTask : ENV_Test.generateOutput_Pointer;
   
   
   
end EXP_Test;
