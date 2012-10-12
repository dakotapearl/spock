with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with NetworkNode; use NetworkNode;
with DataDomain; use DataDomain;
with Ada.Containers.Ordered_Maps;
with Constants; use Constants;
with Commenting; use Commenting;

package NetworkManagement is

   procedure Initialise (InitialiseNetworks : access procedure);

   type SMSNetworkID is range 1 .. SMSNetworkID_Limit;
   type String_Pointer is access all String;
   type SMSNetwork is tagged record
      id : SMSNetworkID;
      Name : Unbounded_String;
      InputRegister : InputInterfaceArray_Pointer;
      OutputRegister : OutputInterfaceArray_Pointer;
      NodePool : NodeArray_Pointer;
   end record;
   type SMSNetwork_Pointer is access all SMSNetwork;

   function newSMSNetwork (NetworkName : String;
                           InputCount : in Natural; 
                           OutputCount : in Natural;
                           setupProcedure : access procedure (InputRegister : InputInterfaceArray_Pointer;
                                                              OutputRegister : OutputInterfaceArray_Pointer;
                                                              NodePool : NodeArray_Pointer) 
                           ) return SMSNetwork_Pointer;
                           
   function availableInputs(network : in SMSNetwork_Pointer) return Natural;
   function availableOutputs(network : in SMSNetwork_Pointer) return Natural;
   function NetworksAreCompatible (TransmittingNetwork : in SMSNetwork_Pointer; 
                                   ReceivingNetwork : in SMSNetwork_Pointer; 
                                   ConnectionQuantity : in Natural) return Boolean;
   procedure ConnectNetworks (TransmittingNetwork : in SMSNetwork_Pointer; 
                              ReceivingNetwork : in SMSNetwork_Pointer; 
                              ConnectionQuantity : in Natural);
   
   procedure SetNetworkAsEnvironment (Network : in SMSNetwork_Pointer;
                                      EnvironmentInputProcedure : in Environment_Input;
                                      NumberOfInputs : out Integer;
                                      EnvironmentOutputs : out OutputInterfaceArray_Pointer);
   
   procedure PrintNetworkSummary (Network : in SMSNetwork_Pointer);
   
private
   
   InputInterfaceIDIndex : InputInterfaceID := InputInterfaceID'First;
   OutputInterfaceIDIndex : OutputInterfaceID := OutputInterfaceID'First;
   SMSNetworkIDIndex : SMSNetworkID := SMSNetworkID'First;

end NetworkManagement;
